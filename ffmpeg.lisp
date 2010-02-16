(defmacro w/o-errors (&body body)
  (let ((condition (gensym "condition")))
  `(handler-case (progn ,@body)
     (error (,condition) (format t "~A~%" ,condition)))))


(defstruct v4l2
  fd buffers format openp)

(defun open-v4l2 (path &key (w 640) (h 480) (pixformat v4l2:pix-fmt-rgb24) (n-buffs 4))
  (let* ((fd    (isys:%sys-open path isys:o-rdwr))
	 (fmt   (v4l2:set-image-format fd w h pixformat))
	 (buffs (v4l2:map-buffers fd n-buffs)))
    (v4l2:stream-on fd buffs)
    (make-v4l2 :fd fd :buffers buffs :format fmt :openp t)))

(defun close-v4l2 (v4l2)
  (v4l2:stream-off    (v4l2-fd v4l2))
  (v4l2:unmap-buffers (v4l2-buffers v4l2))
  (isys:%sys-close    (v4l2-fd v4l2))
  (setf (v4l2-openp v4l2) nil)
  v4l2)

(defmacro with-v4l2 ((v4l2-var path &key
			       (w 640) (h 480) (pixformat v4l2:pix-fmt-rgb24) (n-buffs 4))
		     &body body)
  `(let ((,v4l2-var (open-v4l2 ,path :w ,w :h ,h :pixformat ,pixformat :n-buffs ,n-buffs)))
     (unwind-protect
	  (progn ,@body)
       (when (v4l2-openp ,v4l2-var)
	 (close-v4l2 ,v4l2-var)))))

(defmacro do-frames ((frame-var v4l2 &key end-test-form return-form) &body body)
  (let ((fd (gensym "fd")))
    `(do* ((,fd (v4l2-fd ,v4l2))
	   (,frame-var (w/o-errors (v4l2:get-frame ,fd))
		       (w/o-errors (v4l2:get-frame ,fd))))
	  (,end-test-form ,return-form)
       (when ,frame-var
	 ,@body
	 (v4l2:put-frame ,fd ,frame-var)))))

(defmacro with-v4l2-do-frames ((v4l2-var frame-var path &key
			       (w 640) (h 480) (pixformat v4l2:pix-fmt-rgb24) (n-buffs 4)
			       end-test-form return-form)
			       &body body)
  `(with-v4l2 (,v4l2-var ,path :w ,w :h ,h :pixformat ,pixformat :n-buffs ,n-buffs)
     (do-frames (,frame-var ,v4l2-var ,end-test-form ,return-form)
       ,@body)))

(defun write-frames-to-ffmpeg-fifo-w/o-threads (v4l2 fifo-fd)
  (let ((buffs (v4l2-buffers v4l2))
	(size (slot-value (v4l2:format-pix (v4l2:get-image-format (v4l2-fd v4l2)))
			  'v4l2:sizeimage)))
    (do-frames (frame v4l2)
      (isys:%sys-write fifo-fd (second (nth frame buffs)) size))))

(defun split-video-stream-into-fifos-w/o-threads (v4l2 fifos-and-cmds)
  (loop
     with capture-fd = (v4l2-fd v4l2)
     with frame-size = (slot-value (v4l2:format-pix 
				   (v4l2:get-image-format capture-fd))
				  'v4l2:sizeimage)
     with capture-buffers = (v4l2-buffers v4l2)
     do (loop for (fifo-fd cmd) in fifos-and-cmds
	   for frame    = (w/o-errors (v4l2:get-frame capture-fd))
	   as  buff-ptr = (when frame (second (nth frame capture-buffers)))
	   when buff-ptr do (progn (isys:%sys-write fifo-fd buff-ptr frame-size)
				   (v4l2:put-frame capture-fd frame)))))

(defstruct ffmpeg-fifoo 
  fd
  in
  out
  openp
  (input-size "640x480")
  output-size
  (input-pix-fmt "rgb24")
  (input-format "rawvideo")
  (output-format "mpeg"))

(defun format-ffmepg (ff &optional (ffmpeg "/usr/bin/ffmpeg"))
  #|(assert (ffmpeg-fifoo-openp ff) () "Can't send data to the closed fifo")|#
  (with-slots (in out input-format output-format input-pix-fmt input-size output-size) ff
    (format nil "~A -f ~A -s ~A -pix_fmt ~A -i ~A -f ~A ~A"
	    ffmpeg input-format input-size input-pix-fmt in output-format out)))

(isys:defsyscall (%sys-system "system") :int
  (command :string))

(defun open-ffmeg-fifo (in out &key (input-size "640x480") output-size
			(input-pix-fmt "rgb24") (input-format "rawvideo")
			(output-format "mpeg"))
  (let ((ff (make-ffmpeg-fifoo :in in
			       :out out
			       :input-size input-size
			       :output-size output-size
			       :input-pix-fmt input-pix-fmt
			       :input-format input-format
			       :output-format output-format)))
    (multiple-value-bind (in-path in-type) (iolib.os:file-exists-p in)
      (cond
	((and in-path (not (eql in-type :pipe)))
	 (error "Input pathname ~a exist and it is not an fifo" in-path))
	((null in-path) (isys:%sys-mkfifo in #o660))))
    (setf (ffmpeg-fifoo-fd ff) (isys:%sys-open in isys:o-rdonly)
	  (ffmpeg-fifoo-openp ff) t)
    ff))
