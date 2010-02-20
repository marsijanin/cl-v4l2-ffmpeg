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
     (do-frames (,frame-var ,v4l2-var
			    :end-test-form ,end-test-form
			    :return-form ,return-form)
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

(defstruct ffmpeg-fifo 
  stream
  (in "-")
  (out "-")
  (input-size "640x480")
  output-size 
  (input-pix-fmt "rgb24")
  (input-format "rawvideo")
  (output-format "mpeg")
  input-frame-rate output-frame-rate)

(defun ffmpeg-args (ff)
  (with-slots (in input-format input-size input-pix-fmt input-frame-rate 
		  output-format output-size output-frame-rate out) ff
    (append (list "-f" input-format)
	    (list "-s" input-size)
	    (list "-pix_fmt" input-pix-fmt)
	    (when input-frame-rate
	      (list "-r" input-frame-rate))
	    (list "-i" in)
	    (when output-format
	      (list "-f" output-format))
	    (when output-size
	      (list "-s" output-size))
	    (when output-frame-rate
	      (list "-r" output-frame-rate))
	    (list out))))

(defun argv-to-foreign (argv)
  (loop
     with ln = (+ (length argv) 1)
     with foreign-argv = (cffi:foreign-alloc :pointer :count ln)
     for str in argv
     as i = 0 then (+ 1 i)
     do (setf (cffi:mem-aref foreign-argv :pointer i) (cffi:foreign-string-alloc str))
     finally (progn
	       (setf (cffi:mem-aref foreign-argv :pointer (- ln 1)) (cffi:null-pointer))
	       (return foreign-argv))))

(defun free-foreign-argv (ptr sz)
  (loop 
     for i below sz 
     do (cffi:foreign-free (cffi:mem-aref ptr :pointer i))
     finally (cffi:foreign-free ptr)))

(defmacro with-foreign-argv ((argv args) &body body)
  `(let ((,argv (argv-to-foreign ,args)))
     (unwind-protect (progn ,@body)
       (free-foreign-argv ,argv (+ (length ,args) 1)))))


(defstruct process-pipe 
  pid alivep input-fd output-fd error-fd)

(defun run-process-pipe (cmd args)
  (flet ((bindfd (fd child parent)
	   (isys:%sys-close parent)
	   (isys:%sys-dup2 child fd)
	   (isys:%sys-close child)))
    (multiple-value-bind (child-in parent-out)
	(isys:%sys-pipe)
      (multiple-value-bind (parent-in child-out)
	  (isys:%sys-pipe)
	(multiple-value-bind (parent-err child-err)
	    (isys:%sys-pipe)
	(with-foreign-argv (argv args)
	  (let ((pid (isys:%sys-fork)))
	    (if (zerop pid)
		(progn
		  (bindfd 0 child-in parent-out)
		  (bindfd 1 child-out parent-in)
		  (bindfd 3 child-err parent-err)
		  (isys:%sys-execv cmd argv)
		  #+sbcl(sb-ext:quit :unix-status 1))
		(progn
		  (isys:%sys-close child-in)
		  (isys:%sys-close child-out)
		  (isys:%sys-close child-err)
		  (make-process-pipe :pid pid
				     :alivep t
				     :input-fd parent-in
				     :output-fd parent-out
				     :error-fd parent-err))))))))))

(defun run-ffmpeg-pipe (&key (ffmpeg "/usr/bin/ffmpeg")
			(in "-") (out "-")
			(input-size "640x480")
			output-size 
			(input-pix-fmt "rgb24")
			(input-format "rawvideo")
			(output-format "mpeg")
			input-frame-rate output-frame-rate)
  (let* ((ff (make-ffmpeg-fifo :in in
			  :out out
			  :input-size input-size
			  :input-pix-fmt input-pix-fmt
			  :input-format input-format
			  :output-format output-format
			  :output-size output-size
			  :input-frame-rate input-frame-rate
			  :output-frame-rate output-frame-rate))
	 (args (append (list ffmpeg) (ffmpeg-args ff))))
    (format t "Runing  \"~{~a ~}\"" args)
    (run-process-pipe ffmpeg args)))


