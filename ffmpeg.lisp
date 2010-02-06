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
       (close-v4l2 ,v4l2-var))))

(defun write-frames-to-ffmpeg-fifo-w/o-threads (v4l2 fifo-fd)
  (loop
     with capture-fd = (v4l2-fd v4l2)
     with sz = (slot-value (v4l2:format-pix (v4l2:get-image-format capture-fd)) 'v4l2:sizeimage)
     with capture-buffers = (v4l2-buffers v4l2)
     for frame    = (w/o-errors (v4l2:get-frame capture-fd))
     as  buff-ptr = (when frame (second (nth frame capture-buffers)))
     when buff-ptr do (progn (isys:%sys-write fifo-fd buff-ptr sz)
			     (v4l2:put-frame capture-fd frame))))

