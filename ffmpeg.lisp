;;; some useful macros for working with cl-v4l2 and ffmpeg
;;; (cl-v4l2 example application)
;;; Copyright 2010 Nikolay V. Razbegaev <marsijanin@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cl-ffmpeg)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <v4l2 syntax shugar>
(defmacro w/o-errors (&body body)
  "Like `ignore-errors`, but also print ignored condition"
  (let ((condition (gensym "condition")))
    `(handler-case (progn ,@body)
       (error (,condition) (format t "~A~%" ,condition)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct v4l2
  fd buffers format w h size path stram-on-p)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun open-v4l2 (path &key (w 640) (h 480) (pixformat v4l2:pix-fmt-rgb24)
		  (n-buffs 4))
  "Trying to open v4l2 device by `path` and setup it for streaming
   action with following settings:
   - `w`: frame width;
   - `h`: frame height;
   - `pixformat`: one of v4l2:pix-fmt-* constants;
   - `n-buffs`: number of the mapped v4l2 buffers.
   Return `v4l2` instance if success."
  (let ((fd (isys:%sys-open path isys:o-rdwr)))
    ;; handler-case used in order to close v4l2 device
    ;; if there will be some errors during v4l2 setup
    ;; (try to set up unsupported format etc.)
    (handler-case
	(progn
	  (v4l2:set-image-format fd w h pixformat)
	  (let ((buffs (v4l2:map-buffers fd n-buffs)))
	    (v4l2:stream-on fd buffs)
	    (with-slots ((w v4l2:width) (h v4l2:height) (size v4l2:sizeimage)
			 (fmt v4l2:pixelformat))
		(v4l2:format-pix (v4l2:get-image-format fd))
	      (make-v4l2 :fd fd :buffers buffs :format fmt :path path
			 :w w :h h :size size :stram-on-p t))))
      (error ()
	(when fd (isys:%sys-close fd))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun close-v4l2 (v4l2)
  "Trying to stop streaming action of the v4l2 device,
   unmap it's buffers and to close it."
  ;; I'm not using `with-slots` macro 'case some strange
  ;; sbcl warnings about `SB-PCL::SLOT-ACCESSOR`
  (when (v4l2-stram-on-p v4l2)
      (v4l2:stream-off (v4l2-fd v4l2))
      (setf (v4l2-stram-on-p v4l2) nil))
    (when (v4l2-buffers v4l2)
      (v4l2:unmap-buffers (v4l2-buffers v4l2))
      (setf (v4l2-buffers v4l2) nil))
    (when (v4l2-fd v4l2)
      (isys:%sys-close (v4l2-fd v4l2))
      (setf (v4l2-fd v4l2) nil))
    v4l2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-v4l2 ((v4l2-var path &key (w 640) (h 480)
			       (pixformat v4l2:pix-fmt-rgb24) (n-buffs 4))
		     &body body)
  "with-* macro for v4l2 devices."
  `(let ((,v4l2-var (open-v4l2 ,path
			       :w ,w
			       :h ,h
			       :pixformat ,pixformat
			       :n-buffs ,n-buffs)))
     (unwind-protect
	  (progn ,@body)
       (close-v4l2 ,v4l2-var))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-frames ((frame-var v4l2
				&key user-vars end-test-form return-form)
		     &body body)
  "Iteration macro for v4l2 devices around `do*` macro.
   Execute `body` each time call `v4l2:get-frame` was successful and until
   `end-test-form` is T, and call `v4l2:put-frame` after executing `body`.
   Binds `frame-var` to the current frame number (`v4l2:get-frame`).
   Additional variables can be specified via `user-vars`.
   Return `end-test-form` at the end.

   Example:
   (do-frames (frame v4l2
                     :user-vars((cnt 0 (1+ cnt)))
                     :end-test-form (> cnt 10)
                     :return-form t)
     (isys:%sys-write dest-fd (second (nth frame (v4l2-buff v4l2)))
                              (v4l2-size v4l2)))
  "
  (let ((fd (gensym "fd")))
    `(do* ((,fd (v4l2-fd ,v4l2))
	   (,frame-var (w/o-errors (v4l2:get-frame ,fd))
		       (w/o-errors (v4l2:get-frame ,fd)))
	   ,@user-vars)
	  (,end-test-form ,return-form)
       (when ,frame-var
	 ,@body
	 (v4l2:put-frame ,fd ,frame-var)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-v4l2-do-frames ((v4l2-var frame-var path
					 &key (w 640) (h 480)
					 (pixformat v4l2:pix-fmt-rgb24)
					 (n-buffs 4)
					 user-vars end-test-form return-form)
			       &body body)
  "`with-v4l2` and `do-frames` macros combination"
  `(with-v4l2 (,v4l2-var ,path :w ,w
			 :h ,h
			 :pixformat ,pixformat
			 :n-buffs ,n-buffs)
     (do-frames (,frame-var ,v4l2-var
			    :user-vars ,user-vars
			    :end-test-form ,end-test-form
			    :return-form ,return-form)
       ,@body)))
;;; </v4l2 syntax shugar>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <fork(3p) + execv(3p) wrappers>
(defun argv-to-foreign (argv)
  "Convert `argv` from list of the lisp strings in to the
   array of the C strings (for the `isys:%sys-execv`)"
  (loop
     :with ln           := (+ (length argv) 1)
     :with foreign-argv := (cffi:foreign-alloc :pointer :count ln)
     :for str           :in argv
     :as i              := 0 then (+ 1 i)
     :do (setf (cffi:mem-aref foreign-argv :pointer i)
	       (cffi:foreign-string-alloc str))
     :finally (progn
		(setf (cffi:mem-aref foreign-argv :pointer (- ln 1))
		      (cffi:null-pointer))
		(return foreign-argv))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun free-foreign-argv (ptr sz)
  "Free array of the C strings."
  (loop 
     :for i :below sz
     :do (cffi:foreign-free (cffi:mem-aref ptr :pointer i))
     :finally (cffi:foreign-free ptr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-foreign-argv ((argv args) &body body)
  "with-* macro for calling `isys:%sys-execv` "
  `(let ((,argv (argv-to-foreign ,args)))
     (unwind-protect (progn ,@body)
       (free-foreign-argv ,argv (+ (length ,args) 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct process-pipe 
  pid alivep input-fd output-fd error-fd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-process-pipe (cmd args)
  "Create a new process via `isys:%sys-execv`
   and return corresponding `process-pipe` instance"
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
	  (with-foreign-argv (argv (cons cmd args))
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
				       :input-fd parent-out
				       :output-fd parent-in
				       :error-fd parent-err))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-process-pipe (process-pipe)
  "Trying to kill process."
  (with-accessors ((pid process-pipe-pid) (in process-pipe-input-fd)
		   (out process-pipe-output-fd) (err process-pipe-error-fd)
		   (alivep process-pipe-alivep)) process-pipe
    (handler-case
	(progn
	  (isys:%sys-kill pid 15)	;term
	  (isys:%sys-kill pid 15)	;term
	  (isys:%sys-kill pid 9)	;and finally kill
	  ;; TODO - write path for iolib.syscalls
	  (isys:%sys-waitpid pid (cffi:null-pointer) 1)
	  (sleep 1)			;dump with-timeout alternative
	  (isys:%sys-waitpid pid (cffi:null-pointer) 1))
      ((or isys:echild isys:esrch) (c)
	(declare (ignorable c))
       (isys:%sys-close in)
       (isys:%sys-close out)
       (isys:%sys-close err)
       (setf alivep nil
	     in nil
	     out nil
	     err nil)
       process-pipe))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-process-pipe ((process-pipe cmd args) &body body)
  "with-* macro for running process."
  `(let ((,process-pipe (run-process-pipe ,cmd ,args)))
     (unwind-protect (progn ,@body)
       (when ,process-pipe
	 (kill-process-pipe ,process-pipe)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-process-pipes (binds &body body)
  "with-* macro for running multiple processes"
  (if binds
      `(with-process-pipe ,(car binds)
	 (with-process-pipes ,(cdr binds)
	   ,@body))
      `(progn ,@body)))
;;; <fork(3p) + execv(3p) wrappers>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <ffmpeg wrappers>
(defstruct ffmpeg-cmd
  stream
  (in "-")
  (out "-")
  (input-width 600)
  (input-height 480)
  output-with output-height
  (input-pix-fmt "rgb24")
  (input-format "rawvideo")
  (output-format "mpeg")
  (overwrite-out t)
  input-frame-rate
  output-frame-rate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ffmpeg-args (ffmpeg-cmd)
  "Return list of the ffmpeg arguments, i.e. convert ffmpeg arguments
   from struct `ffmpeg-cmd` form in to the form what can be passed to
   `sb-ext:run-program`."
  (with-slots (in input-format input-width input-height input-pix-fmt
		  input-frame-rate output-format output-with output-height
		  output-frame-rate out overwrite-out)
      ffmpeg-cmd
    (append (list "-f" input-format)
	    (list "-s" (format nil "~Dx~D" input-width input-height))
	    (list "-pix_fmt" input-pix-fmt)
	    (when input-frame-rate
	      (list "-r" input-frame-rate))
	    (list "-i" in)
	    (when output-format
	      (list "-f" output-format))
	    (when (and output-with output-height)
	      (list "-s" (format nil "~Dx~D" output-with output-height)))
	    (when output-frame-rate
	      (list "-r" output-frame-rate))
	    (when overwrite-out
	      (list "-y"))
	    (list out))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-ffmpeg-pipe (ffmpeg-cmd)
  "Run ffmpeg with ffmpeg-cmd arguments
   and return corresponding `process-pipe` instance"
  (let ((args (ffmpeg-args ffmpeg-cmd)))
    (format t "Running  \"~{~a ~}\"" args)
    (run-process-pipe "/usr/bin/ffmpeg" args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-ffmpeg-pipe ((pipe-var ffmpeg-cmd) &body body)
  "with-* macro for running pipe with ffmpeg process."
  `(let (,pipe-var (run-ffmpeg-pipe ,ffmpeg-cmd))
     (unwind-protect ,@body)
     (kill-process-pipe ,pipe-var)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-ffmpeg-pipes (binds &body body)
  "`with-ffmpeg-pipe` macro for multiple `process-pipe`"
  (if binds
      `(with-ffmpeg-pipe ,(car binds)
	 (with-ffmpeg-pipes ,(cdr binds)
	   ,@body))
      `(progn ,@body)))
;;; </ffmpeg wrappers>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
