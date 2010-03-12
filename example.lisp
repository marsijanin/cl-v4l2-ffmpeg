;; some example of the usage cl-v4l2 with ffmpeg
;; Copyright 2010 Nikolay V. Razbegaev <marsijanin@gmail.com>
;; Launching:
;; LD_PRELOAD=/usr/lib/libv4l/v4l2convert.so sbcl \
;; --load example.lisp                            \
;; --eval "(ffmpeg-example:test)"
;; for multiple cameras on one v4l2 device example:
;; --eval "(ffmpeg-example:test-mosaic 2)"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapcar #'(lambda (asdf) (asdf:oos 'asdf:load-op asdf))
	'(:cl-ffmpeg :cl-gtk2-gtkglext))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :ffmpeg-example
  (:use :common-lisp :ffmpeg)
  (:export #:test #:test-mosaic))
(in-package :ffmpeg-example)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass frameshow (gtkglext:gl-drawing-area)
  ((data :initform nil :type (or null (simple-array (unsigned-byte 8) (*)))
	 :accessor frameshow-data :initarg :data)
   (lock :initform (bt:make-lock) :accessor frameshow-lock)
   (v4l2 :initarg :v4l2 :reader frameshow-v4l2)
   (ffmpeg-cmd :accessor frameshow-ffmpeg-cmd :initform nil :initarg :ffmpeg-cmd)
   (ffmpeg-pipe :accessor frameshow-ffmpeg-pipe :initform nil))
  (:metaclass gobject:gobject-class)
  #|(:default-initargs :on-init #'camera-init :on-expose #'camera-draw)|#)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun restart-frameshow-ffmpeg (frameshow cmd)
  (with-accessors ((pipe frameshow-ffmpeg-pipe)) frameshow
    (when pipe
      (kill-process-pipe pipe))
    (setf pipe (run-ffmpeg-pipe cmd))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod (setf frameshow-ffmpeg-cmd)
    :after ((frameshow frameshow) (cmd ffmpeg-cmd))
  (restart-frameshow-ffmpeg frameshow cmd))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod shared-initialize :after ((instance frameshow) slot-names
				      &rest initargs &key ffmpeg-cmd)
  (declare (ignorable slot-names initargs))
  (restart-frameshow-ffmpeg instance ffmpeg-cmd))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fast-v4l2-rgb-buffer->argb-texture (buff texture size)
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type (simple-array (unsigned-byte 8) (*)) texture)
	   (type (integer 0 #.array-dimension-limit) size))
  (dotimes (i size)
    (declare (type (integer 0 #.array-dimension-limit) i))
    (let ((r (cffi:mem-aref buff :uchar (+ (* 3 i) 0)))
	  (g (cffi:mem-aref buff :uchar (+ (* 3 i) 1)))
	  (b (cffi:mem-aref buff :uchar (+ (* 3 i) 2))))
      (declare (type (unsigned-byte 8) r g b))
      (setf (aref texture (+ (* 4 i) 0)) r
	    (aref texture (+ (* 4 i) 1)) g
	    (aref texture (+ (* 4 i) 2)) b))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun process-frameshow (frameshow frame-n)
  "Process frameshow instance:
   - send current frame data (frame-n'th of the buffers of the v4l2 instance)
     to the ffmpeg process;
   - update widget data and queue widget to redraw."
  (with-accessors ((pipe frameshow-ffmpeg-pipe) (data frameshow-data)
		   (lock frameshow-lock) (v4l2 frameshow-v4l2)) frameshow
    (with-accessors ((fd process-pipe-input-fd)) pipe
      (with-accessors ((buffers v4l2-buffers) (size v4l2-size)
		       (w v4l2-w) (h v4l2-h)) v4l2
	(let ((buffer (second (nth frame-n buffers))))
	  ;; send current v4l2 frame buffer data to the ffmpeg pipe
	  ;; (if there is some)
	  (when pipe
	    (isys:%sys-write fd buffer size))
	  ;; convert current v4l2 framen data to the widget format
	  ;; and queue to redraw (if there is some)
	  (when data
	    (bt:with-lock-held (lock)
	      (fast-v4l2-rgb-buffer->argb-texture buffer data (* w h))))
	  (gtk:with-main-loop
	    (gtk:widget-queue-draw frameshow)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some stuff from cl-v4l2 example:
(defparameter *cap-thread-stop* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun char-at (pos data)
  (code-char (ldb (byte 8 (* 8 pos)) data)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun format-string (pixfmt)
  (format nil "~C~C~C~C"
	  (char-at 0 pixfmt)
	  (char-at 1 pixfmt)
	  (char-at 2 pixfmt)
	  (char-at 3 pixfmt)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-capture-thread-fn (frameshow)
  #'(lambda ()
      (format t "cap thread start~%")
      (do-frames (frame (frameshow-v4l2 frameshow)
			:end-test-form *cap-thread-stop*
			:return-form
			(progn
			  (when (frameshow-ffmpeg-pipe frameshow)
			    (kill-process-pipe (frameshow-ffmpeg-pipe frameshow)))
			  (format t "cap thread exit~%")))
	(process-frameshow frameshow frame))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass mosaic-fragment (frameshow)
  ((camera-switcher :reader mosaic-fragment-camera-switcher
		   :initarg :camera-switcher))
  (:metaclass gobject:gobject-class))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-capture-thread-fn-mosaic (mosaic)
  #'(lambda ()
      (format t "cap thread start~%")
      (do-frames (frame (frameshow-v4l2 (aref mosaic 0))
			:user-vars ((fragments-ln (length mosaic))
				    (ref 0 (mod (1+ ref) fragments-ln))
				    (fragment (aref mosaic ref) (aref mosaic ref))
				    ;; If we will be call switchers inside `do-frames`
				    ;; we will be switch camera _only when_
				    ;; `v4l2:get-frame` call will be successful
				    (switch (mosaic-fragment-camera-switcher fragment))
				    (switching-result (funcall switch) (funcall switch)))
			:end-test-form *cap-thread-stop*
			:return-form
			(dotimes (i fragments-ln)
			  (with-accessors ((pipe frameshow-ffmpeg-pipe))
			      (aref mosaic i)
			    (when pipe
			      (kill-process-pipe pipe))
			    (format t "cap thread exit~%"))))
	(unless switching-result
	  (format t "Switching camera failed!~%"))
	(process-frameshow fragment frame))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun camera-init (widget)
  "Modified camera initialisation function from cl-v4l2 example."
  (with-accessors ((v4l2 frameshow-v4l2) (data frameshow-data)) widget
    (with-accessors ((w v4l2-w) (h v4l2-h)) v4l2
      (gl:clear-color 0.8 0.8 0.8 0.8)
      (gl:enable :texture-rectangle-arb :depth-test)
      (gl:depth-func :lequal)
      (gl:bind-texture :texture-rectangle-arb 0)
      (gl:tex-image-2d :texture-rectangle-arb
		       0
		       :rgb8
		       w
		       h
		       0
		       :rgba
		       :unsigned-byte
		       data)
      (gl:new-list 1 :compile)
      (gl:begin :quads)
      (gl:tex-coord 0 h)
      (gl:vertex 0.0 0.0)
      (gl:tex-coord 0 0)
      (gl:vertex 0.0 1.0)
      (gl:tex-coord w 0)
      (gl:vertex 1.0 1.0)
      (gl:tex-coord w h)
      (gl:vertex 1.0 0.0)
      (gl:end)
      (gl:end-list)
      (gl:clear-depth 1.0)
      (gl:flush))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun camera-draw (widget event)
  "Modified camera (re)drawind function from cl-v4l2 example."
  (declare (ignorable event))
  (with-accessors ((v4l2 frameshow-v4l2) (data frameshow-data)
		   (lock frameshow-lock)) widget
    (with-accessors ((w v4l2-w) (h v4l2-h)) v4l2
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      (gl:bind-texture :texture-rectangle-arb 0)
      (when data
	(bt:with-lock-held (lock)
	  (gl:tex-sub-image-2d :texture-rectangle-arb 0
			       0 0
			       w
			       h
			       :rgba
			       :unsigned-byte
			       data)))
      ;; Keep ratio 4:3
      (multiple-value-bind (w h)
	  (gdk:drawable-get-size (gtk:widget-window widget))
	(let ((w1 w)
	      (h1 h))
	  (when (and (> w 0) (> h 0))
	    (if (> (/ w h) 4/3)
		(setq h1 h
		      w1 (* h 4/3))
		(setq w1 w
		      h1 (* w 3/4))))
	  (gl:viewport 0 0 w1 h1)))
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 19.0 1.0 1.0 10.0)
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (glu:look-at 0.0 0.0 3.0
		   0.0 0.0 0.0
		   0.0 1.0 0.0)
      (gl:translate -0.5 -0.5 0.0)
      (gl:call-list 1)
      (gl:flush))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test (&key (v4l2-path "/dev/video0") (want-width 352) (want-height 288)
	     (file "out.mpg"))
  (with-v4l2 (v4l2 v4l2-path :w want-width :h want-height)
    (let ((render-thread-stop (bt:make-condition-variable))
	  (render-thread-lock (bt:make-lock "Render thread lock"))
	  frameshow capturer)
      (gtk:within-main-loop
	(gtk:let-ui
	    (gtk:gtk-window
	     :var win
	     :type :toplevel
	     :window-position :center
	     :title "Camera"
	     :default-width (v4l2-w v4l2)
	     :default-height (v4l2-h v4l2)
	     (frameshow
	      :var frm
	      :data (make-array (* (v4l2-h v4l2) (v4l2-w v4l2) 4)
				:element-type '(unsigned-byte 8)
				:initial-element #xff)
	      :v4l2 v4l2
	      :ffmpeg-cmd (make-ffmpeg-cmd :out file
					   :input-width (v4l2-w v4l2)
					   :input-height (v4l2-h v4l2))
	      :on-init #'camera-init
	      :on-expose #'camera-draw))
	  (when *cap-thread-stop*
	    (setf *cap-thread-stop* nil))
	  (setf frameshow frm)
	  (gobject:connect-signal win "destroy"
				  #'(lambda (widget)
				      (declare (ignorable widget))
				      (bt:condition-notify render-thread-stop)))
	  (gtk:widget-show win)))	; </within-main-loop> !!!
      (sleep 1)
      (setf capturer (bt:make-thread (make-capture-thread-fn frameshow)
				     :name "capturer"))
      ;; Wait for window destruction
      (bt:with-lock-held (render-thread-lock)
	(bt:condition-wait render-thread-stop render-thread-lock))
      (setq *cap-thread-stop* t)
      (bt:join-thread capturer)
      t)))				;just return something
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-mosaic (n &key (v4l2-path "/dev/video0") (want-width 352)
		    (want-height 288) (prefix "camera"))
  (with-v4l2 (v4l2 v4l2-path :w want-width :h want-height)
    (let ((render-thread-stop (bt:make-condition-variable))
	  (render-thread-lock (bt:make-lock "Render thread lock"))
	  mosaic capturer)
      (gtk:within-main-loop
	(gtk:let-ui
	    (gtk:gtk-window
	     :var win
	     :type :toplevel
	     :window-position :center
	     :title "Camera"
	     :default-width (v4l2-w v4l2)
	     :default-height (v4l2-h v4l2)
	     (gtk:h-box :var hbox))
	  (let ((m (make-array n)))
	    (dotimes (i n)
	      (setf (aref m i)
		    (make-instance 'mosaic-fragment
				   :data (make-array (* (v4l2-h v4l2) (v4l2-w v4l2) 4)
						     :element-type '(unsigned-byte 8)
						     :initial-element #xff)
				   :v4l2 v4l2
				   :ffmpeg-cmd (make-ffmpeg-cmd :out (format nil
									     "~a~d.mpg"
									     prefix i)
								:input-width (v4l2-w v4l2)
								:input-height (v4l2-h v4l2))
				   ;; Camera switching commoand here
				   :camera-switcher #'(lambda ()
							(format t "Switching to camera ~d" i))
				   :on-init #'camera-init
				   :on-expose #'camera-draw))
	      (gtk:box-pack-start hbox (aref m i)))
	  (when *cap-thread-stop*
	    (setf *cap-thread-stop* nil))
	  (setf mosaic m)
	  (gobject:connect-signal win "destroy"
				  #'(lambda (widget)
				      (declare (ignorable widget))
				      (bt:condition-notify render-thread-stop)))
	  (gtk:widget-show win))))	; </within-main-loop> !!!
      (sleep 1)
      (setf capturer (bt:make-thread (make-capture-thread-fn-mosaic mosaic)
				     :name "capturer"))
      ;; Wait for window destruction
      (bt:with-lock-held (render-thread-lock)
	(bt:condition-wait render-thread-stop render-thread-lock))
      (setq *cap-thread-stop* t)
      (bt:join-thread capturer)
      t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
