(eval-when (:load-toplevel :compile-toplevel)
  (defvar *capture-device* "/dev/video0")
  (defparameter *v4l2* nil)

  ;; what we want from camera
  (defvar *want-width* 352)
  (defvar *want-height* 288)

  ;; what we really get

  (defparameter *camera-widget* nil)
  (defparameter *camera-data* nil)
  (defparameter *camera-data-lock* (bt:make-lock "Camera data lock"))

  (defparameter *cap-thread-stop* nil)

  (defparameter *render-thread-stop* (bt:make-condition-variable))
  (defparameter *render-thread-lock* (bt:make-lock "Render thread lock"))

  (defparameter *ffmpeg-pipe* nil)) ;</ eval-when >

(defmacro without-errors (&body body)
  `(handler-case (progn ,@body)
     (error (c) (format t "suppressed error: ~A~%" c) nil)))

(defun char-at (pos data)
  (code-char (ldb (byte 8 (* 8 pos)) data)))

(defun format-string (pixfmt)
  (format nil "~C~C~C~C"
	  (char-at 0 pixfmt)
	  (char-at 1 pixfmt)
	  (char-at 2 pixfmt)
	  (char-at 3 pixfmt)))


(defun capture-thread ()
  (format t "cap thread start~%")
  (with-v4l2-do-frames (v4l2 frame buff *capture-device*
			     :w *want-width*
			     :h *want-height*
			     :end-test-form *cap-thread-stop*
			     :return-form (format t "cap thread exit~%"))
    (isys:%sys-write (process-pipe-input-fd *ffmpeg-pipe*)
		     (second buff)
		     (* *want-height* *want-width*))
    (bt:with-lock-held (*camera-data-lock*)
      (declare (optimize (speed 3) (debug 0) (safety 0))
	       (type (simple-array (unsigned-byte 8) (#.(* *want-width* *want-height* 4)))
		     *camera-data*)
	       (type fixnum *want-height* *want-width*))
      (loop for i fixnum from 0 below (* *want-height* *want-width*) do
	   (let ((r (cffi:mem-aref buff :uchar (+ (* 3 i) 0)))
		 (g (cffi:mem-aref buff :uchar (+ (* 3 i) 1)))
		 (b (cffi:mem-aref buff :uchar (+ (* 3 i) 2))))
	     (setf (aref *camera-data* (+ (* 4 i) 0)) r
		   (aref *camera-data* (+ (* 4 i) 1)) g
		   (aref *camera-data* (+ (* 4 i) 2)) b))))
    (when *camera-widget*
      (gtk:with-main-loop
	(gtk:widget-queue-draw *camera-widget*)))))

(defun camera-init (widget)
  (declare (ignore widget))
  (gl:clear-color 0.8 0.8 0.8 0.8)
  (gl:enable :texture-rectangle-arb :depth-test)
  (gl:depth-func :lequal)

  (gl:bind-texture :texture-rectangle-arb 0)

  (gl:tex-image-2d :texture-rectangle-arb
		   0
		   :rgb8
		   *want-width*
		   *want-height*
		   0
		   :rgba
		   :unsigned-byte
		   *camera-data*)

  (gl:new-list 1 :compile)

  (gl:begin :quads)
  (gl:tex-coord 0 *want-height*)
  (gl:vertex 0.0 0.0)
  (gl:tex-coord 0 0)
  (gl:vertex 0.0 1.0)
  (gl:tex-coord *want-width* 0)
  (gl:vertex 1.0 1.0)
  (gl:tex-coord *want-width* *want-height*)
  (gl:vertex 1.0 0.0)
  (gl:end)
  (gl:end-list)

  (gl:clear-depth 1.0)
  (gl:flush))

(defun camera-draw (widget event)
  (declare (ignorable widget event))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:bind-texture :texture-rectangle-arb 0)

  (when *camera-data*
    (bt:with-lock-held (*camera-data-lock*)
      (gl:tex-sub-image-2d :texture-rectangle-arb 0
			   0 0
			   *want-width*
			   *want-height*
			   :rgba
			   :unsigned-byte
			   *camera-data*)))

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
  (gl:flush))

(defun test ()
  (unless *ffmpeg-pipe*
    (setf *ffmpeg-pipe* (run-ffmpeg-pipe
			 :input-size (format nil "~ax~a" *want-width* *want-height*)
			 :out "video.mpg")))
  (unless *camera-data* 
    (setf *camera-data* (make-array (* *want-height* *want-width* 4)
				    :element-type '(unsigned-byte 8)
				    :initial-element #xff)))
  (let ((cap-thread (bt:make-thread #'capture-thread :name "capturer")))
    (gtk:with-main-loop
      (let ((window (make-instance 'gtk:gtk-window
				   :type :toplevel
				   :window-position :center
				   :title "Hello world!"
				   :default-width *want-width*
				   :default-height *want-height*))
	    (hbox (make-instance 'gtk:h-box))
	    (vbox (make-instance 'gtk:v-box))
	    (quit-button (make-instance 'gtk:button :label "Quit")))
	(gobject:connect-signal quit-button "clicked"
				(lambda (widget)
				  (declare (ignore widget))
				  (bt:condition-notify *render-thread-stop*)))
	(gobject:connect-signal window "destroy"
				(lambda (widget)
				  (declare (ignore widget))
				  (bt:condition-notify *render-thread-stop*)))

;; Capture process needs to know which widget to ask for redraw
	(setq *camera-widget* (make-instance 'gtkglext:gl-drawing-area
					     :on-init #'camera-init
					     :on-expose #'camera-draw))
	(gtk:box-pack-start hbox vbox :expand nil)
	(gtk:box-pack-start hbox *camera-widget* :expand t)
	(gtk:box-pack-start vbox quit-button :expand nil)
	(gtk:container-add window hbox)
	(gtk:widget-show window :all t)))

;; Wait for window destruction
    (bt:with-lock-held (*render-thread-lock*)
      (bt:condition-wait *render-thread-stop* *render-thread-lock*))
    (setq *cap-thread-stop* t)
    (bt:join-thread cap-thread)))

