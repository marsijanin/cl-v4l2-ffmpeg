(eval-when (:load-toplevel :compile-toplevel)
  (defstruct glframe widget data (lock (bt:make-lock)))
  (defparameter *want-v4l2* (make-v4l2 :path "/dev/video0" :w 352 :h 288))
  (defparameter *v4l2* nil)

  (defparameter *glframe* (make-glframe))

  (defparameter *cap-thread-stop* nil)

  (defparameter *render-thread-stop* (bt:make-condition-variable))
  (defparameter *render-thread-lock* (bt:make-lock "Render thread lock"))

  (defparameter *ffmpeg-pipe0* nil)
  (defparameter *ffmpeg-pipe1* nil)) ;</ eval-when >

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

(defun capture-thread ()
  (format t "cap thread start~%")
  (with-v4l2 (v4l2 (v4l2-path *want-v4l2*)
		   :w (v4l2-w *want-v4l2*)
		   :h (v4l2-h *want-v4l2*))
    (with-slots (w h size format) v4l2
      (setf *v4l2* v4l2
	    (glframe-data *glframe*) (make-array (* h w 4)
				      :element-type '(unsigned-byte 8)
				      :initial-element #xff))
      (format t "got ~Dx~D size ~D, format ~S~%"
	      w h size (format-string format))
      (let ((args0 (ffmpeg-args (make-ffmpeg-cmd :input-width w
						 :input-height h
						 :out "video0.mpg")))
	    (args1 (ffmpeg-args (make-ffmpeg-cmd :input-width w
						 :input-height h
						 :out "video1.mpg")))
	    (lock (glframe-lock *glframe*))
	    selection)
	(with-process-pipes ((ffmpeg0  "/usr/bin/ffmpeg" args0)
			     (ffmpeg1  "/usr/bin/ffmpeg" args1))
	  (format t "Runing \"ffmpeg ~{~a ~}\"" args0)
	  (format t "Runing \"ffmpeg ~{~a ~}\"" args1)
	  (setf *ffmpeg-pipe0* ffmpeg0)
	  (setf *ffmpeg-pipe1* ffmpeg1)
	  (do-frames (frame buff v4l2
			    :end-test-form *cap-thread-stop*
			    :return-form (format t "cap thread exit~%"))
	    (isys:%sys-write (process-pipe-input-fd (if selection ffmpeg0 ffmpeg1))
			     (second buff)
			     size)
	    (bt:with-lock-held (lock)
	      (fast-v4l2-rgb-buffer->argb-texture (second buff) (glframe-data *glframe*) (* w h)))
	    #|cameras switching commands here|#
	    (setf selection (if selection nil t))
	    (when (glframe-widget *glframe*)
	      (gtk:with-main-loop
		(gtk:widget-queue-draw (glframe-widget *glframe*))))))))))

(defun camera-init (widget)
  (declare (ignore widget))
  (gl:clear-color 0.8 0.8 0.8 0.8)
  (gl:enable :texture-rectangle-arb :depth-test)
  (gl:depth-func :lequal)

  (gl:bind-texture :texture-rectangle-arb 0)

  (gl:tex-image-2d :texture-rectangle-arb
		   0
		   :rgb8
		   (v4l2-w *v4l2*)
		   (v4l2-h *v4l2*)
		   0
		   :rgba
		   :unsigned-byte
		   (glframe-data *glframe*))

  (gl:new-list 1 :compile)

  (gl:begin :quads)
  (gl:tex-coord 0 (v4l2-h *v4l2*))
  (gl:vertex 0.0 0.0)
  (gl:tex-coord 0 0)
  (gl:vertex 0.0 1.0)
  (gl:tex-coord (v4l2-w *v4l2*) 0)
  (gl:vertex 1.0 1.0)
  (gl:tex-coord (v4l2-w *v4l2*) (v4l2-h *v4l2*))
  (gl:vertex 1.0 0.0)
  (gl:end)
  (gl:end-list)

  (gl:clear-depth 1.0)
  (gl:flush))

(defun camera-draw (widget event)
  (declare (ignorable widget event))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:bind-texture :texture-rectangle-arb 0)

  (when (glframe-data *glframe*)
    (let ((lock (glframe-lock *glframe*)))
      (bt:with-lock-held (lock)
	(gl:tex-sub-image-2d :texture-rectangle-arb 0
			     0 0
			     (v4l2-w *v4l2*)
			     (v4l2-h *v4l2*)
			     :rgba
			     :unsigned-byte
			     (glframe-data *glframe*)))))

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
  (let ((cap-thread (bt:make-thread #'capture-thread :name "capturer")))
    (sleep 1)
    (gtk:with-main-loop
      (let ((window (make-instance 'gtk:gtk-window
				   :type :toplevel
				   :window-position :center
				   :title "Hello world!"
				   :default-width (v4l2-w *v4l2*)
				   :default-height (v4l2-h *v4l2*)))
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
	(setf (glframe-widget *glframe*) (make-instance 'gtkglext:gl-drawing-area
					     :on-init #'camera-init
					     :on-expose #'camera-draw))
	(gtk:box-pack-start hbox vbox :expand nil)
	(gtk:box-pack-start hbox (glframe-widget *glframe*) :expand t)
	(gtk:box-pack-start vbox quit-button :expand nil)
	(gtk:container-add window hbox)
	(gtk:widget-show window :all t)))

;; Wait for window destruction
    (bt:with-lock-held (*render-thread-lock*)
      (bt:condition-wait *render-thread-stop* *render-thread-lock*))
    (setq *cap-thread-stop* t)
    (bt:join-thread cap-thread)))

