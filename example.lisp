;; LD_PRELOAD=/usr/lib/libv4l/v4l2convert.so sbcl --load example.lisp
;; some example of the usage cl-v4l2 with ffmpeg
;; Copyright 2010 Nikolay V. Razbegaev <marsijanin@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapcar #'(lambda (asdf) (asdf:oos 'asdf:load-op asdf))
	'(:cl-ffmpeg :cl-gtk2-gtkglext))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package :ffmpeg)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using defstruct nstead of defclass for simplify for now
;; 'case thre is no inheritance
(defstruct frameshow	      ;structure for representign captured v4l2 frame
  widget		      ;widget there frame will be shown
  data			      ;frame data in RGBA format
  (lock (bt:make-lock))	      ;data lock
  ffmpeg-cmd 		      ;ffmpeg parameters (output format etc.)
  ffmpeg-pipe)		      ;ffmpeg process
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
(defun process-frameshow (frameshow v4l2 frame-n)
  "Process frameshow instance:
   - send current frame data (frame-n'th of the buffers of the v4l2 instance)
     to the ffmpeg process;
   - update widget data and queue widget to redraw."
  (with-accessors ((pipe frameshow-ffmpeg-pipe) (data frameshow-data)
		   (widget frameshow-widget) (lock frameshow-lock)) frameshow
    (with-accessors ((fd process-pipe-input-fd)) pipe
      (with-accessors ((buffers v4l2-buffers) (size v4l2-size)
		       (w v4l2-w) (h v4l2-h)) v4l2
	(let ((buffer (second (nth frame-n buffers))))
	  ;; send current v4l2 frame buffer data to the ffmpeg pipe
	  ;; (if there is some)
	  (when pipe
	    (isys:%sys-write fd buffer size))
	  ;; convert current v4l2 framen data to the widget format
	  ;; (if there is some)
	  (when data
	    (bt:with-lock-held (lock)
	      (fast-v4l2-rgb-buffer->argb-texture buffer data (* w h))))
	  ;; redraw widget (if there is some)
	  (when widget
	    (gtk:with-main-loop
	      (gtk:widget-queue-draw widget))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some stuff from cl-v4l2 example:
(defparameter *want-v4l2* (make-v4l2 :path "/dev/video0" :w 352 :h 288))
(defparameter *v4l2* nil)
(defparameter *frameshow*
  (make-frameshow :ffmpeg-cmd (make-ffmpeg-cmd :out "out.mpg")))
(defparameter *cap-thread-stop* nil)
(defparameter *render-thread-stop* (bt:make-condition-variable))
(defparameter *render-thread-lock* (bt:make-lock "Render thread lock"))
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
(defun capture-thread ()
  (format t "cap thread start~%")
  (with-v4l2 (v4l2 (v4l2-path *want-v4l2*)
		   :w (v4l2-w *want-v4l2*)
		   :h (v4l2-h *want-v4l2*))
    (with-accessors ((w v4l2-w) (h v4l2-h)
		     (size v4l2-size) (format v4l2-format)) v4l2
      (with-accessors ((pipe frameshow-ffmpeg-pipe) (cmd frameshow-ffmpeg-cmd)
		       (data frameshow-data)) *frameshow*
	(setf *v4l2* v4l2
	      data
	      (make-array (* h w 4)
			  :element-type '(unsigned-byte 8)
			  :initial-element #xff))
	(when cmd
	  (with-accessors ((ffmpeg-iw ffmpeg-cmd-input-width)
			   (ffmpeg-ih ffmpeg-cmd-input-height)) cmd
	    (setf ffmpeg-ih h
		  ffmpeg-iw  w
		  pipe  (run-ffmpeg-pipe cmd))))
	(format t "got ~Dx~D size ~D, format ~S~%"
		w h size (format-string format))
	(do-frames (frame v4l2
			  :end-test-form *cap-thread-stop*
			  :return-form (format t "cap thread exit~%"))
	  (process-frameshow *frameshow* v4l2 frame))
	(when pipe
	  (kill-process-pipe pipe)
	  (setf pipe nil))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
		   (frameshow-data *frameshow*))

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

  (when (frameshow-data *frameshow*)
    (let ((lock (frameshow-lock *frameshow*)))
      (bt:with-lock-held (lock)
	(gl:tex-sub-image-2d :texture-rectangle-arb 0
			     0 0
			     (v4l2-w *v4l2*)
			     (v4l2-h *v4l2*)
			     :rgba
			     :unsigned-byte
			     (frameshow-data *frameshow*)))))

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
	(setf (frameshow-widget *frameshow*)
	      (make-instance 'gtkglext:gl-drawing-area
			     :on-init #'camera-init
			     :on-expose #'camera-draw))
	(gtk:box-pack-start hbox vbox :expand nil)
	(gtk:box-pack-start hbox (frameshow-widget *frameshow*) :expand t)
	(gtk:box-pack-start vbox quit-button :expand nil)
	(gtk:container-add window hbox)
	(gtk:widget-show window :all t)))

    ;; Wait for window destruction
    (bt:with-lock-held (*render-thread-lock*)
      (bt:condition-wait *render-thread-stop* *render-thread-lock*))
    (setq *cap-thread-stop* t)
    (bt:join-thread cap-thread)))

(test)