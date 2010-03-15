;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; Interfaces for converting with ffmpeg video captured by cl-v4l2"
;; Copyright 2010 Nikolay V. Razbegaev <marsijanin@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage #:cl-ffmpeg
  (:nicknames #:ffmpeg)
  (:use :common-lisp)
  (:export
   ;; struct v4l2
   #:v4l2
   #:v4l2-p
   #:make-v4l2
   #:v4l2-fd
   #:v4l2-buffers
   #:v4l2-format
   #:v4l2-w
   #:v4l2-h
   #:v4l2-size
   #:v4l2-path
   #:v4l2-stream-on-p
   ;; v4l2 manipulations:
   #:open-v4l2
   #:close-v4l2
   #:with-v4l2
   #:do-frames
   #:with-v4l2-do-frames
   ;; struct ffmpeg-cmd
   #:ffmpeg-cmd
   #:ffmpeg-cmd-p
   #:make-ffmpeg-cmd
   #:ffmpeg-cmd-in
   #:ffmpeg-cmd-out
   #:ffmpeg-cmd-input-width
   #:ffmpeg-cmd-input-height
   #:ffmpeg-cmd-output-with
   #:ffmpeg-cmd-output-height
   #:ffmpeg-cmd-input-pix-fmt
   #:ffmpeg-cmd-input-format
   #:ffmpeg-cmd-output-format
   #:ffmpeg-cmd-input-frame-rate
   #:ffmpeg-cmd-output-frame-rate
   ;; struct process-pipe
   #:process-pipe
   #:process-pipe-p
   #:make-process-pipe
   #:process-pipe-alivep
   #:process-pipe-input-fd
   #:process-pipe-output-fd
   #:process-pipe-error-fd
   #:process-pipe-pid
   ;; process-pipe manipulations:
   #:run-process-pipe
   #:kill-process-pipe
   #:with-process-pipe
   #:with-process-pipes
   ;; run-ffmpeg-pipe
   #:run-ffmpeg-pipe
   #:with-ffmpeg-pipe
   #:with-ffmpeg-pipes
   ;; CLOS stuff
   #:framesprocessor
   #:framesprocessor-ffmpeg-cmd
   #:framesprocessor-ffmpeg-pipe
   #:framesprocessor-v4l2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
