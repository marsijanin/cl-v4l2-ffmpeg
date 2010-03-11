;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsystem #:cl-ffmpeg
  :description "Interfaces for converting with ffmpeg video captured by cl-v4l2"
  :author "Nikolay V. Razbegaev <marsijanin@gmail.com>"
  :licence "Not decide yet" 
  :depends-on (:cl-v4l2)
  :components ((:file "ffmpeg")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
