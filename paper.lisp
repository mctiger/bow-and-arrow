;;; -*- Mode: LISP -*-
;;; Copyright (c) 2011 Kaïraba Cissé, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :bow-and-arrow)

(defclass paper (base)
  (;; `strings' is a list of string. Each string represents just one line
   (strings     :initform nil :initarg :strings :accessor %strings)))

(defmethod draw ((paper paper))
  (draw-image paper +path-image-paper+)
  (let ((top 15)
	(strings (%strings paper)))
    (dolist (text strings)
      (sdl:draw-string-solid-* text 
			       (/ *video-width* 2)
			       (+ top (%y paper))
			       :font (sdl:initialise-font sdl:*font-6x13*)
			       :color sdl:*black*
			       :justify :center)
      (incf top 13)))) ;; 13 because in *font-6x13* the second number is 13


(defun make-paper (&optional strings)
  (let ((x (- (/ *video-width* 2) (/ +paper-width+ 2)))
	(y (- (round (/ *video-height* 3)) (/  +paper-height+ 2))))
  (make-instance 'paper 
		 :x x 
		 :y y 
		 :width  +paper-width+ 
		 :height  +paper-height+
		 :strings strings )))

(defun draw-level-paper (level)
  (case level 
    (1
     (draw (make-paper *level-one-paper*)))
    (2
     (draw (make-paper *level-two-paper*)))
    (3
     (draw (make-paper *level-three-paper*)))
    (4
     (draw (make-paper *level-four-paper*)))

    ))


(defun draw-copyright-paper nil
  (draw (make-paper *copyright-paper*)))

(defun draw-no-more-arrows-paper nil
  (draw (make-paper *hero-without-arrows-paper*)))


(defun draw-end-paper nil
  (draw (make-paper *end-paper*)))
