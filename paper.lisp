;;; -*- Mode: LISP -*-
;;; Copyright (c) 2011-2012 Kaïraba Cissé, All Rights Reserved
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

;;; Paper
(defstruct (paper (:include base 
			    (x (- (/ *video-width* 2) (/ +paper-width+ 2)))
			    (y (- (round (/ *video-height* 3)) (/  +paper-height+ 2)))
			    (width  +paper-width+)
			    (height  +paper-height+)))
  (strings nil :type list)
  )

;; %strings
(defmethod %strings ((paper paper))
  (paper-strings paper))

(defmethod (setf %strings) ((paper paper) strings)
  (setf (paper-strings paper) strings))


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

(defun draw-level-paper (level)
  (declare (type fixnum level))
  (case level 
    (1
     (draw (make-paper :strings *level-one-paper*)))
    (2
     (draw (make-paper :strings *level-two-paper*)))
    (3
     (draw (make-paper :strings *level-three-paper*)))
    (4
     (draw (make-paper :strings *level-four-paper*)))
    (5
     (draw (make-paper :strings *level-five-paper*)))))

(defun draw-dead-paper (level)
  (case level
    (4 
     (draw (make-paper :strings *end-of-level-four-paper*)))))

(defun draw-copyright-paper nil
  (draw (make-paper :strings *copyright-paper*)))

(defun draw-no-more-arrows-paper nil
  (draw (make-paper :strings *hero-without-arrows-paper*)))


(defun draw-end-paper nil
  (draw (make-paper :strings *end-paper*)))


(defun draw-end-paper nil
  (draw (make-paper :strings *end-paper*)))

