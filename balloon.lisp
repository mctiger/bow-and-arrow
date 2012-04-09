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

(defclass balloon (base)
  (;; color can be :red or :yellow for the moment
   (color :initform :red :initarg :color :accessor %color)))


(defmethod draw ((balloon balloon))
  (let ((path-image (if (eq (%color balloon) :red)
			+path-image-balloon+
			+path-image-balloon-yellow+))
	(path-image-dead  (if (eq (%color balloon) :red)
			      +path-image-balloon-dead+
			      +path-image-balloon-yellow-dead+)))
    (if (%alive-p balloon)
	  (draw-image balloon path-image)
	  (draw-image balloon path-image-dead))))


(defun make-balloon (x &key (y *video-height*) 
		            (color :red) 
		            (width +balloon-width+) 
		            (height +balloon-height+) 
		            (speed *speed-balloon*))
    (make-instance 'balloon
		   :x x 
		   :y y
		   :color color
		   :width width
		   :height height
		   :speed speed))

(defun make-balloons-list (n)
  (let ((balloons nil)
	;; we multiply by 2 because there is a blank space 
	;; (of size +balloon-width+) between the last balloon and the board
	(x (- *video-width* (* 2 +balloon-width+)))) 
    (loop repeat n do
	 (push (make-balloon x) balloons)
	 (decf x (1+ +balloon-width+)))
    balloons))

;; TODO : find a solution to simplify code because it's very obscure
(defun make-balloons-random-list (n)
  (let ((balloons nil)
	(j 1)
	(yellow-balloons-position (random-lst-number n 3)) ;;3 because there are 3 yellow balloons
	;; we multiply by 2 because there is a blank space 
	;; (of size +balloon-width+) between the last balloon and the board
	(x (- *video-width* (* 2 +balloon-width+))))
    (loop for i from 1 to n do
	 (if (member i yellow-balloons-position)
	     (push (make-balloon x 
				 :color :yellow 
				 :y  (+ *video-height* (random 100)) 
				 :speed (incf j)) balloons)
	     (push (make-balloon x 
				 :color :red 
				 :speed (random* 1 3) 
				 :y (+ *video-height* (random 100))) balloons))
	 (decf x (1+ +balloon-width+)))
    balloons))


(defmethod move* ((balloon balloon))
  (if (%alive-p balloon)
      (progn 
	(decf (%y balloon) (%speed balloon))
	(when (minusp (+ (%y balloon) (%height balloon)))
	  (setf (%y balloon) *video-height*)))
      (incf (%y balloon) (%speed balloon))))


