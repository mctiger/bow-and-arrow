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

(defstruct (butterfly (:include base
				(width +butterfly-bubled-width+)
				(height  +butterfly-bubled-height+)))
  ;; direction is the initial direction and can be :up :down
  (direction :up :type symbol)
  ;; bubled-p return t if this butterfly is bubled otherwise nil
  (bubled-p t :type boolean))

;; %direction
(defmethod %direction ((butterfly butterfly))
  (butterfly-direction butterfly))
(defmethod (setf %direction) (direction (butterfly butterfly))
  (setf (butterfly-direction butterfly) direction))

;; %bubled-p
(defmethod %bubled-p ((butterfly butterfly))
  (butterfly-bubled-p butterfly))
(defmethod (setf %bubled-p) (bubled-p (butterfly butterfly))
  (setf (butterfly-bubled-p butterfly) bubled-p))

(defmethod draw ((butterfly butterfly))
  (if (%bubled-p butterfly)
      (draw-image butterfly +path-image-butterfly-bubled+)
      (draw-image butterfly +path-image-butterfly+)))

(defmethod move* ((butterfly butterfly))
  (if (%bubled-p butterfly)
      (if (eql (%direction butterfly) :up)
	  (progn 
	    (decf (%y butterfly) (%speed butterfly))
	    (when (minusp (+ (%y butterfly) (%height butterfly)))
	      (setf (%direction butterfly) :down)))
	  (progn 
	    (incf (%y butterfly) (%speed butterfly))
	    (when (> (%y butterfly) *video-height*)
	      (setf  (%direction butterfly) :up))))
      (progn 
	(decf (%y butterfly) (%speed butterfly))
	(decf (%x butterfly) (%speed butterfly)))))


(defun make-butterflies-random-list (n)
  (let ((butterflies nil)
	;; we multiply by 2 because there is a blank space 
	;; (of size +butterfly-bubled-width+) between the last butterfly and the board
	(x (- *video-width* (* 2 +butterfly-bubled-width+))))
    (loop repeat n do
	 (push (make-butterfly :x x 
			       :y (random *video-height*) 
			       :direction (if (zerop (random 2)) :up :down))
	       butterflies)
	 (decf x (1+ +butterfly-bubled-width+)))
    butterflies))

