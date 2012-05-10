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

;;; Fire
(defstruct (fire (:include slime
			    (y (random (- *video-height* +fire-height+)))
			    (width +fire-width+)
			    (height +fire-height+)
			    (speed 5)))
  (state :fire1 :type symbol))

(defmethod draw ((fire fire))
  (if (%alive-p fire)
      (if (eq (fire-state fire) :fire1)
	  (progn
	    (draw-image fire +path-image-fire1+)
	    (setf (fire-state fire) :fire2))
	  (progn
	    (draw-image fire +path-image-fire2+)
	    (setf (fire-state fire) :fire1)))
      (draw-image fire +path-image-fire-dead+)))

(defun make-fires-random-list (n)
  (declare (type fixnum n))
  (let ((fires nil)
	(x *video-width*))
    (declare (type list fires))
    (loop repeat n do
	 (push (make-fire :x x) fires)
	 (incf x (* 2 +fire-width+)))
    (the list fires)))

