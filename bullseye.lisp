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

(defstruct (bullseye (:include base
			       (x (- *video-width* 50))
			       (y (random* 0 (- *video-height* +bullseye-height+)))
			       (width +bullseye-width+)
			       (height +bullseye-height+)
			       (speed 2)))
  ;; direction is the initial direction and can be :up :down
  (direction :up :type symbol)
  ;; arrows
  (arrows nil :type list))

;; %direction
(defmethod %direction ((bullseye bullseye))
  (bullseye-direction bullseye))
(defmethod (setf %direction) (direction (bullseye bullseye))
  (setf (bullseye-direction bullseye) direction))

;; %arrows
(defmethod %arrows ((bullseye bullseye))
  (bullseye-arrows bullseye))
(defmethod (setf %arrows) (arrows (bullseye bullseye))
  (setf (bullseye-arrows bullseye) arrows))


(defmethod draw ((bullseye bullseye))
  (draw-image bullseye +path-image-bullseye+)
  (let ((arrows (%arrows bullseye)))
    (dolist (arrow arrows)
      (draw arrow))))

(defmethod move* ((bullseye bullseye))
  (if (eql (%direction bullseye) :up)
      (progn 
	(decf (%y bullseye) (%speed bullseye))
	(dolist (arrow (%arrows bullseye))
	  (decf (%y arrow) (%speed bullseye)))
	(when (minusp (%y bullseye))
	  (setf (%direction bullseye) :down)))
      (progn 
	(incf (%y bullseye) (%speed bullseye))
	(dolist (arrow (%arrows bullseye))
	  (incf (%y arrow) (%speed bullseye)))		
	(when (> (+ (%y bullseye) (%height bullseye)) *video-height*)
	  (setf  (%direction bullseye) :up)))))

(defmethod reached-p ((bullseye bullseye) (arrow arrow))
  (let* ((height/2 (/ +bullseye-height+ 2))
	 (lower (+ (%y bullseye) (1- height/2)))
	 (upper (+ (%y bullseye) (1+ height/2)))
	 (y (+ (%y arrow) 2)))
    (<= lower y upper)))
    
    

