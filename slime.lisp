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

(defstruct slime
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (speed 1 :type fixnum)
  (width +slime-width+ :type fixnum)
  (height +slime-height+ :type fixnum)
  (discard nil :type boolean)
  (alive-p t :type boolean))



(defmethod colliding-p ((slime slime) base2)
  (let ((right1 (+ (slime-x slime) (slime-width slime)))
	(right2 (+ (%x base2) (%width base2)))
	(bottom1 (+ (slime-y slime) (slime-height slime)))
	(bottom2 (+ (%y base2) (%height base2)))
	(left1 (slime-x slime))
	(left2 (%x base2))
	(top1 (slime-y slime))
	(top2 (%y base2)))
    (not (or (< right1 left2)
	     (< bottom1 top2)
	     (> left1 right2)
	     (> top1 bottom2)))))


(defmethod draw-image ((slime slime) path)
  (sdl:draw-surface-at-* (sdl:load-image path :alpha 0 :image-type :png) ; (alpha == 0) =>  transparency
			 (slime-x slime) 
			 (slime-y slime)))


(defmethod  move ((slime slime) x y)
  (setf (slime-x slime) x
	(slime-y slime) y))


(defmethod out-of-bounds-p ((slime slime))
  (or (minusp (slime-x slime))
      (minusp (slime-y slime))
      (> (slime-x slime) *video-width*)
      (> (slime-y slime) *video-height*)))

(defmethod negative-bounds-p ((slime slime))
  (or (minusp (slime-x slime))
      (minusp (slime-y slime))))
  

(defmethod draw ((slime slime))
  (if (slime-alive-p slime)
      (draw-image slime +path-image-slime+)
      (draw-image slime +path-image-slime-dead+)))


(defun make-slime* (&key 
		    (x *video-width*) 
		    (y (random (- *video-height* +slime-height+))) 
		    (speed 3))
  (make-slime :x x :y y :speed speed))


(defmethod move* ((slime slime))
  (when (slime-alive-p slime)
    (decf (slime-x slime) (slime-speed slime))))


(defun make-slimes-random-list (n)
  (let ((slimes nil)
	(x *video-width*))
    (declare (type list slimes))
    (loop repeat n do
	 (push (make-slime* :x x) slimes)
	 (incf x (* 2 +slime-width+)))
    (the list slimes)))

