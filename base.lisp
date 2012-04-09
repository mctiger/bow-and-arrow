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

;;; BASE class

(defclass base ()
  ((x :initarg :x :accessor %x)
   (y :initarg :y :accessor %y)
   (speed :initform 1 :initarg :speed :accessor %speed)
   (width :initarg :width :accessor %width)
   (height :initarg :height :accessor %height)
   (discard :initform nil :reader   %discard-p)
   (alive :initform t :accessor %alive-p)))


(defgeneric colliding-p (base1 base2))
(defgeneric draw (base))
(defgeneric draw-image (base path))
(defgeneric move (base x y))
(defgeneric out-of-bounds-p (base))

(defmethod colliding-p (base1 base2)
  (let ((right1 (+ (%x base1) (%width base1)))
	(right2 (+ (%x base2) (%width base2)))
	(bottom1 (+ (%y base1) (%height base1)))
	(bottom2 (+ (%y base2) (%height base2)))
	(left1 (%x base1))
	(left2 (%x base2))
	(top1 (%y base1))
	(top2 (%y base2)))
    (not (or (< right1 left2)
	     (< bottom1 top2)
	     (> left1 right2)
	     (> top1 bottom2)))))

(defmethod draw-image (base path)
  (sdl:draw-surface-at-* (sdl:load-image path :alpha 0 :image-type :png) ; (alpha == 0) =>  transparency
			 (%x base) 
			 (%y base)))


(defmethod  move (base x y)
  (setf (%x base) x
	(%y base) y))


(defmethod out-of-bounds-p (base)
  (or (minusp (%x base))
      (minusp (%y base))
      (> (%x base) *video-width*)
      (> (%y base) *video-height*)))



(defun remove-if-fn-and-out-of-bounds (items fn)
  (remove-if #'(lambda (item) 
		 (and (funcall fn item)
		      (out-of-bounds-p item))) 
	     items))
