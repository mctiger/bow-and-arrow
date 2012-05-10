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

;;; Base
(defstruct base
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (speed 2 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (alive-p t :type boolean))

;; %x
(defmethod %x ((base base))
  (base-x base))
(defmethod (setf %x) (x (base base))
  (setf (base-x base) x))

;; %y
(defmethod %y ((base base))
  (base-y base))
(defmethod (setf %y) (y (base base))
  (setf (base-y base) y))

;; %speed
(defmethod %speed ((base base))
  (base-speed base))
(defmethod (setf %speed) (speed (base base))
  (setf (base-speed base) speed))

;; %width
(defmethod %width ((base base))
  (base-width base))
(defmethod (setf %width) (width (base base))
  (setf (base-width base) width))

;; %height
(defmethod %height ((base base))
  (base-height base))
(defmethod (setf %height) (height (base base))
  (setf (base-height base) height))

;; %alive-p
(defmethod %alive-p ((base base))
  (base-alive-p base))
(defmethod (setf %alive-p) (alive-p (base base))
  (setf (base-alive-p base) alive-p))

;; generics
(defgeneric colliding-p (base1 base2))
(defgeneric draw (base))
(defgeneric draw-image (base path))
(defgeneric move (base x y))
(defgeneric out-of-bounds-p (base))

;; methods
(defmethod colliding-p (base1 base2)
  (let ((right1 (+ (%x base1) (%width base1)))
	(right2 (+ (%x base2) (%width base2)))
	(bottom1 (+ (%y base1) (%height base1)))
	(bottom2 (+ (%y base2) (%height base2)))
	(left1 (%x base1))
	(left2 (%x base2))
	(top1 (%y base1))
	(top2 (%y base2)))
    (declare (type fixnum right1 right2 bottom1 bottom2 left1 left2 top1 top2))
    (the boolean (not (or (< right1 left2)
			  (< bottom1 top2)
			  (> left1 right2)
			  (> top1 bottom2))))))

(defmethod draw-image (base path)
  (sdl:draw-surface-at-* (sdl:load-image path
					 :alpha 0  ; (alpha == 0) =>  transparency
					 :image-type :png)
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

(defmethod negative-bounds-p (base)
  (or (minusp (%x base))
      (minusp (%y base))))

;; functions
(defun remove-if-fn-and-out-of-bounds (items fn)
  (let (tmp)
    (declare (type list tmp))
    (dolist (item items tmp)
      (when (not (and (funcall fn item) (out-of-bounds-p item)))
	(push item tmp)))))

(defun remove-if-fn-or-out-of-bounds (items fn)
  (let (tmp)
    (declare (type list tmp))
    (dolist (item items tmp)
      (when (not (or (funcall fn item) (out-of-bounds-p item)))
	(push item tmp)))))

(defun remove-if-fn-and-negative-bounds (items fn)
  (let (tmp)
    (declare (type list tmp))
    (dolist (item items tmp)
      (when (not (and (funcall fn item) (negative-bounds-p item)))
	(push item tmp)))))

(defun remove-if-fn-or-negative-bounds (items fn)
  (let (tmp)
    (declare (type list tmp))
    (dolist (item items tmp)
      (when (not (or (funcall fn item) (negative-bounds-p item)))
	(push item tmp)))))

