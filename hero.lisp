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

;;; HERO class
;;; TODO : a great feature : hero with infinite number of arrows (-1)
(defclass hero (base)
  (;; state can be :without-arrow, :stand or :armed
   (state  :initform :without-arrow :initarg :state :accessor %state)
   ;; number of arrows that the hero can used
   (nb-arrows :initform 0 :initarg :nb-arrows :accessor %nb-arrows)
   ;; the level slot is is necessary ?
   (level :initform 1 :initarg :level :accessor %level)
   ;; the set of arrows shoots by this hero
   (arrows :initform nil :initarg :arrows :accessor %arrows)))


(defgeneric change-state (hero mouse-key))
(defgeneric remove-arrows-if-out-of-bounds (hero))
(defgeneric draw-and-move-shoot-arrows (hero))

(defmethod draw ((hero hero))
  (case (%state hero)
    (:without-arrow (draw-image hero +path-image-hero-without-arrow+))
    (:armed   (draw-image hero +path-image-hero-armed+))
    (:stand   (draw-image hero +path-image-hero-stand+))))

(defmethod change-state ((hero hero) mouse-key)
  (when (plusp (%nb-arrows hero))
    (case (%state hero)
      (:without-arrow
	  (when (eql mouse-key sdl:sdl-button-right)
	    (setf (%state hero) :stand)
	    (setf (%width hero) +hero-stand-width+)
	    (setf (%height hero) +hero-stand-height+)))
      (:stand
       (when (eql mouse-key sdl:sdl-button-left)
	 (setf (%state hero) :armed)
	 (setf (%width hero) +hero-armed-width+)
	 (setf (%height hero) +hero-armed-height+)))
      (:armed
       (when (eql mouse-key sdl:sdl-button-left)
	 ;; this order is important
	 (push (make-arrow hero) (%arrows hero))
	 (decf (%nb-arrows hero))
	 (setf (%width hero) +hero-without-arrow-width+)
	 (setf (%height hero) +hero-without-arrow-height+)
	 (setf (%state hero) :without-arrow))))))


;; TODO : extend this function for a better display

(defmethod move ((hero hero) x y)
  (let ((hero-height (%height hero)))
    (declare (type fixnum hero-height))
    (when (<= (+ y hero-height) 
	      (+ *video-height* 
		 (- hero-height +arrow-position-regarding-hero+)))
      (setf (%x hero) x
	    (%y hero) y))))


(defun  make-hero (level)
  (make-instance 'hero 
		 :x 0
		 :y 0 
		 :level level
		 :width +hero-without-arrow-width+ 
		 :height +hero-without-arrow-height+
		 :nb-arrows (cdr (assoc level *alist-level-arrows*))))


(defmethod remove-arrows-if-out-of-bounds ((hero hero))
  (let ((arrows (%arrows hero))
	(tmp nil))
    (declare (type list arrows)  (type list tmp))
    (dolist (arrow arrows)
      (unless (out-of-bounds-p arrow)
	(push arrow tmp)))
    (setf (%arrows hero) tmp))) 

(defmethod draw-and-move-shoot-arrows ((hero hero))
  (let ((arrows (%arrows hero)))
    (declare (type list arrows))
    (dolist (arrow arrows)
      (draw arrow)
      (move* arrow))))

