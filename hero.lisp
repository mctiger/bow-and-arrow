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

;;; Arrow
(defstruct (arrow (:include base
			    (width +arrow-width+)
			    (height +arrow-height+)
			    (speed *speed-arrow*))))
(defgeneric move* (arrow))
(defmethod draw ((arrow arrow))
  (draw-image arrow +path-image-arrow+))
(defmethod move* ((arrow arrow))
  (incf (%x arrow) (%speed arrow)))

(defmethod colliding-p ((base1 arrow)  base2)
  (let ((right1 (+ (%x base1) (%width base1)))
	(right2 (+ (%x base2) (%width base2)))
	(bottom1 (+ (%y base1) (- (%height base1) 2))) ;; ...
	(bottom2 (+ (%y base2) (%height base2)))
	(left1 (%x base1))
	(left2 (%x base2))
	(top1 (+ (%y base1) 2)) ;; ...
	(top2 (%y base2)))
    (declare (type fixnum right1 right2 bottom1 bottom2 left1 left2 top1 top2))
    (the boolean (not (or (< right1 left2)
			  (< bottom1 top2)
			  (> left1 right2)
			  (> top1 bottom2))))))


;;; Hero
(defstruct (hero (:include base 
			   (width +hero-without-arrow-width+)
			   (height +hero-without-arrow-height+)))
  ;; state can be :without-arrow, :stand or :armed
  (state :without-arrow :type symbol)
   ;; number of arrows that the hero can used
  (nb-arrows 0 :type fixnum)
  ;; the level
  (level 1 :type fixnum)
  ;; the set of arrows shoots by this hero
  (arrows nil :type list))

;; %state
(defmethod %state ((hero hero))
  (hero-state hero))
(defmethod (setf %state) (state (hero hero))
  (setf (hero-state hero) state))

;; %nb-arrows
(defmethod %nb-arrows ((hero hero))
  (hero-nb-arrows hero))
(defmethod (setf %nb-arrows) (nb-arrows (hero hero))
  (setf (hero-nb-arrows hero) nb-arrows))

;; %level
(defmethod %level ((hero hero))
  (hero-level hero))
(defmethod (setf %level) (level (hero hero))
  (setf (hero-level hero) level))

;; %arrows
(defmethod %arrows ((hero hero))
  (hero-arrows hero))
(defmethod (setf %arrows) (arrows (hero hero))
  (setf (hero-arrows hero) arrows))


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
	 (push (make-arrow :x (+ (%x hero) (%width hero)) 
			   :y (+ (%y hero) +arrow-position-regarding-hero+)) 
	       (%arrows hero))
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

(defmethod remove-arrows-if-out-of-bounds ((hero hero))
  (let ((arrows (%arrows hero))
	(tmp nil))
    (declare (type list arrows)  (type list tmp))
    (dolist (arrow arrows)
      (unless (or (out-of-bounds-p arrow) (not (%alive-p arrow)))
	(push arrow tmp)))
    (setf (%arrows hero) tmp))) 

(defmethod draw-and-move-shoot-arrows ((hero hero))
  (let ((arrows (%arrows hero)))
    (declare (type list arrows))
    (dolist (arrow arrows)
      (when (%alive-p arrow)
	(draw arrow)
	(move* arrow)))))
