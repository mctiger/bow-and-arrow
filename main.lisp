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

(defun initialize-fullscreen-dimensions nil
  (let ((dimensions (sdl:video-dimensions)))
    (setf *video-width* (aref dimensions 0)
	  *video-height* (aref dimensions 1))))

(defun level-1-or-2 (my-hero balloons)
  ;; remove arrows shoot by hero if they are out of bounds
  (remove-arrows-if-out-of-bounds my-hero)
  (draw my-hero)
  (draw-and-move-shoot-arrows my-hero)
  ;; collision 
  (mapcar #'(lambda (balloon)
	      (let ((arrows (%arrows my-hero)))
		(dolist (arrow  arrows nil)
		  (when (and (colliding-p balloon arrow) (%alive-p balloon))
		    (setf (%alive-p balloon) nil)))))
	  balloons)
  ;; remove dead and out of bounds balloons
  (setf balloons 
	(remove-if-fn-and-out-of-bounds balloons 
					#'(lambda (balloon) 
					    (not (%alive-p balloon)))))
  (mapcar #'draw balloons)
  (mapcar #'move* balloons)
  (values 
   ;; the hero has no arrow 
   (and (null (%arrows my-hero)) (zerop (%nb-arrows my-hero)))
   ;; every red balloon is dead
   (every #'(lambda (balloon) (or (not (%alive-p balloon)) (eql (%color balloon) :yellow)))
	  balloons)))



(defun level-3 (my-hero butterflies)
  ;; remove arrows shoot by hero if they are out of bounds
  (remove-arrows-if-out-of-bounds my-hero)
  (draw my-hero)
  (draw-and-move-shoot-arrows my-hero)
  ;; collision 
  (mapcar #'(lambda (butterfly)
	      (let ((arrows (%arrows my-hero)))
		(dolist (arrow  arrows nil)
		  (when (and (colliding-p butterfly arrow) (%bubled-p butterfly))
		    (setf (%bubled-p butterfly) nil)))))
	  butterflies)
  
  ;; remove unbubled and out of bounds balloons
  (setf butterflies 
	(remove-if-fn-and-out-of-bounds butterflies 
					#'(lambda (butterfly) (not (%bubled-p butterfly)))))
  (mapcar #'draw butterflies)
  (mapcar #'move* butterflies)
  (values 
   ;; the hero has no arrow 
   (and (null (%arrows my-hero)) (zerop (%nb-arrows my-hero)))
   ;; every butterfly is unbubled, we don't kill butterfly :)
   (every #'(lambda (butterfly) (not (%bubled-p butterfly)))
	  butterflies)))


(defun play (&key (fullscreen nil) width height)
  (format t '(:blue :normal) +license+)
  (force-output)
  (sdl:with-init (sdl:sdl-init-video)
    (cond (fullscreen 
	   (initialize-fullscreen-dimensions))
	  ((and width height)
	   (setf *video-width* width
		 *video-height* height))
	  (t 
	   (setf *video-width* 1000
		 *video-height* 720)))
    (let* (my-hero
	   balloons
	   butterflies
	   (state :copyright)
	   (level 1))
      (sdl:window   *video-width* *video-height*  :title-caption +title+ :icon-caption +title+)
      (sdl:show-cursor nil)
      (sdl:with-events ()
	(:quit-event () t)
	;; TODO : use key-down-event to save the current game state (serialization)
	(:key-down-event (:key key)
			 (when (equal key :sdl-key-q)
			   (sdl:push-quit-event))
			 (when (equal key :sdl-key-space)
			   (cond ((eq state :paused) (setf state :play))
				 ((eq state :play) (setf state :paused)))))
	(:mouse-button-down-event (:button button)
				  (case state 
				    (:copyright (setf state :level))
				    (:level (setf state :play))
				    (:play (change-state my-hero button))
				    (:no-arrows (setf state :copyright))
				    (:end (sdl:push-quit-event))))
	(:mouse-button-up-event (:button button)
				(when (eq state :play)
				  (change-state my-hero button)))
	(:mouse-motion-event (:y y)
			     (when (eq state :play)
			       (move my-hero 0 y)))
	(:idle ()	       
	       (unless (eq state :paused)
		 ;; initialize a green background
		 (sdl:clear-display sdl:*green*)
		 (case state
		   (:copyright  (draw-copyright-paper))
		   (:end  (draw-end-paper))
		   (:no-arrows  (draw-no-more-arrows-paper))
		   (:level 
		    (draw-level-paper level)
		    ;; on each level, we create a new hero
		    (setf my-hero (make-hero level))
		    (case level
		      ;; there are 15 balloons at level 1
		      (1 (setq balloons (make-balloons-list 15)))
		      ;; there are 15 balloons at level 2 (12 red and 3 yellow)
		      (2 (setq balloons (make-balloons-random-list 15)))
		      ;; there are 15 bubled butterflies at level 3
		      (3 (setq butterflies (make-butterflies-random-list 15)))))
		   (:play
		    (sdl:clear-display sdl:*green*)
		    (case level
		      ((1 2 3)
		       (multiple-value-bind (any-arrow any-item-alive)
			   (case level
			     ((1 2) (level-1-or-2 my-hero balloons))
			     (3 (level-3 my-hero butterflies)))
			 (cond ((and any-arrow (not any-item-alive))
				(setq state :no-arrows
				      level 1))
			       ((or
				 (and any-arrow any-item-alive) 
				 ;; hero has some arrows and each balloon is dead
				 (and (not any-arrow) any-item-alive))
				(setq state :level)
				(incf level)))))
		      (otherwise (setf state :end
				       level 1))))))
	       (sdl:update-display))))))


