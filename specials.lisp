;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER-*-
;;; Copyright (c) 2011 Kaïraba Cissé, All Rights Reserved
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

;; TODO : replace defparamater by defcontant for some initilizations

(defparameter +title+ "Bow & Arrow")

(defparameter *speed-arrow* 10)

(defparameter *speed-balloon* 2)

;; TODO : find a better name for the following constant
(defparameter  *alist-level-arrows*
  '((1 . 15) (2 . 15) (3 . 15)))

(defvar *video-height*)
(defvar *video-width*)

(defparameter +images-directory+
  (merge-pathnames #P".bow-and-arrow/images/" (user-homedir-pathname)))

(defmacro def-image-path (variable name)
  `(defparameter ,variable 
		(merge-pathnames ,name +images-directory+)))

;; arrow
(def-image-path +path-image-arrow+ "arrow.png")
(defparameter +arrow-width+ 51)
(defparameter +arrow-height+ 5)


;; balloon
(def-image-path +path-image-balloon+ "balloon.png")

(def-image-path +path-image-balloon-dead+ "balloon_dead.png")

(def-image-path +path-image-balloon-yellow+
  "balloon_yellow.png")

(def-image-path +path-image-balloon-yellow-dead+
  "balloon_yellow_dead.png")

(defparameter +balloon-width+ 25)
(defparameter +balloon-height+ 39)

(defparameter +balloon-dead-width+ 10)
(defparameter +balloon-dead-width+ 45)



;; bird
(def-image-path +path-image-bird+
  "bird.png")
(defparameter +bird-width+ 27)
(defparameter +bird-height+ 21)


;; bulls-eye
(def-image-path +path-image-bulls-eye+
  "bulls_eye.png")
(defparameter +bulls-eye-width+ 17)
(defparameter +bulls-eye-height+ 40)


;; butterfly
(def-image-path +path-image-butterfly+
  "butterfly.png")
(defparameter +butterfly-width+ 18)
(defparameter +butterfly-height+ 19)

(def-image-path +path-image-butterfly-bubled+
  "butterfly_bubled.png")
(defparameter +butterfly-bubled-width+ 30)
(defparameter +butterfly-bubled-height+ 32)


;; fire
(def-image-path +path-image-fire1+
  "fire1.png")
(defparameter +fire1-width+ 49)
(defparameter +fire1-height+ 20)

(def-image-path +path-image-fire2+
  "fire2.png")
(defparameter +fire2-width+ 49)
(defparameter +fire2-height+ 20)

(def-image-path +path-image-fire-dead+
  "fire_dead.png")
(defparameter +fire-dead-width+ 52)
(defparameter +fire-dead-height+ 20)


;; hero
(def-image-path +path-image-hero-armed+
  "hero_armed.png")
(defparameter +hero-armed-width+ 99)
(defparameter +hero-armed-height+ 105)

(def-image-path +path-image-hero-stand+
  "hero_stand.png")
(defparameter +hero-stand-width+ 128)
(defparameter +hero-stand-height+ 106)

(def-image-path +path-image-hero-without-arrow+
  "hero_without_arrow.png")
(defparameter +hero-without-arrow-width+ 94)
(defparameter +hero-without-arrow-height+ 106)

;; let x and y, the position of hero. let w and h, the width and 
;; height of hero. So, (x+w) and (y+40) is the position of the arrow
;; when the hero shoots an arrow 
;; 40 is the position of the arrow in the image hero_armed.png
(defparameter +arrow-position-regarding-hero+ 40)

;; paper
(def-image-path +path-image-paper+
  "paper.png")
(defparameter +paper-width+ 286)
(defparameter +paper-height+ 220)


;; slime
(def-image-path +path-image-slime+
  "slime.png")
(defparameter +slime-width+ 39)
(defparameter +slime-height+ 49)

(def-image-path +path-image-slime-dead+
  "slime_dead.png")
(defparameter +slime-width+ 40)
(defparameter +slime-height+ 49)


;; vulture
(def-image-path +path-image-vulture1+
  "vulture1.png")
(defparameter +vulture1-width+ 55)
(defparameter +vulture1-height+ 51)

(def-image-path +path-image-vulture2+
  "vulture2.png")
(defparameter +vulture2-width+ 55)
(defparameter +vulture2-height+ 34)

(def-image-path +path-image-vulture-dead+
  "vulture_dead.png")
(defparameter +vulture-dead-width+ 55)
(defparameter +vulture-dead-height+ 51)


;; wind
(def-image-path +path-image-wind1+
  "wind1.png")
(defparameter +wind1-width+ 30)
(defparameter +wind1-height+ 32)

(def-image-path +path-image-wind2+
  "wind2.png")
(defparameter +wind2-width+ 30)
(defparameter +wind2-height+ 32)

(def-image-path +path-image-wind-dead+
  "wind_dead.png")
(defparameter +wind-dead-width+ 37)
(defparameter +wind-dead-height+ 32)


(defvar *end-paper*
  '("No doubt, You are "
    ""
    "The GREATEST Archer"))

(defvar *copyright-paper* 
  '("Common Lisp - Bow and Arrow" 
    "version 1.2" 
    "" 
    "In search of" 
    "The GREATEST Archer"
    ""
    "A remake of the old Win95 game"
    ""
    "Made by :"
    ""
    "Kaïraba Cissé"
    "ckairaba@gmail.com"))

(defvar *level-one-paper*
  '("Target Practice" 
    "Our journey begins on the target range." 
    "" 
    "Target launchers release balloons up" 
    "range for the archers to shoot. Your" 
    "task -"
    "Shoot all the balloons."))

(defvar *level-two-paper*
  '("More Target Practice"
    ""
    "Nice shooting !"
    ""
    "The only way to become a great archer"
    "is to practice. After all, practice"
    "make perfect. Now it gets a little"
    "tougher."
    "Only shoot the RED balloons."))


(defvar *level-three-paper*
  '("Bouncing Bubbles"
    ""
    "Having had enough target practice for"
    "one day, you take a walk. In a small"
    "glade you spy a number of butterflies"
    "imprisoned by bubbles. Taking pity on"
    "the little creatures, you decide to free"
    "them ..."))

(defvar *level-four-paper*
  '("SLIMED"
    ""
    "The greatful butterflies tell of an evil"
    "imprisonment spell cast by The Black"
    "Archer, The greatest archer in all the"
    "Land !"
    "Greatest ? Hah ! you snicker. The Quest"
    "begins. In you path : the SWAMP ..."))

(defvar *end-of-level-four-paper*
  '("You've been Slimed"
    ""
    "The End"))
    

(defvar *hero-without-arrows-paper*
  '("What is an archer without his arrows ?"
    "He doesn't continue in this Game."
    ""
    "The End"))





