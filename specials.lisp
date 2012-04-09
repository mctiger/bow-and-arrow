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

(define-constant +license+
    ";;; Welcome to Bow & Arrow - A remake of W* 1995 game
;;; Written by Kaïraba Cissé <ckairaba@gmail.com>")

(define-constant +title+ "Bow & Arrow")

(define-constant *speed-arrow* 10)

(define-constant *speed-balloon* 2)

;; TODO : find a better name for the following constant
(define-constant  *alist-level-arrows*
    '((1 . 15) (2 . 15) (3 . 15)))

(defvar *video-height*)
(defvar *video-width*)

(define-constant +images-directory+
    (merge-pathnames #P"images/"  (asdf:system-source-directory 'bow-and-arrow)))

(defmacro def-image-path (variable name)
  `(define-constant ,variable 
       (merge-pathnames ,name +images-directory+)))

;; arrow
(def-image-path +path-image-arrow+ "arrow.png")
(define-constant +arrow-width+ 51)
(define-constant +arrow-height+ 5)


;; balloon
(def-image-path +path-image-balloon+ "balloon.png")

(def-image-path +path-image-balloon-dead+ "balloon_dead.png")

(def-image-path +path-image-balloon-yellow+
    "balloon_yellow.png")

(def-image-path +path-image-balloon-yellow-dead+
    "balloon_yellow_dead.png")

(define-constant +balloon-width+ 25)
(define-constant +balloon-height+ 39)

(define-constant +balloon-dead-width+ 10)
(define-constant +balloon-dead-width+ 45)



;; bird
(def-image-path +path-image-bird+
    "bird.png")
(define-constant +bird-width+ 27)
(define-constant +bird-height+ 21)


;; bulls-eye
(def-image-path +path-image-bulls-eye+
    "bulls_eye.png")
(define-constant +bulls-eye-width+ 17)
(define-constant +bulls-eye-height+ 40)


;; butterfly
(def-image-path +path-image-butterfly+
    "butterfly.png")
(define-constant +butterfly-width+ 18)
(define-constant +butterfly-height+ 19)

(def-image-path +path-image-butterfly-bubled+
    "butterfly_bubled.png")
(define-constant +butterfly-bubled-width+ 30)
(define-constant +butterfly-bubled-height+ 32)


;; fire
(def-image-path +path-image-fire1+
    "fire1.png")
(define-constant +fire1-width+ 49)
(define-constant +fire1-height+ 20)

(def-image-path +path-image-fire2+
    "fire2.png")
(define-constant +fire2-width+ 49)
(define-constant +fire2-height+ 20)

(def-image-path +path-image-fire-dead+
    "fire_dead.png")
(define-constant +fire-dead-width+ 52)
(define-constant +fire-dead-height+ 20)


;; hero
(def-image-path +path-image-hero-armed+
    "hero_armed.png")
(define-constant +hero-armed-width+ 99)
(define-constant +hero-armed-height+ 105)

(def-image-path +path-image-hero-stand+
    "hero_stand.png")
(define-constant +hero-stand-width+ 128)
(define-constant +hero-stand-height+ 106)

(def-image-path +path-image-hero-without-arrow+
    "hero_without_arrow.png")
(define-constant +hero-without-arrow-width+ 94)
(define-constant +hero-without-arrow-height+ 106)

;; let x and y, the position of hero. let w and h, the width and 
;; height of hero. So, (x+w) and (y+40) is the position of the arrow
;; when the hero shoots an arrow 
;; 40 is the position of the arrow in the image hero_armed.png
(define-constant +arrow-position-regarding-hero+ 40)

;; paper
(def-image-path +path-image-paper+
    "paper.png")
(define-constant +paper-width+ 286)
(define-constant +paper-height+ 220)


;; slime
(def-image-path +path-image-slime+
    "slime.png")
(define-constant +slime-width+ 39)
(define-constant +slime-height+ 49)

(def-image-path +path-image-slime-dead+
    "slime_dead.png")
(define-constant +slime-width+ 40)
(define-constant +slime-height+ 49)


;; vulture
(def-image-path +path-image-vulture1+
    "vulture1.png")
(define-constant +vulture1-width+ 55)
(define-constant +vulture1-height+ 51)

(def-image-path +path-image-vulture2+
    "vulture2.png")
(define-constant +vulture2-width+ 55)
(define-constant +vulture2-height+ 34)

(def-image-path +path-image-vulture-dead+
    "vulture_dead.png")
(define-constant +vulture-dead-width+ 55)
(define-constant +vulture-dead-height+ 51)


;; wind
(def-image-path +path-image-wind1+
    "wind1.png")
(define-constant +wind1-width+ 30)
(define-constant +wind1-height+ 32)

(def-image-path +path-image-wind2+
    "wind2.png")
(define-constant +wind2-width+ 30)
(define-constant +wind2-height+ 32)

(def-image-path +path-image-wind-dead+
    "wind_dead.png")
(define-constant +wind-dead-width+ 37)
(define-constant +wind-dead-height+ 32)


(define-constant *end-paper*
    '("No doubt, You are "
      ""
      "The GREATEST Archer"))

(define-constant *copyright-paper* 
    '("Common Lisp - Bow and Arrow" 
      "version 1.3" 
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

(define-constant *level-one-paper*
    '("Target Practice" 
      "Our journey begins on the target range." 
      "" 
      "Target launchers release balloons up" 
      "range for the archers to shoot. Your" 
      "task -"
      "Shoot all the balloons."))

(define-constant *level-two-paper*
    '("More Target Practice"
      ""
      "Nice shooting !"
      ""
      "The only way to become a great archer"
      "is to practice. After all, practice"
      "make perfect. Now it gets a little"
      "tougher."
      "Only shoot the RED balloons."))


(define-constant *level-three-paper*
    '("Bouncing Bubbles"
      ""
      "Having had enough target practice for"
      "one day, you take a walk. In a small"
      "glade you spy a number of butterflies"
      "imprisoned by bubbles. Taking pity on"
      "the little creatures, you decide to free"
      "them ..."))

(define-constant *level-four-paper*
    '("SLIMED"
      ""
      "The greatful butterflies tell of an evil"
      "imprisonment spell cast by The Black"
      "Archer, The greatest archer in all the"
      "Land !"
      "Greatest ? Hah ! you snicker. The Quest"
      "begins. In you path : the SWAMP ..."))

(define-constant *end-of-level-four-paper*
    '("You've been Slimed"
      ""
      "The End"))


(define-constant *hero-without-arrows-paper*
    '("What is an archer without his arrows ?"
      "He doesn't continue in this Game."
      ""
      "The End"))





