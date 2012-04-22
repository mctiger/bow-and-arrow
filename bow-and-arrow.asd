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

(defpackage :bow-and-arrow 
  (:nicknames :bow)
  (:use :common-lisp)
  (:export :play :+images-directory+ :+version+))

(defparameter bow-and-arrow:+version+ "1.4")

(asdf:defsystem :bow-and-arrow
  :description "a remake of W* 1995 game Bow & Arrow"
  :version bow-and-arrow:+version+
  :author "Kaïraba Cissé <ckairaba@gmail.com>"
  :licence "MIT"
  :depends-on (#:asdf #:lispbuilder-sdl #:lispbuilder-sdl-image)
  :serial t
  :components ((:file "tools")
	       (:file "specials")
	       (:file "base")
	       (:file "paper")
	       (:file "balloon")
	       (:file "arrow")
	       (:file "hero")
	       (:file "butterfly")
	       (:file "slime")
	       (:file "main")))



