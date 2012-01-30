;;; -*- Mode: LISP -*-
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

(asdf:defsystem :bow-and-arrow
  :description "a remake of W* 1995 game Bow & Arrow"
  :version "1.0"
  :author "Kaïraba Cissé <ckairaba@gmail.com>"
  :licence "MIT"
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "packages")
	       (:file "specials"
		      :depends-on ("packages"))
	       (:file "tools"
		      :depends-on ("packages"))

	       (:file "base"
		      :depends-on ("specials"))
	       (:file "paper"
		      :depends-on ("base"))
	       (:file "balloon"
		      :depends-on ("base" "tools"))
	       (:file "arrow"
		      :depends-on ("base"))
	       (:file "hero"
		      :depends-on ("base"))
	       (:file "butterfly"
		      :depends-on ("base"))
	       (:file "main"
		      :depends-on ("packages"
				   "specials"
				   "base"
				   "paper"
				   "balloon"
				   "arrow"
				   "hero"))))



