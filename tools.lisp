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

(defun random-lst-number (n &optional (len 1))
  (let (lst)
    (loop while (< (length lst) len)
       do  (let ((r (random (1+ n))))
	     (when (and (/= r 0) (not (member r lst)))
	       (push r lst))))
    lst))

(defun random* (i j)
  (+ i (random (1+ (- j i)))))


;; from sbcl manual http://www.sbcl.org/manual/Defining-Constants.html
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;; some parts of the following code comes from :
;; https://github.com/sile/cl-mine/blob/master/console.lisp

(deftype color () '(member :black :red :green :yellow :blue :magenta :cyan :white :normal))

(defparameter +escape+ (common-lisp:format nil "~c[" (code-char #8r33)))

(defun color-code (color)
  (declare (color color))
  (ecase color 
    (:black   30)
    (:red     31)
    (:green   32)
    (:yellow  33)
    (:blue    34)
    (:magenta 35)
    (:cyan    36)
    (:white   37)
    (:normal  39)))

(defun style (x &key (color :normal) (bgcolor :normal) bold inverse underline)
  (declare (color color bgcolor))
  (common-lisp:format nil "~a~{~d;~}~d;~dm~a~a0m"
		      +escape+
		      (remove nil (list (and bold 1) (and underline 4) (and inverse 7)))
		      (color-code color)
		      (+ (color-code bgcolor) 10)
		      x
		      +escape+))


(defmacro format (stream style control-string &rest format-arguments)
  (let ((cddr-style (gensym)))
    `(progn
       (assert (listp ,style))
       (if (null ,style)
	   (common-lisp:format ,stream ,control-string ,@format-arguments)
	   (let ((,cddr-style (cddr ,style)))
	     (common-lisp:format ,stream 
				 (style (common-lisp:format nil
							    ,control-string ,@format-arguments)
					:color (first ,style)
					:bgcolor (second ,style)
					:bold (member :bold ,cddr-style)
					:inverse (member :inverse ,cddr-style)
					:underline (member :underline ,cddr-style))))))))


