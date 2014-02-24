;;; Copyright (c) 2014, Georg Bartels <georg.bartels@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;; * Neither the name of the Institute for Artificial Intelligence/
;;; Universitaet Bremen nor the names of its contributors may be used to
;;; endorse or promote products derived from this software without specific
;;; prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-feature-constraints)

(defgeneric equal-p (a b))

(defmethod equal-p ((a cl-transforms:3d-vector) (b cl-transforms:3d-vector))
  (or (eq a b)
      (and
       (= (cl-transforms:x a) (cl-transforms:x b))
       (= (cl-transforms:y a) (cl-transforms:y b))
       (= (cl-transforms:z a) (cl-transforms:z b)))))

(defmethod equal-p ((a geometric-feature) (b geometric-feature))
  (or (eq a b)
      (and
       (string= (id a) (id b))
       (string= (frame-id a) (frame-id b))
       (eql (feature-type a) (feature-type b))
       (equal-p (origin a) (origin b))
       (equal-p (orientation a) (orientation b)))))
      
(defmethod equal-p ((a feature-relation) (b feature-relation))
  (or (eq a b)
      (and
       (string= (id a) (id b))
       (string= (reference a) (reference b))
       (eql (function-type a) (function-type b))
       (equal-p (tool-feature a) (tool-feature b))
       (equal-p (object-feature a) (object-feature b)))))

(defmethod equal-p ((a feature-constraint) (b feature-constraint))
  (or (eq a b)
      (and
       (string= (id a) (id b))
       (= (lower-boundary a) (lower-boundary b))
       (= (upper-boundary a) (upper-boundary b))
       (equal-p (relation a) (relation b)))))

(defmethod equal-p ((a feature-constraint-state) (b feature-constraint-state))
  (or (eq a b)
      (and
       (string= (constraint-id a) (constraint-id b))
       (= (output a) (output b))
       (= (ctrl-output a) (ctrl-output b))
       (= (ctrl-weight a) (ctrl-weight b)))))