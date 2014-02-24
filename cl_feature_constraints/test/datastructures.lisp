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

(in-package :cl-feature-constraints-tests)

(define-test feature-equal-p ()
  (let* ((feature 
           (make-geometric-feature
            :id "spatula front edge"
            :frame-id "/spatula"
            :feature-type 'line
            :origin (cl-transforms:make-3d-vector 0.1 0.2 0.3)
            :orientation (cl-transforms:make-3d-vector -0.1 -0.2 -0.3)))
         (feature2 (copy-geometric-feature feature))
         (other-feature (copy-geometric-feature feature))
         (other-feature2 (copy-geometric-feature feature))
         (other-feature3 (copy-geometric-feature feature))
         (other-feature4 (copy-geometric-feature feature))
         (other-feature5 (copy-geometric-feature feature)))
    ;; changing one slot in each 'other' feature
    (setf (id other-feature) "pancake rim")
    (setf (frame-id other-feature2) "l_arm_gripper")
    (setf (feature-type other-feature3) 'point)
    (setf (origin other-feature4) (cl-transforms:make-identity-vector))
    (setf (orientation other-feature5) (cl-transforms:make-identity-vector))
    (assert-true (equal-p feature feature2))
    (assert-false (equal-p feature other-feature))
    (assert-false (equal-p feature2 other-feature))
    (assert-false (equal-p feature other-feature2))
    (assert-false (equal-p feature other-feature3))
    (assert-false (equal-p feature other-feature4))
    (assert-false (equal-p feature other-feature5))))