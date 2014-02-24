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
         (other-feature (copy-geometric-feature feature :id "pancake rim"))
         (other-feature2 (copy-geometric-feature feature :frame-id "l_arm_gripper"))
         (other-feature3 (copy-geometric-feature feature :feature-type 'point))
         (other-feature4 (copy-geometric-feature feature 
                                                 :origin (cl-transforms:make-identity-vector)))
         (other-feature5 (copy-geometric-feature feature
                                                 :orientation (cl-transforms:make-identity-vector))))
    (assert-true (equal-p feature feature2))
    (assert-false (equal-p feature other-feature))
    (assert-false (equal-p feature2 other-feature))
    (assert-false (equal-p feature other-feature2))
    (assert-false (equal-p feature other-feature3))
    (assert-false (equal-p feature other-feature4))
    (assert-false (equal-p feature other-feature5))))

(define-test feature-relation-equal-p ()
  (let* ((feature 
           (make-geometric-feature
            :id "spatula front edge"
            :frame-id "/spatula"
            :feature-type 'line
            :origin (cl-transforms:make-3d-vector 0.1 0.2 0.3)
            :orientation (cl-transforms:make-3d-vector -0.1 -0.2 -0.3)))
         (feature2
           (make-geometric-feature
            :id "pancake center"
            :frame-id "/kinect"
            :feature-type 'point
            :origin (cl-transforms:make-3d-vector 2.0 0.2 0.03)
            :orientation (cl-transforms:make-identity-vector)))
         (relation (make-feature-relation
                    :id "spatula front over pancake center"
                    :reference "torso_lift_link"
                    :function-type 'above
                    :tool-feature (copy-geometric-feature feature)
                    :object-feature (copy-geometric-feature feature2)))
         (relation2 (copy-feature-relation relation))
         (other-relation (copy-feature-relation relation :id "huhu"))
         (other-relation2 (copy-feature-relation relation :reference "there"))
         (other-relation3 (copy-feature-relation relation :function-type 'below))
         (other-relation4 
           (copy-feature-relation relation 
                                  :tool-feature (copy-geometric-feature feature2)))
         (other-relation5 
           (copy-feature-relation relation 
                                  :object-feature (copy-geometric-feature feature))))
    (assert-true (equal-p relation relation))
    (assert-true (equal-p relation relation2))
    (assert-false (equal-p relation other-relation))
    (assert-false (equal-p relation other-relation2))
    (assert-false (equal-p relation other-relation3))
    (assert-false (equal-p relation other-relation4))
    (assert-false (equal-p relation other-relation5))))

(define-test feature-constraint-equal-p ()
  (let* ((feature 
           (make-geometric-feature
            :id "spatula front edge"
            :frame-id "/spatula"
            :feature-type 'line
            :origin (cl-transforms:make-3d-vector 0.1 0.2 0.3)
            :orientation (cl-transforms:make-3d-vector -0.1 -0.2 -0.3)))
         (relation
           (make-feature-relation
            :id "some relation"
            :reference "here"
            :function-type 'behind
            :tool-feature (copy-geometric-feature feature)
            :object-feature (copy-geometric-feature feature :frame-id "somewhere")))
         (constraint
           (make-feature-constraint
            :id "some constraint"
            :relation (copy-feature-relation relation)
            :lower-boundary 0.0
            :upper-boundary 10.0))
         (constraint2 (copy-feature-constraint constraint))
         (other-constraint (copy-feature-constraint constraint :id "hehe"))
         (other-constraint2 
           (copy-feature-constraint constraint
                                    :relation (copy-feature-relation relation :id "unknown")))
         (other-constraint3 (copy-feature-constraint constraint :lower-boundary -2))
         (other-constraint4 (copy-feature-constraint constraint :upper-boundary 11)))
    (assert-true (equal-p constraint constraint))
    (assert-true (equal-p constraint constraint2))
    (assert-false (equal-p constraint other-constraint))
    (assert-false (equal-p constraint other-constraint2))
    (assert-false (equal-p constraint other-constraint3))
    (assert-false (equal-p constraint other-constraint4))))