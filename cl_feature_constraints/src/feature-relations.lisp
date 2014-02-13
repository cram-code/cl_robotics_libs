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
 
;;; CLASS DEFINITION FOR FEATURE RELATIONS
;;;
;;; We call two geometric feature which are related to one another by a 1D feature
;;; function 'feature relations'. As such they are a way of analysis geometric relations
;;; of geometric features. Motion goals are subsequently defined as desired outputs
;;; over feature relations.
;;;
;;; Some feature functions in fact require a third argument: The point of view (or
;;; reference) from which they are evaluated. For example: Imagine a cup and a plate
;;; standing on a table. Whether the cup is left or right of the plate depends on the
;;; physical point of view of the observer. This reference is specified as a TF frame.

(defclass feature-relation ()
  ((id :initarg :id :accessor id :type string
       :documentation "ID used for identification, possibly human-readable.")
   (reference :initarg :reference :accessor reference :type string
              :documentation "TF frame w.r.t. the function is evaluated.")
   (function :initarg :function :accessor feature-function :type symbol
             :documentation "1D function relating the two features.")
   (tool-feature :initarg :tool-feature :accessor tool-feature :type geometric-feature
                 :documentation "Geometric feature controlled by robot.")
   (object-feature :initarg :object-feature :accessor object-feature 
                   :type geometric-feature
                   :documentation "Geometric feature not necessarily controlled by robot."))
  (:documentation "Geometric relation between two geometric features."))

;;; DEFAULT VALUES FOR FEATURE RELATIONS
;;;
;;; Some same defaults used in convenience create functions to make sure the slots
;;; of feature relations are bound.

(defparameter *default-relation-id* ""
  "Default id of feature relations.")

(defparameter *default-relation-reference* ""
  "Default reference of feature relations.")

(defparameter *default-relation-function* :UNKNOWN-RELATION-FUNCTION
  "Default function symbol feature relations.")

(defparameter *default-tool-feature* (make-geometric-feature)
  "Default tool feature of feature relations.")

(defparameter *default-object-feature* (make-geometric-feature)
  "Default object feature of feature relations.")

;;; CONVENIENCE FUNCTIONS

(defun make-feature-relation (&key (id *default-relation-id*)
                                (reference *default-relation-reference*)
                                (function *default-relation-function*)
                                (tool-feature *default-tool-feature*)
                                (object-feature *default-object-feature*))
 "Creates an instance of type 'feature-relation' filling it with the content provided in
 the parameters. If not specified as params, slots are bound to defaults with correct type."
  (declare (type string id reference)
           (type symbol function)
           (geometric-feature tool-feature object-feature))
  (make-instance 
   'feature-relation
   :id id :reference reference :function function
   :tool-feature tool-feature :object-feature object-feature))