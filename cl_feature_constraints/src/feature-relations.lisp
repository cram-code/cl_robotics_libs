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
;;; Some feature functions require a third argument: The point of view (or reference)
;;; from which they are evaluated. For example: Imagine a cup and a plate
;;; standing on a table. Whether the cup is left or right of the plate depends on the
;;; physical point of view of the observer. This reference is specified as a TF frame.

(defclass feature-relation ()
  ((id :initarg :id :accessor id :type string
       :documentation "ID used for identification, possibly human-readable.")
   (reference :initarg :reference :accessor reference :type string
              :documentation "TF frame w.r.t. the function is evaluated.")
   (function-type :initarg :function-type :accessor function-type :type symbol
                  :documentation "Symbol denoting 1D function relating two features.")
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

(defparameter *default-relation-function-type* :UNKNOWN-RELATION-FUNCTION
  "Default function symbol of feature relations.")

(defparameter *default-tool-feature* (make-geometric-feature :validate-args nil)
  "Default tool feature of feature relations.")

(defparameter *default-object-feature* (make-geometric-feature :validate-args nil)
  "Default object feature of feature relations.")

;;; CONVENIENCE FUNCTIONS

(defparameter *valid-relation-functions* 
  (list 'above 'below 'right 'left 'behind 'infront 'distance 'perpendicular 'pointing)
  "List of symbols denoting valid geometric relation functions understood by controller.")

(defun valid-relation-functions ()
  "Returns list of symbols denoting valid geometric relation functions understood by
 controller."
  *valid-relation-functions*)

(defun valid-relation-function-symbol-p (relation-type)
  "Checks whether the symbol `relation-type' denotes a valid geometric relation function.
 Returns `relation-type' if yes, else 'nil'."
  (declare (type symbol relation-type))
  (find relation-type (valid-relation-functions)))

(defun valid-relation-type-p (relation)
  "Checks whether the feature relation `relation' has a valid geometric function type.
 If yes, returns the function type symbol of `relation', else 'nil'."
  (declare (type feature-relation relation))
  (with-slots (function-type) relation
    (valid-relation-function-symbol-p function-type)))

(defun make-feature-relation (&key (id *default-relation-id*)
                                (reference *default-relation-reference*)
                                (function-type *default-relation-function-type*)
                                (tool-feature *default-tool-feature*)
                                (object-feature *default-object-feature*)
                                (validate-args *default-validation-level*))
 "Creates an instance of type 'feature-relation' filling it with the content provided in
 the parameters. If not specified as params, slots are bound to defaults with correct type."
  (declare (type string id reference)
           (type symbol function-type)
           (type geometric-feature tool-feature object-feature))
  (make-instance 
   'feature-relation
   :id id :reference reference :function-type function-type
   :tool-feature tool-feature :object-feature object-feature
   :validate-args validate-args))

(defmethod initialize-instance :after ((relation feature-relation) 
                                       &key (validate-args *default-validation-level*))
  ;; possibly checking/enforcing that feature relations have correct type
  (when validate-args
    (unless (valid-relation-type-p relation)
      (with-slots (id function-type) relation
        (if (eq validate-args :warn)
            (warn "Feature relation '~a' initialized with invalid function-type: ~a"
                  id function-type)
            (error "Feature relation '~a' initialized with invalid function-type: ~a"
                  id function-type))))))

(defun copy-feature-relation (relation &key id reference function-type 
                                         tool-feature object-feature)
  "Creates and returns a deep copy of `relation'. If any of the key-arguments are given,
 they are used to initialize the respective slot of the copied feature-relation."
  (with-slots ((old-id id) (old-reference reference) (old-function-type function-type)
               (old-tool-feature tool-feature) (old-object-feature object-feature))
      relation
    (make-feature-relation 
     :id (or id old-id)
     :reference (or reference old-reference)
     :function-type (or function-type old-function-type)
     :tool-feature (or tool-feature old-tool-feature)
     :object-feature (or object-feature old-object-feature))))

(defmethod print-object ((object feature-relation) stream)
  "Clumsy printing of an fccl feature relation."
  (print-unreadable-object (object stream :type t)
    (with-slots (id reference function-type tool-feature object-feature) object
        (format stream "~%id: ~s ~%reference: ~s ~%type: ~a ~%tool: ~a ~%object: ~a"
                id reference function-type tool-feature object-feature))))