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

;;; CLASS DEFINITION FOR FEATURE CONSTRAINTS
;;;
;;; Desired motion goals are specified using range constraints over geometric relations.
;;; 
;;; Implementation/design decisions: I could have gone with a relation-ID instead of the
;;; full-blown relation in the constraint representation, i.e. making it more database-
;;; style. However, in order to be more agressive in prototyping (and not care about
;;; storing and looking-up geometric relations) I chose to not do so. In a later and more
;;; mature implementation, it could be desirable to change to couple these data structures
;;; only through IDs.

(defclass feature-constraint ()
  ((id :initarg :id :accessor id :type string
       :documentation "ID used for identification, possibly human-readable.")
   (relation :initarg :relation :accessor relation :type geometric-relation
             :documentation "Feature relation we constrain.")
   (lower-boundary :initarg :lower-boundary :accessor lower-boundary :type number
                   :documentation "Desired lower boundary for the relation.")
   (upper-boundary :initarg :upper-boundary :accessor upper-boundary :type number
                   :documentation "Desired upper boundary for the relation."))
  (:documentation "Feature constraint expressing a desired geometric relation."))

;;; DEFAULT VALUES FOR FEATURE CONSTRAINTS
;;;
;;; Some same defaults used for convenience create function to make sure the slots
;;; of feature constraints are bound.

(defparameter *default-constraint-id* ""
  "Default ID of feature constraints.")

(defparameter *default-constraint-relation* (make-feature-relation :validate-args nil)
  "Default relation of feature constraints.")

(defparameter *default-constraint-lower* *neg-infinity*
  "Default lower boundary of feature constraints.")

(defparameter *default-constraint-upper* *pos-infinity*
  "Default upper boundary of feature constraints.")

(defun make-feature-constraint (&key (id *default-constraint-id*)
                                  (relation *default-constraint-relation*)
                                  (lower-boundary *default-constraint-lower*)
                                  (upper-boundary *default-constraint-upper*))
 "Creates an instance of type 'feature-constraint' filling it with the content provided in
 the parameters. If not specified as params, slots are bound to defaults with correct type."
  (declare (type string id)
           (type feature-relation relation)
           (type number lower-boundary upper-boundary))
  (make-instance 
   'feature-constraint
   :id id :relation relation :lower-boundary lower-boundary :upper-boundary upper-boundary))

(defun copy-feature-constraint (constraint &key id relation lower-boundary upper-boundary)
  "Creates and returns a deep copy of `constraint'. If any of the key-arguments are given,
 they are used to initialize the respective slot of the copied feature-constraint."
  (with-slots ((old-id id) (old-relation relation) (old-lower-boundary lower-boundary)
               (old-upper-boundary upper-boundary)) constraint
    (make-feature-constraint
     :id (or id old-id)
     :relation (or relation old-relation)
     :lower-boundary (or lower-boundary old-lower-boundary)
     :upper-boundary (or upper-boundary old-upper-boundary))))

(defmethod print-object ((object feature-constraint) stream)
  "Clumsy printing of an fccl feature constraint."
  (print-unreadable-object (object stream :type t)
    (with-slots (id relation lower-boundary upper-boundary) object
      (format stream "~%id: ~s ~%relation: ~a ~%lower-boundary: ~d ~% upper-boundary: ~d ~%"
              id relation lower-boundary upper-boundary))))
     
;;; CLASS DEFINITION FOR FEATURE CONSTRAINT STATES
;;;
;;; The controller reports the state of each constraint back for further analysis or
;;; reactions by the caller. As such the current output of the geometric relation, and
;;; the instantaneous desired output and effective weight of the constraint (as chosen
;;; by the controller) are fed back.

(defclass feature-constraint-state ()
  ((constraint-id :initarg :constraint-id :accessor constraint-id :type string
                  :documentation "Identifier of original feature constraint.")
   (output :initarg :output :accessor output :type number
           :documentation "Current output of feature relation.")
   (ctrl-output :initarg :ctrl-output :accessor ctrl-output :type number
               :documentation "Current controller set-point of feature constraint.")
   (ctrl-weight :initarg :ctrl-weight :accessor ctrl-weight :type number
                :documentation "Current weight of feature constraint by controller."))
  (:documentation "Current state of geometric constraint reported from controller."))

;;; DEFAULT VALUES FOR FEATURE CONSTRAINT STATES
;;;
;;; Some same defaults used for convenience create function to make sure the slots
;;; of feature constraint states are bound.

(defparameter *default-constraint-output* 0
  "Default output of feature constraint states.")

(defparameter *default-constraint-ctrl-output* 0
  "Default control output of feature constraint states.")

(defparameter *default-constraint-ctrl-weight* 0
  "Default control weight of feature constraint states.")

(defun make-feature-constraint-state (&key (constraint-id *default-constraint-id*)
                                        (output *default-constraint-output*)
                                        (ctrl-output *default-constraint-ctrl-output*)
                                        (ctrl-weight *default-constraint-ctrl-weight*))
 "Creates an instance of type 'feature-constraint' filling it with the content provided in
 the parameters. If not specified as params, slots are bound to defaults with correct type."
  (declare (type string constraint-id)
           (type number output ctrl-output ctrl-weight))
  (make-instance 
   'feature-constraint-state
   :constraint-id constraint-id :output output :ctrl-output ctrl-output
   :ctrl-weight ctrl-weight))

(defun copy-feature-constraint-state (state &key constraint-id output
                                              ctrl-output ctrl-weight)
  "Creates and returns a deep copy of `state'. If any of the key-arguments are given, they
 are used to initialize the respective slot of the copied feature-constraint-state."
  (declare (type feature-constraint-state state))
  (with-slots ((old-constraint-id constraint-id) (old-output output)
               (old-ctrl-output ctrl-output) (old-ctrl-weight ctrl-weight)) state
    (make-feature-constraint-state
     :constraint-id (or constraint-id old-constraint-id)
     :output (or output old-output)
     :ctrl-output (or ctrl-output old-ctrl-output)
     :ctrl-weight (or ctrl-weight old-ctrl-weight))))

(defun feature-constraint-fulfilled-p (constraint-state)
  (declare (type feature-constraint-state constraint-state))
  (< (ctrl-weight constraint-state) 1.0))