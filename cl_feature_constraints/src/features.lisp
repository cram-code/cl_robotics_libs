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

;;; CLASS DEFINITION FOR GEOMETRIC FEATURES
;;;
;;; Geometric features are point features with varying orientation information.
;;; They are used to define geometric relations which in turn are the basis
;;; of the constraint-based motion constraints.
;;;
;;; Currently, three types of geometric features are supported: points, lines, and
;;; planes. All three have a 3D origin, while only lines and planes carry additional
;;; 3D orientation information (Implementation detail: As they all share the same data
;;; data structure, points also have orientation information which is ignored.).
;;; Both origin and orientation are defined w.r.t a TF frame.

(defclass geometric-feature ()
  ((id :initarg :id :accessor id :type string
       :documentation "ID used for identification, possibly human-readable.")
   (frame-id :initarg :frame-id :accessor frame-id :type string
             :documentation "TF frame w.r.t. the feature is defined.")
   (feature-type :initarg :feature-type :accessor feature-type :type symbol
                 :documentation "Symbol denoting type of feature: POINT/LINE/PLANE")
   (origin :initarg :origin :accessor origin :type cl-transforms:3d-vector
             :documentation "3d-vector representing feature origin w.r.t. frame-id.")
   (orientation :initarg :orientation :accessor orientation :type cl-transforms:3d-vector
              :documentation "3d-vector representing feature orientation w.r.t. frame-id."))
  (:documentation "A geometric feature used for modelling motion with motion constraints"))

;;; DEFAULT VALUES FOR GEOMETRIC FEATURES
;;;
;;; Some same defaults used to convenience create functions to make sure the slots
;;; of geometric features are bound.

(defparameter *default-feature-id* ""
  "Default ID of geometric features.")

(defparameter *default-feature-frame-id* ""
  "Default frame-id of geometric features.")

(defparameter *default-feature-type* :UNKNOWN-FEATURE-TYPE
  "Default type of geometric features.")

(defparameter *default-feature-origin* (cl-transforms:make-identity-vector)
  "Default origin of geometric features.")

(defparameter *default-feature-orientation* (cl-transforms:make-identity-vector)
  "Default orientation of geometric features.")

(defparameter *vaild-feature-types* (list :point :line :plane)
  "List of symbols denoting valid feature types.")

;;; CONVENIENCE FUNCTIONS

(defun valid-feature-types ()
  "Returns a list of symbols denoting valid types of geometric features."
  *vaild-feature-types*)

(defun valid-feature-type-symbol-p (feature-type)
  "Checks whether the symbol `feature-type' denotes a valid geometric feature. Returns
 `feature-type' if yes, else 'nil'."
  (declare (type symbol feature-type))
  (find feature-type (valid-feature-types)))

(defun valid-feature-type-p (feature)
  "Checks whether the geometric feature `feature' has a valid geometric feature type.
 Returns the feature type symbol of `feature', else 'nil'."
  (declare (type geometric-feature feature))
  (with-slots (feature-type) feature
    (valid-feature-type-symbol-p feature-type)))

(defun make-geometric-feature (&key (id *default-feature-id*) 
                                 (frame-id *default-feature-frame-id*)
                                 (feature-type *default-feature-type*)
                                 (origin *default-feature-origin*)
                                 (orientation *default-feature-orientation*)
                                 (validate-args *default-validation-level*))
  "Creates an instance of type 'geometric-feature' filling it with the content provided in
 the parameters. If not specified as params, slots are bound to defaults with correct type.
 
 `validate-args' is flag enabling a check for a correct feature type: nil will deactivate
 checking, :warn will cause a warning to be thrown if an incorrect type was given, and 
 everything else will cause an error in case of invalid types."
  (declare (type string id frame-id)
           (type symbol feature-type)
           (type cl-transforms:3d-vector origin orientation))
  (make-instance 'geometric-feature
                 :id id :frame-id frame-id :feature-type feature-type
                 :origin origin :orientation orientation
                 :validate-args validate-args))

(defmethod initialize-instance :after ((feature geometric-feature) 
                                       &key (validate-args *default-validation-level*))
  ;; possibly checking/enforcing that geometric features have correct type
  (when validate-args
    (unless (valid-feature-type-p feature)
      (with-slots (id feature-type) feature
        (if (eq validate-args :warn)
            (warn "Geometric feature '~a' initialized with invalid feature-type: ~a"
                  id feature-type)
            (error "Geometric feature '~a' initialized with invalid feature-type: ~a"
                  id feature-type))))))