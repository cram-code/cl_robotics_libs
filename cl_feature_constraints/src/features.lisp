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

(defclass geometric-feature ()
  ((name :initarg :name :accessor name :type string
         :documentation "Name used for identification, possibly human-readable.")
   (frame-id :initarg :frame-id :accessor frame-id :type string
             :documentation "TF frame w.r.t. the feature is defined.")
   (feature-type :initarg :feature-type :accessor feature-type :type symbol
                 :documentation "Symbol denoting type of feature: POINT/LINE/PLANE")
   (origin :initarg :origin :accessor origin :type cl-transforms:3d-vector
             :documentation "3d-vector representing feature origin w.r.t. frame-id.")
   (orientation :initarg :orientation :accessor orientation :type cl-transforms:3d-vector
              :documentation "3d-vector representing feature orientation w.r.t. frame-id."))
  (:documentation "A geometric feature used for modelling motion with motion constraints"))

(defun make-geometric-feature (&key (name *default-feature-name*) 
                                 (frame-id *default-feature-frame-id*)
                                 (feature-type *default-feature-type*)
                                 (origin *default-feature-origin*)
                                 (orientation *default-feature-orientation*))
  (declare (type string name frame-id)
           (type symbol feature-type)
           (type cl-transforms:3d-vector origin orientation))
  (make-instance 'geometric-feature
                 :name name :frame-id frame-id :feature-type feature-type
                 :origin origin :orientation orientation))

(defclass feature-constraint ()
  ((name :reader name :initarg :name)
   (feature-function :reader feature-function :initarg :feature-function)
   (tool-feature :reader tool-feature :initarg :tool-feature)
   (world-feature :reader world-feature :initarg :world-feature)
   (lower-boundary :reader lower-boundary :initarg :lower-boundary)
   (upper-boundary :reader upper-boundary :initarg :upper-boundary)
   (weight :reader weight :initarg :weight :initform 1.0) ;; get rid of this
   (maximum-velocity :reader maximum-velocity :initarg :maximum-velocity) ;; possibly get rid of this or change it into sth symbolic from Moritz ;)
   (minimum-velocity :reader minimum-velocity :initarg :minimum-velocity))) ;; get rid of this

(defclass feature-constraint-state () ;; this is ugly because it does not contain the constraints. but it will do for now.
  ((current-weights :reader current-weights :initarg :current-weights)
   (movement-id :reader movement-id :initarg :movement-id)))
