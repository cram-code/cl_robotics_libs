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

(in-package :robot-models)

(defclass joint-state ()
  ((joint-name :initarg :joint-name :accessor joint-name
               :documentation "ID used to identify the joint in a robot model")
   (joint-position :initarg :joint-position :accessor joint-position
             :documentation "Position of the robot joint.")
   (joint-velocity :initarg :joint-velocity :accessor joint-velocity
             :documentation "Velocity of the robot joint.")
   (joint-acceleration :initarg :joint-acceleration :accessor joint-acceleration
                 :documentation "Acceleration of the robot joint.")
   (joint-effort :initarg :joint-effort :accessor joint-effort
           :documentation "Effort of the robot joint."))
  (:documentation "Representation of the state of a single robot joint."))

(declaim (inline equal-joint-state-semantics-p))
(defun equal-joint-state-semantics-p (state-a state-b)
  (declare (type joint-state state-a state-b))
  (string= (joint-name state-a) (joint-name state-b)))

(defun calculate-position-delta (current-state desired-state)
  (declare (type joint-state current-state desired-state))
  (assert (equal-joint-state-semantics-p current-state desired-state))
  (- (joint-position desired-state) (joint-position current-state)))

(defun calculate-velocity-delta (current-state desired-state)
  (declare (type joint-state current-state desired-state))
  (assert (equal-joint-state-semantics-p current-state desired-state))
  (- (joint-velocity desired-state) (joint-velocity current-state)))

(defun calculate-acceleration-delta (current-state desired-state)
  (declare (type joint-state current-state desired-state))
  (assert (equal-joint-state-semantics-p current-state desired-state))
  (- (joint-acceleration desired-state) (joint-acceleration current-state)))

(defun calculate-effort-delta (current-state desired-state)
  (declare (type joint-state current-state desired-state))
  (assert (equal-joint-state-semantics-p current-state desired-state))
  (- (joint-effort desired-state) (joint-effort current-state)))

(defun calculate-state-delta (current-state desired-state)
  (declare (type joint-state current-state desired-state))
  (assert (equal-joint-state-semantics-p current-state desired-state))
  (make-instance
   'joint-state
   :joint-name (joint-name current-state)
   :joint-position (calculate-position-delta current-state desired-state)
   :joint-velocity (calculate-velocity-delta current-state desired-state)
   :joint-acceleration (calculate-acceleration-delta current-state desired-state)
   :joint-effort (calculate-effort-delta current-state desired-state)))