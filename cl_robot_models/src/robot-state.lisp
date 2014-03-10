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

(defclass robot-state ()
  ((robot-name :initarg :robot-name :accessor robot-name
               :documentation "ID used to identify the robot.")
   (robot-model :initarg :robot-model :accessor robot-model
                :documentation "ID used to match the robot against a robot model.")
   (joint-states :initarg joint-states :initform (make-hash-table :test 'equal)
                 :accessor joint-states
                 :documentation "Hash-table mapping joint-names to joint-states"))
  (:documentation "Representation of state of a single robot."))

(define-condition robot-state-error (simple-error) ())

(defun get-joint-state (robot-state joint-name)
  "Retrieves joint-state identified with `joint-name' from `robot-state'. If not present,
 throws error of type 'robot-state-error'."
  (declare (type robot-state robot-state)
           (type string joint-name))
  (multiple-value-bind (joint-state joint-state-present-p)
      (gethash joint-name (joint-states robot-state))
    (if joint-state-present-p
        joint-state
        (error 'robot-state-error
               :format-control "Joint '~a' not found in robot-state."
               :format-arguments (list joint-name)))))

(defun set-joint-state (robot-state joint-state)
  "Adds/overwrites `joint-state' in `robot-state'."
  (declare (type robot-state robot-state)
           (type joint-state joint-state))
  (setf (gethash (joint-name joint-state) (joint-states robot-state)) joint-state))

(defun remove-joint-state (robot-state joint-name)
  "Removes the joint-state identified by `joint-name' from `robot-state'."
  (declare (type robot-state robot-state)
           (type string joint-name))
  (remhash joint-name (joint-states robot-state)))