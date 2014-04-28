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

(in-package :robot-controllers)

;;;
;;; GENERIC SETUP OF HASHED CONTROLLER CONFIGURATIONS
;;;
;;; The idea is to provide a configuration with a hash-table inside
;;; which fully specifies which controller to create and with which
;;; values to fill it. My intention is to instantiate controllers at
;;; runtime from data without a need to directly depending on the lib.
;;;
;;; EXAMPLE: P-CONTROLLER
;;;   Hash-table in slot 'content' is expected to have the following
;;;   key-value pairs:
;;;     :controller-name --> "P-CONTROLLER"
;;;     :package-name --> "CL-ROBOT-CONTROLLERS"
;;;     :p-gain --> 2.5
;;;
;;;   Calling make-controller with such a configuration `config'
;;;   and specifying that the controller-type is :unknown:
;;;     CL-USER > (make-controller config :unknown)
;;;     CL-USER > #<P-CONTROLLER {...}>
;;;   

(defclass hashed-controller-configuration (controller-configuration) 
  ((content :initform (make-hash-table :test 'equal) :initarg :content
            :accessor content :type hash-table
            :documentation "Hashed content of this configuration."))
  (:documentation "Hashed-controller-configuration for prototyping."))

(defmethod make-controller ((configuration hashed-controller-configuration) 
                            (controller-type (eql :UNKNOWN)))
    "Default implementation trying to look up controller type from configuration."
    (declare (ignore controller-type))
    (multiple-value-bind (controller-name package-name)
        (read-hash-values (content configuration) (list :controller-name :package-name))
      (make-controller configuration (intern controller-name package-name))))