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

(defclass motion-phase ()
  ((id :initarg :id :accessor id :type string
       :documentation "ID used for identification, possibly human-readable.")
   (constraints :initarg :constraints :accessor constraints :type list
                :documentation "List of feature constraints shaping this motion phase."))
  (:documentation "Motion phase description unsing feature constraints."))

(defparameter *default-motion-id* ""
  "Default ID of motion phases.")

(defparameter *default-constraint-list* nil
  "Default constraints of motion phases.")

(defun make-motion-phase (&key (id *default-motion-id*) 
                            (constraints *default-constraint-list*))
  "Creates and returns an instance of type 'motion-phase' filled with the content provide
 by the user of default values which comply with the required types."
  (declare (type string id)
           (type list constraints))
  (make-instance
   'motion-phase
   :id id :constraints constraints))

(defun copy-motion-phase (motion &key id (constraints nil constraints-supplied-p))
  "Creates and returns an instance of type 'motion-phase' filled the content of `motion',
 possibly replaced by data given as key-parameter."
  (declare (type motion-phase motion))
  (with-slots ((old-id id) (old-constraints constraints)) motion
    (make-motion-phase
     :id (or id old-id)
     :constraints (if constraints-supplied-p
                      constraints 
                      old-constraints))))

(defmethod print-object ((object motion-phase) stream)
  "Clumsy printing of an fccl motion-phase."
  (print-unreadable-object (object stream :type t)
    (with-slots (id constraints) object
        (format stream "~%id: ~s ~%constraints: ~%~a"
                id constraints))))