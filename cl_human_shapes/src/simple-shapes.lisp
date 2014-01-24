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

(in-package :cl-human-shapes)

(defparameter *default-body-part-id* 0
  "Default id of body parts used in case users do not provide any.")
(defparameter *default-body-part-shape* (make-sphere)
  "Default shape of body parts used in case users do not provide any.")
(defparameter *default-body-part-label* :NO-BODY-PART
  "Default shape of body parts used in case users do not provide any.")

(defclass human-body ()
  ((body-parts :initarg :body-parts :initform (list) :accessor body-parts :type list
               :documentation "List of body-parts that are part of this human."))
  (:documentation "A Simple representation of the body of a human co-worker."))

(defclass human-body-part ()
  ((id :initarg :id :accessor id :type number
       :documentation "Unique id used to identify body part.")
   (label :initarg :label :accessor label :type symbol
          :documentation "Symbol denoting the type of body part.")
   (shape :initarg :shape :accessor shape :type sphere
          :documentation "Shape object approximating physical extents of body-part."))
  (:documentation "Representation of human body part."))

(defun make-human-body (&key (body-parts nil))
  "Creates and returns an instance of type 'human-body' filled with body-parts from list
 `body-parts'."
  (declare (type list body-parts))
  (make-instance 'human-body :body-parts body-parts))

(defun make-human-body-part (&key (id *default-body-part-id*)
                               (label *default-body-part-label*) 
                               (shape *default-body-part-shape*))
  "Creates and returns an instance of type 'human-body-part' filled with `id', `label' and
 `shape'."
  (declare (type number id)
           (type symbol label)
           (type sphere shape))
  (make-instance 'human-body-part :id id :label label :shape shape))