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

(in-package :cl-3d-shapes)

;;; 3D SPHERES

(defparameter *default-sphere-centroid* (make-identity-vector)
  "Default centroid given to spheres if nothing is provided by user.")
(defparameter *default-sphere-radius* 0
  "Default radius given to spheres if nothing is provided by user.")
(defparameter *default-sphere-frame-id* ""
  "Default frame-id given to spheres if nothing is provided by user.")

(defclass sphere ()
  ((centroid :initarg :centroid :initform *default-sphere-centroid* :accessor centroid
             :type 3d-vector
             :documentation "3d-point representing the centroid of the sphere in Cart.
             coordinates w.r.t. to frame-id. Units: (m).")
   (radius :initarg :radius :initform *default-sphere-radius* :accessor radius
           :type number
           :documentation "Radius of sphere (in m).")
   (frame-id :initarg :frame-id :initform *default-sphere-frame-id* :accessor frame-id
             :type string))
  (:documentation "Representation of a 3D sphere in Cartesian space."))

(defun make-sphere (&key (centroid *default-sphere-centroid*) 
                      (radius *default-sphere-radius*) (frame-id *default-sphere-frame-id*))
  "Creates and returns an instance of type 'sphere' with `centroid', `radius' and `frame-id'
 set as specified by the user."
  (declare (type 3d-vector centroid)
           (type number radius)
           (type string frame-id))
  (make-instance 'sphere :centroid centroid :radius radius :frame-id frame-id))