;;; Copyright (c) 2014, Jan Winkler <winkler@cs.uni-bremen.de>
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

(in-package :cl-control-primitives)

(defclass connectable ()
  ((inputs :reader inputs :initarg :inputs)
   (outputs :reader outputs :initarg :outputs)))

(defclass controllable (connectable)
  ((control-function :reader control-function
                     :initarg :control-function)
   (state-function :reader state-function
                   :initarg :state-function)))

(defclass controller (connectable)
  ((controller-function :reader controller-function
                        :initarg :controller-function)))

(defun make-plant (control-function state-function inputs outputs)
  (make-instance
   'controllable
   :control-function control-function
   :state-function state-function
   :inputs inputs
   :outputs outputs))

(defun make-controller (controller-function inputs outputs)
  (make-instance
   'controller
   :controller-function controller-function
   :inputs inputs
   :outputs outputs))

(defun make-p-controller (reference-value gain &key min max)
  (make-controller (lambda (x)
                     (let* ((strength (* (- x reference-value) gain))
                            (saturated-strength
                              (cond (max
                                     (* (signum strength)
                                        (min (abs strength) (abs max))))
                                    (t strength)))
                            (elevated-strength
                              (cond (min
                                     (* (signum saturated-strength)
                                        (max (abs saturated-strength) (abs min))))
                                    (t saturated-strength))))
                       (- elevated-strength))) 1 1))

(defun control (plant controllers threshold-function
                &optional wait-time io-in io-out)
  (let* ((controllers-inputs (loop for controller in controllers
                                   summing (inputs controller)))
         (controllers-outputs (loop for controller in controllers
                                    summing (outputs controller)))
         (io-in
           (make-array
            `(,(outputs plant) ,controllers-inputs)
            :initial-contents
            (or
             io-in
             (loop for i from 0 below (outputs plant)
                   collect (loop for j from 0 below controllers-inputs
                                 collect (cond ((eql j i) 1)
                                               (t 0)))))))
         (io-out
           (make-array
            `(,controllers-outputs ,(inputs plant))
            :initial-contents
            (or io-out
                (loop for i from 0 below controllers-outputs
                      collect (loop for j from 0 below (inputs plant)
                                    collect (cond ((eql j i) 1)
                                                  (t 0))))))))
    (loop while (not (funcall threshold-function plant))
          as wait = (and wait-time (sleep wait-time))
          do (control-step plant controllers io-in io-out))))

(defun control-step (plant controllers io-in io-out)
  (let* ((plant-state-values (funcall (state-function plant)))
         (plant-state
           (make-array
            `(,(length plant-state-values) 1)
            :initial-contents
            (mapcar
             (lambda (x)
               `(,x))
             plant-state-values)))
         (mapped-state (mmult io-in plant-state))
         (controller-index 0))
    (let* ((control-out
             (loop for controller in controllers
                   as inputs = (inputs controller)
                   as state = (array-slice mapped-state controller-index)
                   as control-in = (subseq (1d-array-to-list state)
                                           0
                                           (inputs controller))
                   collect (progn
                             (incf controller-index)
                             (apply (controller-function controller)
                                    (first control-in)
                                    (rest control-in)))))
           (ctrl-len (length control-out))
           (plant-in
             (alexandria:flatten
              (2d-array-to-list
               (mmult
                io-out
                (make-array
                 `(,ctrl-len 1)
                 :initial-contents
                 (loop for i from 0 below ctrl-len
                       collect `(,(elt control-out i)))))))))
      (apply (control-function plant)
             (first plant-in) (rest plant-in)))))

(defun 1d-array-to-list (array)
  (loop for j below (array-dimension array 0)
        collect (aref array j)))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

(defun array-slice (arr row)
  (make-array (array-dimension arr 1)
              :displaced-to arr
              :displaced-index-offset (* row (array-dimension arr 1))))

(defun mmult (a b)
  (loop
    with m = (array-dimension a 0)
    with n = (array-dimension a 1)
    with l = (array-dimension b 1)
    with c = (make-array (list m l) :initial-element 0)
    for i below m do
      (loop for k below l do
        (setf (aref c i k)
              (loop for j below n
                    sum (* (aref a i j)
                           (aref b j k)))))
    finally (return c)))
