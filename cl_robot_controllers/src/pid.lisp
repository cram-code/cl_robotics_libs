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
;;; EXPORTED API
;;;

(defstruct pid-gains
  "Representation of gains of a PID controller."
  (p-gain 0.0 :type number :read-only t)
  (i-gain 0.0 :type number :read-only t)
  (d-gain 0.0 :type number :read-only t)
  (i-max 0.0 :type number :read-only t)
  (i-min 0.0 :type number :read-only t))

(defstruct pid
  "Representation of a PID controller."
  (gains (make-pid-gains) :type pid-gains :read-only t)
  (last-i-term 0.0 :type number :read-only t)
  (last-error 0.0 :type number :read-only t))

(define-condition pid-controller-error (error)
  ((text :initarg :text :reader text)))

(defun compute-command (pid error dt)
  "Computes a command of the PID controller `pid' to counter a current `error'. `dt' is 
 the time [in seconds] which passed since we were last called to control. Returns the 
 command as a number and the new PID controller (with update internal state). Signals
 error of type 'pid-controller-error in case of insurmountable issues."
  (declare (type pid pid)
           (type number error dt))
  (with-slots (gains last-error last-i-term) pid
    (with-slots (p-gain i-gain d-gain i-max i-min) gains
      (let ((p-term (compute-p-term p-gain error))
            (i-term (compute-i-term i-gain error dt last-i-term i-min i-max))
            (d-term (compute-d-term d-gain error dt last-error)))
        (values
         (+ p-term i-term d-term)
         (make-pid :gains (copy-pid-gains gains) :last-i-term i-term :last-error error))))))

;;;
;;; INTERNAL API
;;;

(defun compute-p-term (p-gain error)
  (* p-gain error))

(defun compute-i-term (i-gain error dt last-i-term i-min i-max)
  (when (>= 0.0 dt)
    (error 'pid-controller-error :text "dt provided to PID controller not greater 0.0"))
  (limit-value (+ last-i-term (* i-gain error dt)) i-min i-max))

(defun compute-d-term (d-gain error dt last-error)
  (when (>= 0.0 dt)
    (error 'pid-controller-error :text "dt provided to PID controller not greater 0.0"))
  (* d-gain (/ (- error last-error) dt)))
       
(defun limit-value (current-value minimum-value maximum-value)
  (declare (type number current-value minimum-value maximum-value))
  (when (> minimum-value maximum-value)
    (error 'pid-controller-error :text "Provided i-min greater than i-max."))
  (max minimum-value (min current-value maximum-value)))