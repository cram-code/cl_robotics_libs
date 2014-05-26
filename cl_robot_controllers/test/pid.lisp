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

(in-package :cl-robot-controllers-tests)

(define-test p-controller ()
  "In-depth unit-tests for p-controller."
  (let* ((controller (make-p-controller :p-gain 1.0))
         (controller2 (copy-p-controller controller)))
    (assert-number-equal 1.0 (p-controller-p-gain controller))
    (assert-number-equal (p-controller-p-gain controller) 
                         (p-controller-p-gain controller2))
    (assert-false (eq controller controller2))
    (assert-number-equal 1.0 (compute-p-control controller 1.0))
    (assert-number-equal 1.0 (compute-p-control controller2 1.0))
    (assert-number-equal 1.0 (p-controller-p-gain controller))
    (assert-number-equal 1.0 (p-controller-p-gain controller2))))

(define-test i-controller ()
  "In-depth unit-tests for i-controller."
  (let* ((controller 
           (make-i-controller :i-gain 1.0 :i-max 10.0 :i-min -10.0 :dt 1.5))
         (controller2 (copy-i-controller controller))
         (controller4 
           (make-i-controller
            :i-gain 1.0 :i-max 10.0 :i-min -10.0  :integrated-error 1.5 :dt 1.5)))
    ;; CHECKING CONTRUCTOR AND COPY
    (assert-number-equal 1.0 (i-controller-i-gain controller))
    (assert-number-equal -10.0 (i-controller-i-min controller))
    (assert-number-equal 10.0 (i-controller-i-max controller))
    (assert-number-equal 0.0 (i-controller-integrated-error controller))
    (assert-number-equal 1.5 (i-controller-dt controller))
    (assert-false (eq controller controller2))
    (with-slots (i-gain i-min i-max integrated-error dt) controller2
      (assert-number-equal 1.0 i-gain)
      (assert-number-equal -10.0 i-min)
      (assert-number-equal 10.0 i-max)
      (assert-number-equal 0.0 integrated-error)
      (assert-number-equal 1.5 dt))
    (with-slots (i-gain i-min i-max integrated-error dt) controller4
      (assert-number-equal 1.0 i-gain)
      (assert-number-equal -10.0 i-min)
      (assert-number-equal 10.0 i-max)
      (assert-number-equal 1.5 integrated-error)
      (assert-number-equal 1.5 dt))
    ;; CHECKING COMPUTATION
    (assert-number-equal 1.5 (compute-i-control controller 1.0))
    (assert-number-equal 1.5 (i-controller-integrated-error controller))
    (assert-number-equal 3 (compute-i-control controller 1.0))
    (assert-number-equal 3 (i-controller-integrated-error controller))
    (assert-number-equal 10.0 (compute-i-control controller 10.0))
    (assert-number-equal 10.0 (i-controller-integrated-error controller))
    (assert-number-equal 8.5 (compute-i-control controller -1.0))
    (assert-number-equal 8.5 (i-controller-integrated-error controller))
    (assert-number-equal -10.0 (compute-i-control controller -15.0))
    (assert-number-equal -10 (i-controller-integrated-error controller))
    ;; CHECKING ERROR CATCHING
    (setf (i-controller-dt controller) 0.0)
    (assert-error 'error (compute-i-control controller -11.0))
    (setf (i-controller-dt controller) -0.1)
    (assert-error 'error (compute-i-control controller -11.0))
    (setf (i-controller-dt controller) 1)
    (setf (i-controller-i-min controller) (+ 1 (i-controller-i-max controller)))
    (assert-error 'error (compute-i-control controller 1.0))))

(define-test d-controller ()
  "In-depth unit-tests for d-controller."
  (let* ((controller (make-d-controller :d-gain 1.0))
         (controller2 (copy-d-controller controller)))
    ;; CHECKING CONSTRUCTOR AND COPYING
    (assert-number-equal 1.0 (d-controller-d-gain controller))
    (assert-number-equal 1.0 (d-controller-d-gain controller2))
    (assert-false (eq controller controller2))
    ;; CHECK COMPUTATION
    (assert-number-equal 2 (compute-d-control controller 2.0))
    (assert-number-equal 1 (d-controller-d-gain controller))
    (assert-number-equal 1 (compute-d-control controller 1.0))))

(define-test pd-control ()
  "Checks basic PD control behavior over two cycles."
  (let ((controller
          (make-pd-controller
           :p-controller (make-p-controller :p-gain 1.0)
           :d-controller (make-d-controller :d-gain 3.0))))
    (assert-number-equal 93 (compute-pd-control controller 3.0 30.0))
    (assert-number-equal -28.0 (compute-pd-control controller 2.0 -10.0))))

(define-test pi-control ()
  "Checks basic PI control behavior over two cycles."
  (let* ((controller 
           (make-pi-controller
            :p-controller (make-p-controller :p-gain 1.0)
            :i-controller (make-i-controller :i-gain 2.0 :i-max 2.0 :i-min -2.0 :dt 0.1))))
    ;; cycle 1
    (assert-number-equal 3.6 (compute-pi-control controller 3.0)
    (assert-number-equal 0.6 (i-controller-integrated-error 
                              (pi-controller-i-controller controller)))
    ;; cycle 2
    (assert-number-equal 3.0 (compute-pi-control controller 2.0))
    (assert-number-equal 1.0 (i-controller-integrated-error
                              (pi-controller-i-controller controller))))))

(define-test pid-control ()
  "Checks basic PID control behavior over two cycles."
  (let* ((controller 
           (make-pid-controller
            :p-controller (make-p-controller :p-gain 1.0)
            :i-controller (make-i-controller :i-gain 2.0 :i-max 2.0 :i-min -2.0 :dt 0.1)
            :d-controller (make-d-controller :d-gain 3.0))))
    ;; cycle 1
    (assert-number-equal 93.6 (compute-pid-control controller 3.0 30.0))
    (assert-number-equal 0.6 (i-controller-integrated-error 
                              (pid-controller-i-controller controller)))
    ;; cycle 2
    (assert-number-equal -27.0 (compute-pid-control controller 2.0 -10.0))
    (assert-number-equal 1.0 (i-controller-integrated-error
                              (pid-controller-i-controller controller)))))