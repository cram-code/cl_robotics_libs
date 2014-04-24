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
  (let* ((controller (make-p-controller 1.0))
         (controller2 (copy-p-controller controller))
         (controller3 (copy-p-controller controller :p-gain 2.0)))
    (assert-number-equal 1.0 (p-gain controller))
    (assert-number-equal (p-gain controller) (p-gain controller2))
    (assert-number-equal 2.0 (p-gain controller3))
    (assert-false (eq controller controller2))
    (assert-false (eq controller controller3))
    (assert-number-equal 1.0 (compute-command controller :error 1.0))
    (assert-number-equal 1.0 (compute-command controller2 :error 1.0 :dt 0.0))
    (assert-number-equal 1.0 (compute-command controller3 :error 0.5 :dt 0.001))
    (assert-number-equal 1.0 (p-gain controller))
    (assert-number-equal (p-gain controller) (p-gain controller2))
    (assert-number-equal 2.0 (p-gain controller3))))

(define-test i-controller ()
  "In-depth unit-tests for i-controller."
  (let* ((controller (make-i-controller 1.0 10.0 -10.0))
         (controller2 (copy-i-controller controller))
         (controller3 (copy-i-controller controller :i-gain 2.0 :i-max 20.0 :i-min -20.0
                                         :integrated-error 2.5))
         (controller4 (make-i-controller 1.0 10.0 -10.0 1.5)))
    ;; CHECKING CONTRUCTOR AND COPY
    (assert-number-equal 1.0 (i-gain controller))
    (assert-number-equal -10.0 (i-min controller))
    (assert-number-equal 10.0 (i-max controller))
    (assert-number-equal 0.0 (integrated-error controller))
    (assert-false (eq controller controller2))
    (assert-number-equal 1.0 (i-gain controller2))
    (assert-number-equal -10.0 (i-min controller2))
    (assert-number-equal 10.0 (i-max controller2))
    (assert-number-equal 0.0 (integrated-error controller2))
    (assert-false (eq controller controller3))
    (assert-number-equal 2.0 (i-gain controller3))
    (assert-number-equal -20.0 (i-min controller3))
    (assert-number-equal 20.0 (i-max controller3))
    (assert-number-equal 2.5 (integrated-error controller3))
    (assert-number-equal 1.0 (i-gain controller4))
    (assert-number-equal -10.0 (i-min controller4))
    (assert-number-equal 10.0 (i-max controller4))
    (assert-number-equal 1.5 (integrated-error controller4))
    ;; CHECKING COMPUTATION
    (assert-number-equal 1.5 (compute-command controller :error 1.0 :dt 1.5))
    (assert-number-equal 1.5 (integrated-error controller))
    (assert-number-equal 2.5 (compute-command controller :error 1.0 :dt 1.0))
    (assert-number-equal 10.0 (compute-command controller :error 10.0 :dt 1.0))
    (assert-number-equal 9.0 (compute-command controller :error -1.0 :dt 1.0))
    (assert-number-equal -10.0 (compute-command controller :error -11.0 :dt 2.0))
    ;; CHECKING ERROR CATCHING
    (assert-error 'error (compute-command controller :error -11.0 :dt 0.0))
    (assert-error 'error (compute-command controller :error -11.0 :dt -0.1))
    (setf (i-min controller) (+ 1 (i-max controller)))
    (assert-error 'error (compute-command controller :error 1.0 :dt 1.5))))

(define-test d-controller ()
  "In-depth unit-tests for d-controller."
  (let* ((controller (make-d-controller 1.0))
         (controller2 (copy-d-controller controller))
         (controller3 (copy-d-controller controller :d-gain 2.0 :last-error 1.5))
         (controller4 (make-d-controller 1.0 2.0)))
    ;; CHECKING CONSTRUCTOR AND COPYING
    (assert-number-equal 1.0 (d-gain controller))
    (assert-number-equal 0.0 (last-error controller))
    (assert-number-equal 1.0 (d-gain controller2))
    (assert-number-equal 0.0 (last-error controller2))
    (assert-false (eq controller controller2))
    (assert-number-equal 2.0 (d-gain controller3))
    (assert-number-equal 1.5 (last-error controller3))
    (assert-false (eq controller controller3))
    (assert-number-equal 1.0 (d-gain controller4))
    (assert-number-equal 2.0 (last-error controller4))
    ;; CHECK COMPUTATION
    (assert-number-equal 2 (compute-command controller :error 1.0 :dt 0.5))
    (assert-number-equal 1 (last-error controller))
    (assert-number-equal 0 (compute-command controller :error 1.0 :dt 0.5))
    ;; CHECK ERROR CATCHING
    (assert-error 'error (compute-command controller :error 1.0 :dt 0.0))
    (assert-error 'error (compute-command controller :error 1.0 :dt -0.1))))
    

(define-test pid-control ()
  "Checks basic PID control behavior over two cycles."
  (let* ((controller (make-pid-controller
                      (make-p-controller 1.0)
                      (make-i-controller 2.0 2.0 -2.0)
                      (make-d-controller 3.0))))
    ;; cycle 1
    (assert-number-equal 93.6 (compute-command controller :error 3.0 :dt 0.1))
    (assert-number-equal 3 (last-error (d-controller controller)))
    (assert-number-equal 0.6 (integrated-error (i-controller controller)))
    ;; cycle 2
    (assert-number-equal -27.0 (compute-command controller :error 2.0 :dt 0.1))
    (assert-number-equal 2 (last-error (d-controller controller)))
    (assert-number-equal 1.0 (integrated-error (i-controller controller)))))