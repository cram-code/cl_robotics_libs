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

;; (define-test pid-gains-basics ()
;;   "Checks whether creating and getting of pid gains works."
;;   (let ((gains (make-pid-gains :p-gain 1.0 :d-gain -2.0 :i-max 0.1)))
;;     (with-slots (p-gain i-gain d-gain i-max i-min) gains
;;       (assert-equal p-gain 1.0)
;;       (assert-equal i-gain 0.0)
;;       (assert-equal d-gain -2.0)
;;       (assert-equal i-max 0.1)
;;       (assert-equal i-min 0.0))))

;; (define-test pid-controller-basics ()
;;   "Checks whether creating and getting of slots of pid-controller works."
;;   (let ((controller (make-pid :gains (make-pid-gains :p-gain 1.0) :last-i-term 12.1)))
;;     (with-slots (gains last-i-term last-error) controller
;;       (assert-equalp gains (make-pid-gains :p-gain 1.0))
;;       (assert-equal last-i-term 12.1)
;;       (assert-equal last-error 0.0))))
    
;; (define-test p-control ()
;;   "Checks whether we can use the PID as a simple p-controller."
;;   (let* ((controller (make-pid :gains (make-pid-gains :p-gain 2.0)))
;;          (controller-copy (copy-pid controller)))
;;     (multiple-value-bind (command new-controller) (compute-command controller 3.0 0.1)
;;       (with-slots (gains last-i-term last-error) new-controller
;;         (assert-equal command 6.0)
;;         (assert-equal last-error 3.0)
;;         (assert-equal last-i-term 0.0)
;;         (assert-equalp gains (slot-value new-controller 'gains))
;;         (assert-equalp controller controller-copy)))))

;; (define-test i-control ()
;;   "Checks whether we can use the PID as a simple i-controller."
;;   (let* ((controller 
;;            (make-pid :gains (make-pid-gains :i-gain 2.0 :i-min -10.0 :i-max 10.0)))
;;          (controller-copy (copy-pid controller)))
;;     (multiple-value-bind (command new-controller) (compute-command controller 3.0 0.1)
;;       (with-slots (gains last-i-term last-error) new-controller
;;         (assert-equal command 0.6)
;;         (assert-equal last-error 3.0)
;;         (assert-equal last-i-term 0.6)
;;         (assert-equalp gains (slot-value new-controller 'gains))
;;         (assert-equalp controller controller-copy)))))

;; (define-test limit-i-term ()
;;   "Checks whether limiting the i-term works as expected."
;;   (let* ((controller1 (make-pid :gains (make-pid-gains :i-gain 2.0)))
;;          (controller2 (make-pid :gains (make-pid-gains :i-gain 2.0 :i-min -5 :i-max 10)))
;;          (controller3 (make-pid :gains (make-pid-gains :i-gain 2.0 :i-min 10 :i-max 10))))
;;     ;; CONTROLLER WITH LIMITS OF 0 SHALL HAVE NO I-TERM
;;     (multiple-value-bind (command new-controller) (compute-command controller1 1 1)
;;         (declare (ignore new-controller))
;;       (assert-equal command 0.0))
;;     (multiple-value-bind (command new-controller) (compute-command controller1 -2 1)
;;         (declare (ignore new-controller))
;;       (assert-equal command 0.0))
;;     ;; CONTROLLER WITH NON-0 LIMITS SHOULD LIMIT I-TERMS TO THEM
;;     (multiple-value-bind (command new-controller) (compute-command controller2 3 1)
;;         (declare (ignore new-controller))
;;       (assert-equal command 6.0))
;;     (multiple-value-bind (command new-controller) (compute-command controller2 -1.5 1)
;;         (declare (ignore new-controller))
;;       (assert-equal command -3.0))
;;     (multiple-value-bind (command new-controller) (compute-command controller2 6 1)
;;         (declare (ignore new-controller))
;;       (assert-equal command 10.0))
;;     (multiple-value-bind (command new-controller) (compute-command controller2 -3 1)
;;         (declare (ignore new-controller))
;;       (assert-equal command -5.0))
;;     ;;CONTROLLER WITH A EQUAL LIMITS SHOULD ALWAYS RETURN THEM AS I-TERM
;;     (multiple-value-bind (command new-controller) (compute-command controller3 6 1)
;;         (declare (ignore new-controller))
;;       (assert-equal command 10.0))
;;     (multiple-value-bind (command new-controller) (compute-command controller3 -3 1)
;;         (declare (ignore new-controller))
;;       (assert-equal command 10.0))
;;     (multiple-value-bind (command new-controller) (compute-command controller3 0 1)
;;         (declare (ignore new-controller))
;;       (assert-equal command 10.0))))

;; (define-test signal-error-limits-not-sane ()
;;   "Checks whether an error is signal in case the limits for the i-term are not sane."
;;   (let ((controller (make-pid :gains (make-pid-gains :i-min 0.1))))
;;     (assert-error 'pid-controller-error (compute-command controller 1.0 1.0))))

;; (define-test d-control ()
;;   "Checks whether we can use the PID controller as a simple D controller."
;;   (let* ((controller 
;;            (make-pid :gains (make-pid-gains :d-gain 2.0)))
;;          (controller-copy (copy-pid controller)))
;;     (multiple-value-bind (command new-controller) (compute-command controller 3.0 0.1)
;;       (with-slots (gains last-i-term last-error) new-controller
;;         (assert-equal command 60.0)
;;         (assert-equal last-error 3.0)
;;         (assert-equal last-i-term 0.0)
;;         (assert-equalp gains (slot-value new-controller 'gains))
;;         (assert-equalp controller controller-copy)))))

;; (define-test pid-control ()
;;   "Checks basic PID control behavior over two cycles."
;;   (let* ((controller (make-pid :gains (make-pid-gains :p-gain 1.0 :i-gain 2.0 :d-gain 3.0
;;                                                       :i-min -2.0 :i-max 2.0))))
;;     (multiple-value-bind (command new-controller) (compute-command controller 3.0 0.1)
;;       (with-slots (gains last-i-term last-error) new-controller
;;         (assert-equal command 93.6)
;;         (assert-equal last-error 3.0)
;;         (assert-equal last-i-term 0.6)
;;         (assert-equalp gains (slot-value new-controller 'gains)))
;;       (multiple-value-bind (command new-controller) (compute-command new-controller 2.0 0.1)
;;         (with-slots (gains last-i-term last-error) new-controller
;;           (assert-equal command -27.0)
;;           (assert-equal last-error 2.0)
;;           (assert-equal last-i-term 1.0)
;;           (assert-equalp gains (slot-value new-controller 'gains)))))))

;; (define-test signal-error-incorrect-dt ()
;;   "Checks whether we get an error signaled if dt is not positive."
;;   (assert-error 'pid-controller-error (compute-command (make-pid) 1.0 0.0))
;;   (assert-error 'pid-controller-error (compute-command (make-pid) 1.0 -0.1)))