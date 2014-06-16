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

(define-test fir-test ()
  (let ((b-coeff (make-array 5 :initial-element 0.2))
        (input-data '(1 1.2 1.4 1.6 1.8 2 2.2 2.4 2.6 2.8 3.0 3.2 3.4 3.6 3.8 4.0))
        (result-data '(0.2 0.44 0.72 1.04 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.4 3.6)))
    (let ((filter (create-digital-filter b-coeff)))
     (mapcar (lambda (des-result calc-result)
               (assert-float-equal des-result calc-result))
             result-data
             (mapcar (lambda (in) (digital-filter filter in)) input-data)))))

(define-test iir-test ()
  (let ((b-coeff '(0.0024 -0.0012 0.0034 -0.0012 0.0024))
        (a-coeff '(1.0000 -3.2179 3.9457 -2.1773 0.4553))
        (input-data '(1 1 1 1 1 1 1 1 1 1))
        (result-data '(0.0024 0.0090 0.0241 0.0506 0.0920 0.1508 0.2271 0.3188 0.4223 0.5325)))
    (let ((filter (create-digital-filter 
                   (coerce b-coeff 'vector) (coerce a-coeff 'vector))))
      ;; TODO(Georg): consider why the numerical error is so big..
      (let ((*epsilon* 0.05))
        (mapcar (lambda (des-result calc-result)
                  (assert-float-equal des-result calc-result))
              result-data
              (mapcar (lambda (in) (digital-filter filter in)) input-data))))))