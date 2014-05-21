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
;;; PORTED FROM: github.com/ros/angles.git
;;;

(defconstant 2PI (* 2 PI) 
  "Approximation of the mathematical constant <2 PI>")

(declaim (inline to-deg))
(defun to-deg (radians)
  "Converts an angle `radians' given in radians to degrees."
  (/ (* 180.0 radians) PI))

(declaim (inline to-rad))
(defun to-rad (degrees)
  "Converts an angle `degrees' given in degrees to radians."
  (/ (* degrees PI) 180.0))

(declaim (inline remainder))
(defun remainder (number divisor)
  (multiple-value-bind (quotient remainder) 
      (floor number divisor)
    (declare (ignore quotient))
    remainder))

(declaim (inline normalize-angle-positive))
(defun normalize-angle-positive (angle)
  "Normalizes `angle', assumed to be in radians, to be within [0, 2*PI]."  
  (remainder (+ (remainder angle 2PI) 2PI) 2PI))

(declaim (inline normalize-angle))
(defun normalize-angle (angle)
  "Normalizes `angle', assumed to be in radians, to be within [-PI, +PI]."
  (let ((a (normalize-angle-positive angle)))
    (if (> a PI)
        (- a 2PI)
        a)))

(declaim (inline shortest-angular-distance))
(defun shortest-angular-distance (from to)
  "Given 2 angles (in radians), returns the shortest angular distance
 from `from' to `to'.

 The result is always within [-PI, +PI]. Adding the result to `from'
 will yield an angle equivalent to `to'."
  (let ((diff 
          (normalize-angle-positive 
           (- (normalize-angle-positive to) (normalize-angle-positive from)))))
    ;; If diff > PI then it's shorter the other way round.
    (if (> diff PI)
        (normalize-angle (- diff 2PI))
        (normalize-angle diff))))

(declaim (inline two-pi-complement))
(defun two-pi-complement (angle)
  "Returns the equivalent (in rad) of `angle' (in rad) within [-2PI, 2PI]."
  (let ((unit-angle 
          (if (or (> angle 2PI) (< angle (- 2PI)))
              (remainder angle 2PI)
              angle)))
    (cond
      ((< unit-angle 0) (+ unit-angle 2PI))
      ((> unit-angle 0) (- unit-angle 2PI))
      (t 2PI))))

(defun find-min-max-delta (from left-limit right-limit)
  "Returns the min and max amount (in radians) that can be moved
 from `from' angle to `left-limit' and `right-limit', all in radians.
 Also returns whether `from' was within [`left-limit', `right-limit'].
 First value: minimum (delta) angle (in radians) that can be moved
 from `from' position before hitting the joint stop. Second value:
 maximum (delta) angle (in radians) that can be moved from `from'
 position before hitting the joint stop. Third value: 't' if `from'
 was within [`left-limit', `right-limit'], else 'nil'.

 Note: `from', `left-limit', and `right-limit' are assumed to lie
 within [-PI, +PI]."
  (let* ((delta-min (shortest-angular-distance from left-limit))
         (delta-min-compl (two-pi-complement delta-min))
         (delta-max (shortest-angular-distance from right-limit))
         (delta-max-compl (two-pi-complement delta-max)))
    (when (= 0 delta-min)
      (values delta-min (max delta-max delta-max-compl) t))
    (when (= 0 delta-max)
      (values (min delta-min delta-min-compl) delta-max t))

    (when (< delta-min-compl delta-min)
      (rotatef delta-min delta-min-compl))
    (when (> delta-max-compl delta-max)
      (rotatef delta-max delta-max-compl))

    (when (or (<= delta-min delta-max-compl)
              (>= delta-max delta-min-compl))
      (if (and (= left-limit (- PI)) (= right-limit PI))
          (values delta-max-compl delta-min-compl t)
          (values delta-max-compl delta-min-compl nil)))
    
    (values delta-min delta-max t)))