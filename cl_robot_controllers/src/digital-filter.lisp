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
;;; DESIGNING THE FILTER IN MATLAB (R2013a):
;;;   requirements:
;;;     - sampling frequency 24.3Hz
;;;     - use a Chebyshev Type II filter
;;;     - highpass filter
;;;     - stopband attenuation of at least 60dB
;;;     - passband attenuation of at maximum 3dB
;;;     - stopband cutoff at 1Hz
;;;     - passband cutoff at 2Hz
;;;
;;;   determining the minimal filter order:
;;;     >> fs=24.3; Ws=1/fs; Wp=2/fs;
;;;     >> Rp=3; Rs=60;
;;;     >> [n, Ws]=cheb2ord(Wp, Ws, Rp, Rs)
;;;        n = 6
;;;        Ws = 0.0412
;;;
;;;   designing and visualizing the filter:
;;;     >> [b,a]=cheby2(n,Rs,Ws,'high');
;;;     >> freqz(b,a,512,fs)
;;;
;;;   obtained filter design:
;;;     >> b
;;;     b =
;;;        0.6653   -3.9751    9.9130  -13.2062    9.9130   -3.9751    0.6653
;;;
;;;     >> a
;;;     a =
;;;        1.0000   -5.1644   11.1827  -12.9886    8.5312   -3.0034    0.4426
;;;

;;;
;;; DIGITAL FILTERING CODE
;;;

(defstruct (digital-filter (:conc-name filter-))
  "Representation of a digital filter in Matlab style.

 See here for background:
 http://www.mathworks.de/de/help/signal/ref/digitalfilter-class.html"
  (a (make-array 0) :type array :read-only t) ; FIR params
  (b (make-array 0) :type array :read-only t) ; IIR params
  (x (make-array 0) :type array :read-only nil) ; FIFO buffered input signals
  (y (make-array 0) :type array :read-only nil) ; FIFO buffered output signals
  (iir-p nil :type boolean :read-only t)) ; flag indicating FIR/IIR filters

(defun create-digital-filter (b &optional (a nil a-supplied-p))
  "Creates a digital filter from filter coefficients. Array `b'
 represents the coefficients of a FIR filter, while the optional array
 `a' denotes the denominator coeffecients of an IIR filter. Note: If
  provided, `a' has to have the same length as `b'!

 Tries to mimick the Matlab digital filter implementation. See the
 following links for background:

 http://www.mathworks.de/de/help/signal/ref/digitalfilter-class.html"
  (let* ((len (length b))
         (filter (make-digital-filter
                  :a (if a-supplied-p a (make-array len))
                  :b b
                  :x (make-array len)
                  :y (make-array len)
                  :iir-p a-supplied-p)))
    (ensure-equal-buffer-lengths filter)
    filter))

(defun digital-filter (filter new-input)
  "Filters the single datum `new-input' with `filter' and returns
 a new single output value. NOTE: This will change the internal buffers
 of `filter'."
  (push-fifo-element 0 (filter-y filter))
  (push-fifo-element new-input (filter-x filter))
  (setf (aref (filter-y filter) 0) 
        (simulate-digital-filter filter)))
    
(defun simulate-digital-filter (filter)
  "Simulates the digital filter system `filter' without
 changing its internal state. Returns the next output datum."
  (ensure-equal-buffer-lengths filter)
  (with-slots (a b x y iir-p) filter
    (+
     (* (aref b 0) (aref x 0)) ; first data point is treated differently
     (loop for i from 1 to (1- (length a)) 
           summing
           (if iir-p
               ;; IIR filter equation
               (- (* (aref b i) (aref x i)) (* (aref a i) (aref y i)))
               ;; FIR filter equation
               (* (aref b i) (aref x i)))))))

(defun ensure-equal-buffer-lengths (digital-filter)
  "Ensures that all the internal members of `digital-filter'
 have equal size."
  (unless (equal-buffer-lengths-p digital-filter)
    (error "Digital filter has buffers of varying length.")))

(defun equal-buffer-lengths-p (digital-filter)
  "Predicate to check whether all internal members of
 `digital-fitler have equal size."
  (= (length (filter-a digital-filter))
     (length (filter-b digital-filter))
     (length (filter-x digital-filter))
     (length (filter-y digital-filter))))

;;;
;;; FIFO BUFFER AUX CODE
;;;

(defun push-fifo-element (new-element fifo-buffer)
  "Pushes the number `new-element' onto the number array `fifo-buffer'. Will shift all
 prior elements one slot back and push `new-element' at the front. Will return a fifo
 of the same size as `fif-buffer'. Does nothing if `fifo-buffer' has size 0."
  (declare (type number new-element)
           (type array fifo-buffer))
  (when (> (length fifo-buffer) 0)
    (loop for i from (1- (length fifo-buffer)) downto 1 do
      (setf (aref fifo-buffer i) (aref fifo-buffer (1- i))))
    (setf (aref fifo-buffer 0) new-element))
  fifo-buffer)