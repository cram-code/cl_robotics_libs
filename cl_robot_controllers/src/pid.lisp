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
;;; P-CONTROLLER
;;;

(defclass p-controller ()
  ((p-gain :initform 0.0 :initarg :p-gain :accessor p-gain :type number))
  (:documentation "A simple p-controller."))

(defun make-p-controller (p-gain)
  (make-instance 'p-controller :p-gain p-gain))

(defun copy-p-controller (p-controller &key p-gain)
  (with-slots ((old-p-gain p-gain)) p-controller
    (make-p-controller (or p-gain old-p-gain))))

(defmethod compute-command ((controller p-controller) &key error &allow-other-keys)
  (* error (p-gain controller)))

;;;
;;; I-CONTROLLER
;;;

(defclass i-controller ()
  ((i-gain :initform 0.0 :initarg :i-gain :accessor i-gain :type number)
   (i-max :initform 0.0 :initarg :i-max :accessor i-max :type number)
   (i-min :initform 0.0 :initarg :i-min :accessor i-min :type number)
   (integrated-error :initform 0.0 :initarg :integrated-error :accessor integrated-error
                     :type number :documentation "For internal use."))
  (:documentation "A simple i-controller."))

(defun make-i-controller (i-gain i-max i-min &optional (integrated-error 0))
  (make-instance 'i-controller :i-gain i-gain :i-max i-max :i-min i-min 
                               :integrated-error integrated-error))

(defun copy-i-controller (i-controller &key i-gain i-max i-min integrated-error)
  (with-slots ((old-i-gain i-gain) (old-i-max i-max) (old-i-min i-min)
               (old-integrated-error integrated-error)) i-controller
    (make-i-controller (or i-gain old-i-gain) (or i-max old-i-max) (or i-min old-i-min)
                       (or integrated-error old-integrated-error))))

(defmethod compute-command ((controller i-controller) &key error dt &allow-other-keys)
  (flet ((limit-value (current-value minimum-value maximum-value)
           (max minimum-value (min current-value maximum-value))))
    (with-slots (i-gain i-min i-max integrated-error) controller
      (when (>= 0.0 dt)
        (error "dt provided to i-controller not greater than 0.0"))
      (when (> i-min i-max)
        (error "i-min bigger than i-max during control of i-controller."))
      (setf integrated-error 
            (limit-value (+ integrated-error (* i-gain error dt)) i-min i-max)))))

;;;
;;; D-CONTROLLER
;;;

(defclass d-controller ()
  ((d-gain :initform 0.0 :initarg :d-gain :accessor d-gain :type number)
   (last-error :initform 0.0 :initarg :last-error :accessor last-error
               :type number :documentation "For internal use."))
  (:documentation "A simple d-controller."))

(defun make-d-controller (d-gain &optional (last-error 0))
  (make-instance 'd-controller :d-gain d-gain :last-error last-error))

(defun copy-d-controller (d-controller &key d-gain last-error)
  (with-slots ((old-d-gain d-gain) (old-last-error last-error)) d-controller
    (make-d-controller (or d-gain old-d-gain) (or last-error old-last-error))))

(defmethod compute-command ((controller d-controller) &key error dt &allow-other-keys)
  (with-slots (d-gain last-error) controller
    (when (>= 0.0 dt)
      (error "dt provided to d-controller not greater than 0.0"))
    (let ((command (* d-gain (/ (- error last-error) dt))))
      (setf last-error error)
      command)))

;;;
;;; MACRO TO WRITE COMPOUND CONTROLLERS
;;;
 
;;;
;;; REFERENCE IMPLEMENTATION: PID-CONTROLLER
;;; (def-compound-controller t t t) should deliver this:
;;;
;;; (defclass pid-controller ()
;;;   ((p-controller :initform (make-instance 'p-controller) :initarg :p-controller
;;;                  :accessor p-controller :type p-controller)
;;;    (i-controller :initform (make-instance 'i-controller) :initarg :i-controller
;;;                  :accessor i-controller :type i-controller)
;;;    (d-controller :initform (make-instance 'd-controller) :initarg :d-controller
;;;                  :accessor d-controller :type d-controller))
;;;   (:documentation "A simple compound PID-controller."))
;;;
;;; (defun make-pid-controller (p-controller i-controller d-controller)
;;;   (make-instance 'pid-controller :p-controller p-controller :i-controller i-controller
;;;                                  :d-controller d-controller))
;;;
;;; (defun copy-pid-controller (pid-controller &key p-controller i-controller d-controller)
;;;   (with-slots ((old-p-controller p-controller) (old-i-controller i-controller)
;;;                (old-d-controller)) pid-controller
;;;     (make-pid-controller (or p-controller old-p-controller) 
;;;                          (or i-controller old-i-controller)
;;;                          (or d-controller old-d-controller))))
;;;
;;; (defmethod compute-command (pid-controller &key error dt &allow-other-keys)
;;;   (with-slots (p-controller i-controller d-controller) pid-controller
;;;     (+ (compute-command p-controller :error error :dt dt)
;;;        (compute-command i-controller :error error :dt dt)
;;;        (compute-command d-controller :error error :dt dt))))
;;;

(defmacro def-compound-controller (p-controller-p i-controller-p d-controller-p)
;;; TODO(Georg): consider rewriting the function and method definitions by
;;;              introspection of the type. After all, it should already be
;;;              written and compiled after the first line of the macro, right?
  (declare (type boolean p-controller-p i-controller-p d-controller-p))
  (let ((configuration-list (list p-controller-p i-controller-p d-controller-p)))
    ;; make sure that at least two of the controller types are present
    (if (>= 1 (reduce #'+ configuration-list :key (lambda (elem) (if elem 1 0))))
        (warn "DEF-COMPOUND-CONTROLLER with less than 2 controllers: p: ~a, i: ~a, d: ~d."
              p-controller-p i-controller-p d-controller-p)
        (labels ((name-prefix (p i d)
                   (concatenate 'string (when p "P") (when i "I") (when d "D")))
                 (to-symbol (name) (intern (string-upcase name)))
                 (to-keyword (name) (intern (string-upcase name) "KEYWORD"))
                 (controller-name (prefix)
                   (concatenate 'string prefix "-controller"))
                 (controller-symbol (prefix)
                   (to-symbol (controller-name prefix)))
                 (slot-name (pre) (concatenate 'string (string pre) "-controller"))
                 (slot-names (prefix) (map 'list #'slot-name prefix))
                 (slot-symbols (prefix) (mapcar #'to-symbol (slot-names prefix)))
                 (slot-keywords (prefix) (mapcar #'to-keyword (slot-names prefix)))
                 ;; CLASS DEFINITION STUFF
                 (controller-slot-specifier (prefix)
                   (mapcar (lambda (slot-symbol slot-keyword)
                             `(,slot-symbol :initform (make-instance ',slot-symbol)
                                            :initarg ,slot-keyword
                                            :accessor ,slot-symbol
                                            :type ,slot-symbol))
                           (slot-symbols prefix) (slot-keywords prefix)))
                 (controller-doc-string (prefix)
                   (concatenate 'string "A simple compound " (controller-name prefix) "."))
                 (controller-class-definition (prefix)
                   `(defclass ,(controller-symbol prefix) () 
                      ,(controller-slot-specifier prefix)
                      (:documentation ,(controller-doc-string prefix))))
                 ;; CONSTRUCTOR STUFF
                 (constructor-name (prefix)
                   (concatenate 'string "make-" (controller-name prefix)))
                 (constructor-symbol (prefix)
                   (to-symbol (constructor-name prefix)))
                 (constructor-body (prefix)
                   `(make-instance
                     ',(controller-symbol prefix)
                     ,@(apply #'append 
                              (mapcar (lambda (slot-keyword slot-symbol)
                                        `(,slot-keyword ,slot-symbol))
                                      (slot-keywords prefix) (slot-symbols prefix)))))
                 (controller-constructor (prefix)
                   `(defun ,(constructor-symbol prefix)
                        ,(slot-symbols prefix)
                      ,(constructor-body prefix)))
                 ;; COPY CONSTRUCTOR STUFF
                 (aux-slot-name (prefix) 
                   (concatenate 'string "old-" (string prefix) "-controller"))
                 (aux-slot-names (prefix) (map 'list #'aux-slot-name prefix))
                 (aux-slot-symbols (prefix) (mapcar #'to-symbol (aux-slot-names prefix)))
                 (copier-name (prefix) 
                   (concatenate 'string "copy-" (controller-name prefix)))
                 (copier-symbol (prefix) (to-symbol (copier-name prefix)))
                 (copier-lambda (prefix)
                   `(,(controller-symbol prefix) &key ,@(slot-symbols prefix)))
                 (copier-body (prefix)
                   `(with-slots 
                          ,(mapcar (lambda (slot-symbol aux-slot-symbol)
                                     `(,aux-slot-symbol ,slot-symbol))
                            (slot-symbols prefix) (aux-slot-symbols prefix))
                        ,(controller-symbol prefix)
                      (,(constructor-symbol prefix)
                       ,@(mapcar (lambda (slot-symbol aux-slot-symbol)
                                   `(or ,slot-symbol ,aux-slot-symbol))
                                 (slot-symbols prefix) (aux-slot-symbols prefix)))))
                 (controller-copier (prefix)
                   `(defun ,(copier-symbol prefix) ,(copier-lambda prefix)
                      ,(copier-body prefix)))
                 ;; COMPUTE-COMMAND STUFF
                 (computation-body (prefix) 
                   `(with-slots ,(slot-symbols prefix) ,(controller-symbol prefix)
                      (+ ,@(mapcar (lambda (slot-symbol)
                                     `(compute-command ,slot-symbol :error error :dt dt))
                                   (slot-symbols prefix)))))
                 (controller-computation (prefix)
                   `(defmethod compute-command 
                        (,(controller-symbol prefix) &key error dt &allow-other-keys)
                      ,(computation-body prefix))))
          ;; ACTUAL MACRO
          `(progn
             ,(controller-class-definition 
               (name-prefix p-controller-p i-controller-p d-controller-p))

             ,(controller-constructor
               (name-prefix p-controller-p i-controller-p d-controller-p))

             ,(controller-copier
               (name-prefix p-controller-p i-controller-p d-controller-p))

             ,(controller-computation
               (name-prefix p-controller-p i-controller-p d-controller-p)))))))
             

;;;
;;; COMPOUND CONTROLLERS
;;;

;; PI-CONTROLLER
(def-compound-controller t t nil)

;; PD-CONTROLLER
(def-compound-controller t nil t)

;; PID-CONTROLLER
(def-compound-controller t t t)