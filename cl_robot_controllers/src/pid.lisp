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

(defun def-controller-class (name-prefix)
  (labels ((generate-class-name (prefix)
             (string-upcase (concatenate 'string prefix "-controller")))
           (generate-class-symbol (prefix)
             (intern (generate-class-name prefix)))
           (generate-slot-list (prefix)
             (map 'list 
                  (lambda (pre) 
                    (let* ((slot-name (string-upcase
                                       (concatenate 
                                        'string (string pre) "-controller")))
                           (slot-symbol (intern slot-name))
                           (slot-keyword (intern slot-name "KEYWORD")))
                      `(,slot-symbol :initform (make-instance ',slot-symbol)
                                     :initarg ,slot-keyword
                                     :accessor ,slot-symbol
                                     :type ,slot-symbol)))
                  prefix))
           (generate-doc-string (prefix)
             (concatenate 'string "A simple compound " (generate-class-name prefix) ".")))
    `(defclass ,(generate-class-symbol name-prefix) () 
       ,(generate-slot-list name-prefix)
       (:documentation ,(generate-doc-string name-prefix)))))
 
;;; TODO(Georg): consider rewriting this by introspecting the type. After all, it should
;;;              already be written and compiled, right?
(defun def-controller-constructor (name-prefix)
  (labels ((to-symbol (name) (intern (string-upcase name)))
           (to-keyword (name) (intern (string-upcase name) "KEYWORD"))
           (controller-name (prefix) (concatenate 'string prefix "-controller"))
           (controller-symbol (prefix) (to-symbol (controller-name prefix)))
           (slot-names (prefix)
             (map 
              'list
              (lambda (pre) (concatenate 'string (string pre) "-controller"))
              prefix))
           (constructor-name (prefix)
             (concatenate 'string "make-" (controller-name prefix)))
           (constructor-symbol (prefix)
             (to-symbol (constructor-name prefix)))
           (constructor-params (prefix)
             (mapcar (lambda (slot-name) (to-symbol slot-name))
                     (slot-names prefix)))
           (init-arguments (prefix)
             (apply #'append 
                    (mapcar (lambda (slot-name)
                              `(,(to-keyword slot-name) ,(to-symbol slot-name)))
                            (slot-names prefix)))))
    `(defun ,(constructor-symbol name-prefix)
         ,(constructor-params name-prefix)
       (make-instance ',(controller-symbol name-prefix)
                      ,@(init-arguments name-prefix)))))

(defun def-controller-copier (name-prefix)
  (labels ((to-symbol (name) (intern (string-upcase name)))
           (controller-name (prefix) (concatenate 'string prefix "-controller"))
           (controller-symbol (prefix) (to-symbol (controller-name prefix)))
           (slot-name (prefix) (concatenate 'string (string prefix) "-controller"))
           (slot-names (prefix) (map 'list #'slot-name prefix))
           (aux-slot-name (prefix) 
             (concatenate 'string "old-" (string prefix) "-controller"))
           (aux-slot-names (prefix) (map 'list #'aux-slot-name prefix))
           (constructor-name (prefix)
             (concatenate 'string "make-" (controller-name prefix)))
           (constructor-symbol (prefix)
             (to-symbol (constructor-name prefix)))
           (copier-name (prefix) (concatenate 'string "copy-" (controller-name prefix)))
           (copier-symbol (prefix) (to-symbol (copier-name prefix)))
           (copier-lambda (prefix)
             `(,(controller-symbol prefix) 
                &key ,@(mapcar #'to-symbol (slot-names prefix))))
           (copier-body (prefix)
             `(with-slots 
                    ,(mapcar (lambda (slot-name aux-slot-name)
                               `(,(to-symbol aux-slot-name) ,(to-symbol slot-name)))
                      (slot-names prefix) (aux-slot-names prefix))
                  ,(controller-symbol prefix)
                (,(constructor-symbol prefix)
                 ,@(mapcar (lambda (slot-name aux-slot-name)
                            `(or ,(to-symbol slot-name) ,(to-symbol aux-slot-name)))
                          (slot-names prefix) (aux-slot-names prefix))))))
    `(defun ,(copier-symbol name-prefix) ,(copier-lambda name-prefix)
       ,(copier-body name-prefix))))

(defun def-controller-computation (name-prefix)
  (labels ((to-symbol (name) (intern (string-upcase name)))
           (controller-name (prefix) (concatenate 'string prefix "-controller"))
           (controller-symbol (prefix) (to-symbol (controller-name prefix)))
           (slot-name (prefix) (concatenate 'string (string prefix) "-controller"))
           (slot-names (prefix) (map 'list #'slot-name prefix))
           (computation-body (prefix) 
             `(with-slots ,(mapcar #'to-symbol (slot-names prefix))
                  ,(controller-symbol prefix)
                (+ ,@(mapcar (lambda (slot-name)
                               `(compute-command ,(to-symbol slot-name)
                                                 :error error :dt dt))
                             (slot-names prefix))))))
    `(defmethod compute-command 
         (,(controller-symbol name-prefix) &key error dt &allow-other-keys)
       ,(computation-body name-prefix))))

(defmacro def-compound-controller (p-controller-p i-controller-p d-controller-p)
  (declare (type boolean p-controller-p i-controller-p d-controller-p))
  (let ((configuration-list (list p-controller-p i-controller-p d-controller-p)))
    ;; make sure that at least two of the controller types are present
    (when (< 1 (reduce #'+ configuration-list :key (lambda (elem) (if elem 1 0))))
      (let ((name-prefix (concatenate 'string
                                       (when p-controller-p "P")
                                       (when i-controller-p "I")
                                       (when d-controller-p "D"))))
        `(progn
          ,(def-controller-class name-prefix)
          ,(def-controller-constructor name-prefix)
          ,(def-controller-copier name-prefix)
          ,(def-controller-computation name-prefix)))
        )))
             

;;;
;;; COMPOUND CONTROLLERS
;;;

;; PI-CONTROLLER
; (def-compound-controller t t nil)

;; PD-CONTROLLER
; (def-compound-controller t nil t)

;; PID-CONTROLLER
; (def-compound-controller t t t)

;;;
;;; REFERENCE IMPLEMENTATION: PID-CONTROLLER
;;; (def-compound-controller t t t) should deliver this:
;;;

;; (defclass pid-controller ()
;;   ((p-controller :initform (make-instance 'p-controller) :initarg :p-controller
;;                  :accessor p-controller :type p-controller)
;;    (i-controller :initform (make-instance 'i-controller) :initarg :i-controller
;;                  :accessor i-controller :type i-controller)
;;    (d-controller :initform (make-instance 'd-controller) :initarg :d-controller
;;                  :accessor d-controller :type d-controller))
;;   (:documentation "A simple compound PID-controller."))

;; (defun make-pid-controller (p-controller i-controller d-controller)
;;   (make-instance 'pid-controller :p-controller p-controller :i-controller i-controller
;;                                  :d-controller d-controller))

;; (defun copy-pid-controller (pid-controller &key p-controller i-controller d-controller)
;;   (with-slots ((old-p-controller p-controller) (old-i-controller i-controller)
;;                (old-d-controller)) pid-controller
;;     (make-pid-controller (or p-controller old-p-controller) 
;;                          (or i-controller old-i-controller)
;;                          (or d-controller old-d-controller))))

;; (defmethod compute-command (pid-controller &key error dt &allow-other-keys)
;;   (with-slots (p-controller i-controller d-controller) pid-controller
;;     (+ (compute-command p-controller :error error :dt dt)
;;        (compute-command i-controller :error error :dt dt)
;;        (compute-command d-controller :error error :dt dt))))