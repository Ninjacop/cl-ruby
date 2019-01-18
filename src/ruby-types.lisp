;;;; ruby-types.lisp
;;;; Created by Ninjacop123 
;;;; Wednesday, January 16th, 2019
;;;; Ruby to CL variable related functions
(in-package #:cl-ruby)

;; An alist to store declared ruby values for when they need to be called
(defparameter *declared-variables* '())

(defmacro defrubyvar (var-name type-key &optional value)
  (case type-key
    (:int (if (typep value 'integer) ;; numberp captures floats
              `(progn 
                (define-int ,var-name ,value)
                (push (cons ,var-name 'integer) *declared-variables*)) ;todo cons v push
              (error "~S is not an integer." value)))
    (:string (if (stringp value)
                 `(progn 
                    (define-string ,var-name ,value)
                    (push (cons ,var-name 'string) *declared-variables*))
                 (error "~S is not a string." value)))
    (:float (if (floatp value)
                `(progn 
                    (define-float ,var-name ,value)
                    (push (cons ,var-name 'float) *declared-variables*))
                (error "~S is not a floating point number or double." value)))
    (:array `(progn (define-array ,var-name)
              (push (cons ,var-name 'array) *declared-variables*)))
    (:hash `(progn (define-hash ,var-name)
             (push (cons ,var-name 'hash) *declared-variables*)))
    (otherwise (error "~S is not a valid type key." type-key))))

(defun find-var (var alist)
  (let ((some-var (car alist)))
    (if (member var some-var :test #'equalp)
        (cdr some-var) ;; print the type of the variable that has been found
        (if alist ;; if there are still variables in the alist
            (find-var var (cdr alist))
            (error "~S is and undeclared variable." var)))))

(defun var-call (var-name)
  (let ((var-type (find-var var-name *declared-variables*)))
    (if var-type ;; is the variable declared?
        (case var-type
          ('integer (int-call var-name))
          ('float (float-call var-name))
          ('string (str-call var-name))
          ('array (array-call var-name))
          ('hash (hash-call var-name))
          (otherwise (error "~S is not a valid variable type" var-type)))
        (error "~S is undefined." var-name))))