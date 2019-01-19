;;;; ruby-types.lisp
;;;; Created by Ninjacop123 
;;;; Wednesday, January 16th, 2019
;;;; Ruby to CL variable related functions
(in-package #:cl-ruby)

;; An alist to store declared Ruby values for when they need to be called
(defparameter *declared-variables* '())

;; An alist to store declared Ruby constant values for when they 
;; need to be called
(defparameter *declared-constants* '())

(defmacro defrubyvar (var-name type-key &optional value)
  "DEFines a RUBY VARiable dependent on the type given in the form 
   of a key."
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

(defun defrubygconst (const-name type-key value)
  "DEFines a RUBY Global CONSTant dependent on the type given in the form
   of a key."
  (case type-key 
    (:int (if (typep value 'integer) ;; numberp captures floats
              (progn 
                (int-const const-name value)
                (push (cons const-name 'integer) *declared-constants*))
              (error "~S is not an integer." value)))
    (:float (if (floatp value)
               (progn 
                 (float-const const-name value)
                 (push (cons const-name 'float) *declared-constants*))
               (error "~S is not a floating point number or double." value)))
    (:string (if (stringp value)
                 (progn 
                   (str-const const-name value)
                   (push (cons const-name 'string) *declared-constants*))
                 (error "~S is not a string." const-name)))
    (otherwise (error "~S is not a valid type key." type-key))))

(defun defrubymconst (module-name const-name type-key value)
  "DEFines a RUBY Module CONSTant dependent on the type given in the form 
   of a key."
   (let ((module-ptr (cffi:translate-to-foreign (module module-name) 'uintptr_t)))
      (case type-key 
        (:int (if (typep value 'integer) ;; numberp captures floats
                  (progn 
                    (int-module-const module-ptr const-name value)
                    (push (cons const-name 'integer) *declared-constants*))
                  (error "~S is not an integer." value)))
        (:float (if (floatp value)
                   (progn 
                     (float-module-const module-ptr const-name value)
                     (push (cons const-name 'float) *declared-constants*))
                   (error "~S is not a floating point number or double." value)))
        (:string (if (stringp value)
                     (progn 
                       (str-module-const module-ptr const-name value)
                       (push (cons const-name 'string) *declared-constants*))
                     (error "~S is not a string." const-name)))
        (otherwise (error "~S is not a valid type key." type-key)))))

(defun find-var (var alist)
  "Search through an alist to find if `var` matches any 
   keys of the alist, or the `car` of each entry. If 
   the entry exists, return its type."
  (let ((some-var (car alist)))
    (if (member var some-var :test #'equalp) ; not case-sensitive
        (cdr some-var) ; print the type of the variable that has been found
        (if alist ; if there are still variables in the alist
            (find-var var (cdr alist))
            (error "~S is and undeclared variable." var)))))

(defun var-call (var-name)
  "Check if a given variable `var-name` exists, then depending
   on its type, call a certain function that returns a decrypted
   Ruby C VALUE."
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

(defun const-call (var-name)
  "Check if a given constant `var-name` exists, then depending 
   on its type, call a certain function that returns a decrypted 
   Ruby C VALUE."
  (let ((var-type (find-var var-name *declared-constants*)))
    (if var-type ;; is the constant declared?
        (case var-type
          ('integer (int-const-call var-name))
          ('string (str-const-call var-name))
          ('float (float-const-call var-name))
          (otherwise (error "~S is not a valid variable type" var-type)))
        (error "~S is undefined." var-name))))

(defun module-const-call (module-name var-name)
  "Check if a given constant `var-name` exists, then depending 
   on its type, call a certain function that returns a decrypted 
   Ruby C VALUE."
   (let ((var-type (find-var var-name *declared-constants*))
         (module-ptr (cffi:translate-to-foreign (module module-name) 'uintptr_t)))
     (if var-type 
         (case var-type
           ('integer (int-module-const-call module-ptr var-name))
           ('float (float-module-const-call module-ptr var-name))
           ('string (str-module-const-call module-ptr var-name))
           (otherwise (error "~S is not a valid variable type." var-type)))
          (error "~S is undefined." var-name))))