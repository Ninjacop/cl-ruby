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

;;; Ruby <-> CL conversions
(defun rbstr->clstr (str)
  "Take the pointer to a Ruby string, and convert it into
   a Common Lisp string."
  (cffi:with-foreign-object (str-ptr '(:pointer uintptr_t))
    (setf (cffi:mem-ref str-ptr 'uintptr_t) str)
    (rb-string-value-cstr str-ptr)))

(defun undef-const (name)
  "Undefine a constant, or set its value to Ruby's NIL"
  (ruby-define-gconst name (ruby-nil)))

(defun undef-module-const (module-name name)
  (ruby-define-mconst module-name name (ruby-nil)))

(defun defrubyvar (var-name type-key &optional value)
  "DEFines a RUBY VARiable dependent on the type given in the form 
   of a key."
  (case type-key
    (:int (if (typep value 'integer) ;; numberp captures floats
              (progn 
                (push (cons var-name 'integer) *declared-variables*) ; TODO cons v push
                (ruby-define-global var-name (int2num value)) ; declares/allocates the pointer/variable
                (int2num value)) ; return the value so it can be used
              (error "~S is not an integer." value)))
    (:string (if (stringp value)
                 (progn 
                    (push (cons var-name 'string) *declared-variables*)
                    (ruby-define-global var-name (clstr->rbstr value))
                    (clstr->rbstr value))
                 (error "~S is not a string." value)))
    (:float (if (floatp value)
                (progn (push (cons var-name 'float) *declared-variables*)
                    (ruby-define-global var-name (dbl2num value))
                    (dbl2num value))
                (error "~S is not a floating point number or double." value)))
    (:array (progn (push (cons var-name 'array) *declared-variables*)
                   (if (and (eq (car value) :len) (numberp (cadr value)))
                        (progn (if (equal (length value) 2) ; if it only contains '(:len num)
                                   (ruby-define-global var-name
                                      (ruby-new-array-length (int2num (cadr value))))
                                   (if (> (length value) 2)
                                          (error "Invalid number of arguments to DEFRUBYMCONST (~S)" (length value)))))
                        (ruby-define-global var-name (ruby-new-array)))
                    (ruby-get-global var-name)))
    (:hash (progn
             (push (cons var-name 'hash) *declared-variables*)
             (ruby-define-global var-name (ruby-new-hash))
             (ruby-get-global var-name)))
    (otherwise (error "~S is not a valid type key." type-key))))

(defun defrubygconst (const-name type-key &optional value)
  "DEFines a RUBY Global CONSTant dependent on the type given in the form
   of a key."
  (case type-key 
    (:int (if (typep value 'integer) ;; numberp captures floats
              (progn (push (cons const-name 'integer) *declared-constants*)
                (ruby-define-gconst const-name (int2num value))
                (int2num value))
              (error "~S is not an integer." value)))
    (:float (if (floatp value)
               (progn (push (cons const-name 'float) *declared-constants*)
                 (ruby-define-gconst const-name (dbl2num value))
                 (dbl2num value))
               (error "~S is not a floating point number or double." value)))
    (:string (if (stringp value)
                 (progn (push (cons const-name 'string) *declared-constants*)
                   (ruby-define-gconst const-name (clstr->rbstr value))
                   (clstr->rbstr value))
                 (error "~S is not a string." const-name)))
    (:array  (progn (push (cons const-name 'array) *declared-constants*)
                    (ruby-define-gconst const-name (ruby-new-array))
                    (ruby-get-const ~object~ (ruby-intern const-name))))
    (:hash (progn (push (cons const-name 'hash) *declared-constants*)
                  (if (and (eq (car value) :len) (numberp (cadr value)))
                      (progn (if (equal (length value) 2) ; if it only contains '(:len num)
                                 (ruby-define-gconst ~object~ (ruby-intern const-name) 
                                    (ruby-new-array-length (int2num (cadr value))))
                                 (if (> (length value) 2)
                                        (error "Invalid number of arguments to DEFRUBYMCONST (~S)" (length value)))))
                      (ruby-define-gconst ~object~ (ruby-intern const-name) (ruby-new-array)))
                  (ruby-get-const ~object~ (ruby-intern const-name))))
    (otherwise (error "~S is not a valid type key." type-key))))

(defun defrubymconst (module-name const-name type-key &optional value)
  "DEFines a RUBY Module CONSTant dependent on the type given in the form 
   of a key."
   (let ((module-ptr (module module-name)))
      (case type-key 
        (:int (if (typep value 'integer) ;; numberp captures floats
                  (progn 
                    (push (cons const-name 'integer) *declared-constants*)
                    (ruby-define-mconst module-ptr const-name (int2num value))
                    (int2num value))
                  (error "~S is not an integer." value)))
        (:float (if (floatp value)
                   (progn 
                     (push (cons const-name 'float) *declared-constants*)
                     (ruby-define-mconst module-ptr const-name (dbl2num value))
                     (dbl2num value))
                   (error "~S is not a floating point number or double." value)))
        (:string (if (stringp value)
                     (progn 
                       (push (cons const-name 'string) *declared-constants*)
                       (ruby-define-mconst module-ptr const-name (clstr->rbstr value))
                       (clstr->rbstr value))
                     (error "~S is not a string." const-name)))
        (:array  (progn (push (cons const-name 'array) *declared-constants*)
                        (if (and (eq (car value) :len) (numberp (cadr value)))
                            (progn (if (equal (length value) 2) ; if it only contains '(:len num)
                                       (ruby-define-mconst module-ptr const-name
                                           (ruby-new-array-length (int2num (cadr value))))
                                       (if (> (length value) 2)
                                           (error "Invalid number of arguments to DEFRUBYMCONST (~S)" (length value)))))
                            (ruby-define-mconst module-ptr const-name (ruby-new-array)))
                        (ruby-get-const module-ptr const-name)))
        (:hash (progn (push (cons const-name 'hash) *declared-constants*)
                      (ruby-define-mconst module-ptr const-name (ruby-new-hash))
                      (ruby-get-const module-ptr const-name)))
        (otherwise (error "~S is not a valid type key." type-key)))))