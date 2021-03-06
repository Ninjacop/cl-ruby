;;;; ruby-types.lisp
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019
;;;; CFFI bindings of Ruby classes, modules and exceptions.
;;;; Since Ruby classes are like CL global variables/earmuff'd
;;;; I decided to make the Ruby classes unique by putting `~`s 
;;;; around them.
(in-package #:cl-ruby)
(export (list ; This does the "excellent" job of only exporting these symbols
              ; and nothing else :)
;;; Ruby Modules converted into their CL counterparts (pointers)
(cffi:defcvar ("rb_mKernel" ~kernel~) uintptr_t)
(cffi:defcvar ("rb_mComparable" ~comparable~) uintptr_t)
(cffi:defcvar ("rb_mEnumerable" ~enumerable~) uintptr_t)
(cffi:defcvar ("rb_mErrno" ~errno~) uintptr_t)
(cffi:defcvar ("rb_mFileTest" ~file-test~) uintptr_t)
(cffi:defcvar ("rb_mGC" ~gc~) uintptr_t)
(cffi:defcvar ("rb_mMath" ~math~) uintptr_t)
(cffi:defcvar ("rb_mProcess" ~process~) uintptr_t)
(cffi:defcvar ("rb_mWaitReadable" ~readable~) uintptr_t)
(cffi:defcvar ("rb_mWaitWritable" ~writable~) uintptr_t)

;;; Ruby Classes converted into their CL counterparts (pointers)
(cffi:defcvar ("rb_cBasicObject" ~basic-object~) uintptr_t)
(cffi:defcvar ("rb_cObject" ~object~) uintptr_t)
(cffi:defcvar ("rb_cArray" ~array~) uintptr_t)

(cffi:defcvar ("rb_cBignum" ~bignum~) uintptr_t)

(cffi:defcvar ("rb_cBinding" ~binding~) uintptr_t)
(cffi:defcvar ("rb_cClass" ~class~) uintptr_t)
(cffi:defcvar ("rb_cCont" ~cont~) uintptr_t)
(cffi:defcvar ("rb_cData" ~data~) uintptr_t)
(cffi:defcvar ("rb_cDir" ~dir~) uintptr_t)
(cffi:defcvar ("rb_cEncoding" ~encoding~) uintptr_t)
(cffi:defcvar ("rb_cEnumerator" ~enumerator~) uintptr_t)
(cffi:defcvar ("rb_cFalseClass" ~false~) uintptr_t)
(cffi:defcvar ("rb_cFile" ~file~) uintptr_t)

(cffi:defcvar ("rb_cFixnum" ~fixnum~) uintptr_t)

(cffi:defcvar ("rb_cComplex" ~complex~) uintptr_t)
(cffi:defcvar ("rb_cFloat" ~float~) uintptr_t)
(cffi:defcvar ("rb_cHash" ~hash~) uintptr_t)
(cffi:defcvar ("rb_cIO" ~io~) uintptr_t)
(cffi:defcvar ("rb_cInteger" ~integer~) uintptr_t)
(cffi:defcvar ("rb_cMatch" ~match~) uintptr_t)
(cffi:defcvar ("rb_cMethod" ~method~) uintptr_t)
(cffi:defcvar ("rb_cModule" ~module~) uintptr_t)
(cffi:defcvar ("rb_cNameErrorMesg" ~error-mesg~) uintptr_t)
(cffi:defcvar ("rb_cNilClass" ~nil~) uintptr_t)
(cffi:defcvar ("rb_cNumeric" ~numeric~) uintptr_t)
(cffi:defcvar ("rb_cProc" ~proc~) uintptr_t)
(cffi:defcvar ("rb_cRandom" ~random~) uintptr_t)
(cffi:defcvar ("rb_cRange" ~range~) uintptr_t)
(cffi:defcvar ("rb_cRational" ~rational~) uintptr_t)
(cffi:defcvar ("rb_cRegexp" ~regexp~) uintptr_t)
(cffi:defcvar ("rb_cStat" ~stat~) uintptr_t)
(cffi:defcvar ("rb_cString" ~string~) uintptr_t)
(cffi:defcvar ("rb_cStruct" ~struct~) uintptr_t)
(cffi:defcvar ("rb_cSymbol" ~symbol~) uintptr_t)
(cffi:defcvar ("rb_cThread" ~thread~) uintptr_t)
(cffi:defcvar ("rb_cTime" ~time~) uintptr_t)
(cffi:defcvar ("rb_cTrueClass" ~true~) uintptr_t)
(cffi:defcvar ("rb_cUnboundMethod" ~unbound-method~) uintptr_t)

;;; Ruby Exception Types and their CL counterparts (pointers)
(cffi:defcvar ("rb_eException" ~exception~) uintptr_t)
(cffi:defcvar ("rb_eStandardError" ~stderr~) uintptr_t)
(cffi:defcvar ("rb_eSystemExit" ~sysexit~) uintptr_t)
(cffi:defcvar ("rb_eInterrupt" ~interrupt~) uintptr_t)
(cffi:defcvar ("rb_eSignal" ~signal~) uintptr_t)
(cffi:defcvar ("rb_eFatal" ~fatal~) uintptr_t)
(cffi:defcvar ("rb_eArgError" ~arg-error~) uintptr_t)
(cffi:defcvar ("rb_eEOFError" ~eof-error~) uintptr_t)
(cffi:defcvar ("rb_eIndexError" ~index-error~) uintptr_t)
(cffi:defcvar ("rb_eStopIteration" ~stop-iteration~) uintptr_t)
(cffi:defcvar ("rb_eKeyError" ~key-error~) uintptr_t)
(cffi:defcvar ("rb_eRangeError" ~range-error~) uintptr_t)
(cffi:defcvar ("rb_eIOError" ~io-error~) uintptr_t)
(cffi:defcvar ("rb_eRuntimeError" ~runtime-error~) uintptr_t)
(cffi:defcvar ("rb_eFrozenError" ~frozen-error~) uintptr_t)
(cffi:defcvar ("rb_eSecurityError" ~security-error~) uintptr_t)
(cffi:defcvar ("rb_eSystemCallError" ~syscall-error~) uintptr_t)
(cffi:defcvar ("rb_eThreadError" ~thread-error~) uintptr_t)
(cffi:defcvar ("rb_eTypeError" ~type-error~) uintptr_t)
(cffi:defcvar ("rb_eZeroDivError" ~zero-div-error~) uintptr_t)
(cffi:defcvar ("rb_eNotImpError" ~not-imp-error~) uintptr_t)
(cffi:defcvar ("rb_eNoMemError" ~no-mem-error~) uintptr_t)
(cffi:defcvar ("rb_eNoMethodError" ~no-method-error~) uintptr_t)
(cffi:defcvar ("rb_eFloatDomainError" ~float-domain-error~) uintptr_t)
(cffi:defcvar ("rb_eLocalJumpError" ~local-jump-error~) uintptr_t)
(cffi:defcvar ("rb_eSysStackError" ~sys-stack-error~) uintptr_t)
(cffi:defcvar ("rb_eRegexpError" ~regexp-error~) uintptr_t)
(cffi:defcvar ("rb_eEncodingError" ~encoding-error~) uintptr_t)
(cffi:defcvar ("rb_eEncCompatError" ~enc-compat-error~) uintptr_t)

(cffi:defcvar ("rb_eScriptError" ~script-error~) uintptr_t)
(cffi:defcvar ("rb_eNameError" ~name-error~) uintptr_t)
(cffi:defcvar ("rb_eSyntaxError" ~sytax-error~) uintptr_t)
(cffi:defcvar ("rb_eLoadError" ~load-error~) uintptr_t)

(cffi:defcvar ("rb_eMathDomainError" ~math-domain-error~) uintptr_t)))