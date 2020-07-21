;; https://github.com/icfpcontest2020/starterkit-commonlisp
(load "quicklisp/setup")
;; (ql:system-apropos-list "split") ;; Searching for stuff

(ql:quickload "split-sequence")
(ql:quickload "cl-ppcre")
(ql:quickload "drakma") ;; HTTP http://quickdocs.org/drakma/

(ql:quickload "named-readtables")
(ql:quickload "trivia")
(ql:quickload "cl-pattern")


;; ironclad, crypto http://quickdocs.org/ironclad/
;; drakma alexandria split-sequence serapeum esrap cl-ppcre babel flexi-streams closer-mop cffi ironclad opticl cl-wav
;; opticl -> image processing
;; cl-wav -> wav files
;; babel -> libiconv similar

(cl:defpackage :evaluate
  (:use :common-lisp :cl-ppcre :named-readtables :split-sequence :cl-pattern)
  )

;; "smug" is a monadic parser combinator library

(in-package :evaluate)

;; https://github.com/vsedach/Vacietis/blob/master/compiler/reader.lisp
;; (defreadtable syntax
;;   (:macro-char)
;;   (:case :preserve))

;; (defvar *foo* ":1029 = ap ap cons 7 ap ap cons 123229502148636 nil")

;; https://message-from-space.readthedocs.io/en/latest/implementation.html


;;; Parsing

;; (define-condition parse-error (error)
;;   ((message :text message input)))


(defun ops-to-ast (syms)
  "Assuming valid SYMS, produce a tree where aps are cons cells of (f, arg)"
  (destructuring-bind (op &rest rest) syms
    (case op
      ('ap
       (multiple-value-bind (f after-f) (ops-to-ast rest)
         (multiple-value-bind (arg after-arg) (ops-to-ast after-f)
           (values (cons f arg) after-arg))))
      (t (values op rest)))))

;; https://github.com/guicho271828/trivia/wiki/Type-Based-Destructuring-Patterns

;; https://github.com/arielnetworks/cl-pattern


(defun eval-expr (expr)
  "Eval an expression, in AST form"
  (match expr
    (((('s . x) . y) . z) (cons (cons z x) (cons y x)))
    (((('c . x) . y) . z) (cons (cons z x) y))
    (((('b . x) . y) . z) (cons z (cons y x)))
    ((((f . x) . y) . z) (cons (cons (cons (eval-expr f) x) y) z))
    ((('true . _) . y) y)
    ((('false . x) . _) x)
    ((('add . x) . y) (+ (eval-fully x) (eval-fully y)))
    ((('mul . x) . y) (* (eval-fully x) (eval-fully y)))
    ((('div . x) . y) (/ (eval-fully y) (eval-fully x)))
    ((('lt . x) . y) (if (< (eval-fully y) (eval-fully x)) 'true 'false))
    ((('eq . x) . y) (if (= (eval-fully x) (eval-fully y)) 'true 'false))
    ((('cons . x) . y) (cons (cons 'cons (eval-fully y)) (eval-fully x)))
    (((f . x) . y) (cons (cons (eval-expr f) x) y))
    (('i . x) x)
    (('inc . x) (+ 1 (eval-fully x)))
    (('neg . x) (* -1 (eval-fully x)))
    (('nul . x) 'true)
    (('isnil . x) (cons x (cons 'true (cons 's 'false))))
    (('car . x) (cons x 'true))
    (('cdr . x) (cons x 'false))
    (('cons . x) expr)
    ((f . x) (cons (eval-fully f) x))
    (_ expr)))

(defun eval-fully (expr)
  (let ((expanded (eval-expr expr)))
    (if (equalp expr expanded)
        (progn
          (print expanded)
          expanded)
        (eval-fully expanded))))

;; (eval-expr '((cons 7 ())))

;; (eval-expr (ops-to-ast '(ap ap cons 7 ap ap cons 123229502148636 nul)))

;; (eval-fully 1)
;; (eval-fully (cons 'isnil (cons 'inc 'nul)))

;; (match '(inc . 1)
;;   (('inc . x) x)
;;   )


;; (eval-expr '(inc . 1))

;; (eval-fully (ops-to-ast '(ap inc ap inc 1 1)))

;; (eval-expr (ops-to-ast '(ap ap cons 7 ap ap cons 123229502148636 nul)))





;; (defun parse-def (line)
;;   "Parse equation definition line into an AST."
;;   (destructuring-bind (name assign &rest ops) (split-sequence:split-sequence #\Space line)
;;     (unless (string-equal assign "=")
;;       (error 'parse-error ))
;;     (cons (intern name) (parse-ops (mapcar #'read-from-string ops)))))

;; (defun parse-ops (ops)
;;   (declare (type ops (list (or 'symbol 'fixnum))))
;;   (unless ops
;;     (error ))
;;   (destructuring-bind (op &rest others) ops
;;     (if (eq 'ap op)
;;         (destructuring-bind (next &rest rest) others
;;           (cons (compile-ops (list next others))
;;                 (compile-ops others)))
;;         (t (error "unsupported op: ~s" op)))))


