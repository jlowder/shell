(in-package :cl-user)

(defpackage :shell
  (:use :common-lisp
        :cl-ppcre)
  (:export :collect-shell
           :with-shell-command
           :shell->rc
           :shell->string
           :shell->list
           :_
           :if_
           :when_
           :cond_)

(in-package :shell)

(defmacro collect-shell ((cmd line) &body body)
  "run a shell command. for each line of output, execute a form and
append the result to a list. return the list and a bool indicating
if the command was successful."
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(multiple-value-bind (,a ,b ,c) (uiop:run-program (list "/bin/sh" "-c" ,cmd) 
                                                       :output :string
                                                       :ignore-error-status t)
       (declare (ignorable ,b))
       (values (loop 
                  for ,line in (split #\newline ,a)
                  for x = nil then y
                  as y = (cons ,@body x)
                  finally (return y))
               (eq ,c 0)))))

;; example:
;; (collect-shell ("who" line)
;;   (split "\\s+" line))
;; 
;; (("jlowder" "pts/0" "2015-12-24" "09:43" "(:0)")
;;  ("jlowder" ":0" "2015-12-17" "16:02" "(:0)"))
;; T

(defmacro with-shell-command ((cmd line) &body body)
  "run a shell command and execute a form on each line of output."
  `(loop for ,line in
        (split #\newline (uiop:run-program (list "/bin/sh" "-c" ,cmd) 
                                           :output :string
                                           :ignore-error-status t))
      while ,line
      do ,@body))

;; example:
;; (with-shell-command ("who" line)
;;   (format t "line: ~A~%" line))
;; line: jlowder  :0           2015-12-17 16:02 (:0)
;; line: jlowder  pts/0        2015-12-24 09:43 (:0)
;; NIL

(defun shell->rc (cmd)
  "execute a shell command and return the exit code"
  (multiple-value-bind (a b c) 
      (uiop:run-program (list "/bin/sh" "-c" cmd) :output nil :ignore-error-status t)
    (declare (ignore a b)) c))

;; example:
;; (shell-command-rc "who | grep jlowder")
;; 
;; 0
;; (shell-command-rc "who | grep fdjkjkfld")
;; 
;; 1

(defun shell->string (cmd)
  "execute a shell command. return the output as a single string, and
a bool indicating whether or not the command was successful."
  (multiple-value-bind (a b c) 
      (uiop:run-program (list "/bin/sh" "-c" cmd)
                        :output :string
                        :ignore-error-status t)
    (declare (ignorable b))
    (values (string-trim (string #\newline) a) (eq 0 c))))
             
;; example:  
;; (shell-command-string "who")
;; 
;; "jlowder  :0           2015-12-17 16:02 (:0)
;; jlowder  pts/0        2015-12-24 09:43 (:0)"
;; T

(defun shell->list (cmd)
  "execute a shell command. return the output as a list of strings,
and a bool indicating whether or not the command was successful."
  (multiple-value-bind (a b c) 
      (uiop:run-program (list "/bin/sh" "-c" cmd)
                        :output :string
                        :ignore-error-status t)
    (declare (ignorable b))
    (values (split #\newline a) (eq 0 c))))

;; example:
;; (shell->list "who")
;; 
;; ("jlowder  :0           2015-12-17 16:02 (:0)"
;;  "jlowder  pts/0        2015-12-24 09:43 (:0)")
;; T

;; here are a couple of anaphoric macros to ease the handling of multiple value returns
;; (ref. On Lisp, Chapter 14)
(defmacro if_ (if then &optional else)
  (let ((res (gensym)))
    `(multiple-value-bind (_ ,res) ,if
       (if (or ,res _) ,then ,else))))

(defmacro when_ (if then)
  (let ((res (gensym)))
    `(multiple-value-bind (_ ,res) ,if
       (when (or ,res _) ,then))))

(defmacro cond_ (&rest clauses)
  (when clauses
    (let ((cl1 (car clauses))
          (l (gensym)))
      `(let ((,l (multiple-value-list ,(car cl1))))
         (if (eq 2 (length ,l))
             (if (cadr ,l)
                 (let ((_ (car ,l)))
                   ,@(cdr cl1))
                 (cond_ ,@(cdr clauses)))
             (if_ (car ,l)
                  ,@(cdr cl1)
                  (cond_ ,@(cdr clauses))))))))

;; example:
;; (when_ (collect-shell ("who" line)
;;                     (split "\\s+" line))
;;      (mapcar #'cadr _))
;; 
;; ("pts/3" "pts/0" ":0")
;;
;;
;; (cond_
;;  ((shell->list "invalid command") (car _))
;;  ((+ 3 2) (* _ _))
;;  (t 7))
;;
;; 25
