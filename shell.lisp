(in-package :cl-user)

(defpackage :shell
  (:use :common-lisp
        :cl-ppcre)
  (:export :collect-shell
           :with-shell-command
           :shell->rc
           :shell->string
           :shell->list))

(in-package :shell)

(defmacro collect-shell ((cmd line) &body body)
  "run a shell command. for each line of output, execute a form and append the result to a list. return the list."
  `(loop for ,line in
        (split #\newline (uiop:run-program (list "/bin/sh" "-c" ,cmd) 
                                           :output :string
                                           :ignore-error-status t))
      for a = nil then b
      as b = (cons ,@body a)
      finally (return b)))

;; example:
;; (collect-shell ("who" line)
;;   (split "\\s+" line))
;; 
;; (("jlowder" "pts/0" "2015-12-24" "09:43" "(:0)")
;;  ("jlowder" ":0" "2015-12-17" "16:02" "(:0)"))

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
  "execute a shell command and return the output as a single string"
  (multiple-value-bind (a b c) 
      (uiop:run-program (list "/bin/sh" "-c" cmd)
                        :output :string
                        :ignore-error-status t)
    (declare (ignore b c)) a))
             
;; example:  
;; (shell-command-string "who")
;; 
;; "jlowder  :0           2015-12-17 16:02 (:0)
;; jlowder  pts/0        2015-12-24 09:43 (:0)
;; "

(defun shell->list (cmd)
  "execute a shell command and return the output as a list of strings"
  (split #\newline
         (uiop:run-program (list "/bin/sh" "-c" cmd)
                           :output :string
                           :ignore-error-status t)))

;; example:
;; (shell->list "who")
;; 
;; ("jlowder  :0           2015-12-17 16:02 (:0)"
;;  "jlowder  pts/0        2015-12-24 09:43 (:0)")
