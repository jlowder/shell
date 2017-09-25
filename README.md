# shell

This is a collection of macros and functions that can be used to interface with the shell.

## Usage

~~~lisp
 (collect-shell (cmd linevar)
    body ...)
~~~

Run a shell command; for each line of output, execute a form and
append the result to a list. Return the list and a bool indicating if
the command was successful.

Example:

~~~lisp
 (collect-shell ("who" line)
   (split "\\s+" line))
 
 (("jlowder" "pts/0" "2015-12-24" "09:43" "(:0)")
  ("jlowder" ":0" "2015-12-17" "16:02" "(:0)"))
 T
~~~


~~~lisp
 (with-shell-command (cmd linevar)
    body ...)
~~~

Run a shell command and execute a form on each line of output.

Example:

~~~lisp
 (with-shell-command ("who" line)
   (format t "line: ~A~%" line))

 line: jlowder  :0           2015-12-17 16:02 (:0)
 line: jlowder  pts/0        2015-12-24 09:43 (:0)
 NIL
~~~  


~~~lisp
(shell->rc cmd)
~~~

Execute a shell command and return the result.

Example:

~~~lisp
 (shell->rc "who | grep jlowder")
 
 0

 (shell->rc "who | grep fdjkjkfld")
 
 1
~~~


~~~lisp
(shell->string cmd)
~~~

Execute a shell command. Return the output as a single string, and a bool
indicating whether or not the command was successful.

Example:

~~~lisp
 (shell->string "who")
 
 "jlowder  :0           2015-12-17 16:02 (:0)
 jlowder  pts/0        2015-12-24 09:43 (:0)"
 T
~~~


~~~lisp
(shell->list cmd)
~~~

Execute a shell command. Return the output as a list of strings,
and a bool indicating whether or not the command was successful.

Example:

~~~lisp
 (shell->list "who")
 
 ("jlowder  :0           2015-12-17 16:02 (:0)"
  "jlowder  pts/0        2015-12-24 09:43 (:0)")
 T
~~~


A few anaphoric macros are included to ease the handling of multiple
value returns (ref. On Lisp, Chapter 14). 

~~~lisp
(if_ test-form true-form false-form)
~~~

`test-form` is evaluated and the conditional test is always the second
return value if there are two values, or the first return value for
any other case.  Regardless, the variable `_` is bound to the first
value and is available for use in `true-form`.

Example:

~~~lisp
(if_ (shell->string "who")
   (string-upcase _))

"JLOWDER  :0           2016-02-15 10:20 (:0)
JLOWDER  PTS/0        2016-02-15 10:21 (:0)
JLOWDER  PTS/3        2016-02-15 10:21 (:0)"


(if_ (shell->string "invalid command")
   (string-upcase _))

NIL
~~~


~~~lisp
(when_ test true-form)
~~~

The same as `if_` but without a false-form.

Example:

~~~lisp
(when_ (collect-shell ("who" line)
                    (split "\\s+" line))
     (mapcar #'cadr _))

(":0" "pts/0" "pts/3")
~~~


~~~lisp
(cond_ 
   (test-form body-form) ...)
~~~

This is like a normal `cond` statement except condition determination
is the same as `if_`, and `_` is bound to the evaluated result so that
it is available in each `body-form`.

Example:

~~~lisp
(cond_
 ((shell->list "invalid command") (car _))
 ((+ 3 2) (* _ _))
 (t 7))

25
~~~


## License

MIT
