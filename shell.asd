(defsystem shell
    :name "shell"
    :version "0.1.0"
    :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
    :license "MIT"
    :description "Various ways to execute shell commands"
    :depends-on (:cl-ppcre)
    :components ((:file "shell")))
