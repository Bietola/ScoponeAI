(load "C:\\quicklisp\\setup.lisp")
(ql:quickload "asdf")

;(require "asdf")

(defpackage scopone
  (:use :cl :asdf))

(in-package scopone)

(defsystem scopone
           :name "sconpone"
           :serial t
           :components ((:file "lazy")
                        (:file "utils")
                        (:file "game-utils")
                        (:file "cards")
                        (:file "game-core")))

(asdf:load-system 'scopone)
