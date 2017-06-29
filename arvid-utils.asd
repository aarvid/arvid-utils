;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem "arvid-utils"
  :version "0.0.0"
  :maintainer "A. Peterson"
  :author "Andy Peterson"
  :licence "BSD sans advertising clause (see file COPYING for details)"
  :description "General Utilities"
  :long-description "Lisp utility functions"
  :depends-on ()
  :serial t ;; the dependencies are linear.
  :components ((:file "package")
               (:file "utils")))


