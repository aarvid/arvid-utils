(defpackage :arvid-utils
  (:nicknames :arvid :arv)
  (:use :common-lisp )
  (:export
   make-adjustable-string
   multi-dimensional->vectors
   vectors->multi-dimensional
   n-spaces
   list-of-length-p
   singlep
   doublep))
