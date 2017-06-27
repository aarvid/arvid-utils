(in-package :arvid-utils)

;; from my own idiot brain
;; maybe should be macro? not likely to map/funcall this.
(defun make-adjustable-string (size &key (element-type 'character)
                                         (initial-element #\Space)
                                         (initial-contents nil initial-contents-p))
  "similar to make-string "
  (if initial-contents-p
        (make-array (list size)
              :initial-contents initial-contents
              :element-type element-type
              :fill-pointer t
              :adjustable t)
        (make-array (list size)
               :initial-element initial-element
               :element-type element-type
               :fill-pointer t
               :adjustable t)))

;; useful for talking to javascript which has only vectors
(defun multi-dimensional->vectors (array &key (reverse nil))
  "convert a multi-dimensional array into vector of vectors of ...
   indices go from most significant to least
   :reverse t means indices go from least significant to most"
  (labels ((arr->vec (array dims indices)
             (if dims
                 (let* ((dim (car dims))
                        (vec (make-array dim)))
                   (dotimes (i dim)
                     (setf (aref vec i)
                           (arr->vec array (cdr dims) (cons i indices))))
                   vec)
                 (apply #'aref (cons array (funcall (if reverse #'identity #'reverse) indices))))))
    (arr->vec array (funcall (if reverse #'reverse #'identity)
                             (array-dimensions array)) nil)))

;; useful for talking with javascript which has only vectors
(defun vectors->multi-dimensional (vector &key (reverse nil))
  "convert a vector of vectors of ... into multi-dimensional array
   indices go from most significant to least
   :reverse t means indices go from least significant to most"
  (labels ((vector-dimensions (vec)
             (if (vectorp vec)
                 (cons (length vec)
                       (vector-dimensions (aref vec 0)))
                 nil)))
    (let* ((dim-list (vector-dimensions vector))
           (array (make-array (funcall (if reverse #'reverse #'identity)
                                       dim-list))))
      (labels ((vec->arr (vec dims indices)
                 (if dims
                     (dotimes (i (car dims))
                       (vec->arr (aref vec i) (cdr dims) (cons i indices)))
                     (setf (apply #'aref (cons array (funcall (if reverse #'identity #'reverse) indices)))
                           vec))))
        (vec->arr vector dim-list nil)
        array))))



;;; An acceptor that invokes the debugger on errors:
;;; by Andreas Fuchs (hunchentoot-devel list)

;; commented to drop dependency on hunchentoot which has a lot of dependencies.

;; (defclass debuggable-acceptor (hunchentoot:acceptor)
;;     ())

;; (defmethod process-connection ((*acceptor* debuggable-acceptor) (socket t))
;;  (declare (ignore socket))
;;  (handler-bind ((error #'invoke-debugger))
;;    (call-next-method)))

;; (defmethod acceptor-request-dispatcher ((*acceptor* debuggable-acceptor))
;;  (let ((dispatcher (call-next-method)))
;;    (lambda (request)
;;      (handler-bind ((error #'invoke-debugger))
;;        (funcall dispatcher request)))))

;; use library alexandria
;; (defmacro when-let ((var expr) &body body)
;;   `(let ((,var ,expr))
;;      (when ,var
;;        ,@body)))

;; use library alexandria
;; (defmacro when-let* (binds &body body)
;;   (if (null binds)
;;       `(progn ,@body)
;;     `(let (,(car binds))
;;        (if ,(caar binds)
;; 	   (when-let* ,(cdr binds) ,@body)))))

;; ;; use library alexandria
;; (defmacro with-gensyms (syms &body body)
;;   `(let ,(mapcar #'(lambda (s)
;; 		     `(,s (gensym)))
;; 		 syms)
;;      ,@body))


;; alexandria has similar for sequences
(defun list-of-length-p (l n)
  (if (= n 0) 
      (null l)
      (and (consp l)
           (list-of-length-p (rest l) (1- n)))))

(defun singlep (l) 
  (and (consp l) (null (rest l))))

(defun doublep (l)
  (and (consp l) (singlep (rest l))))



;; on-lisp ch 4.3
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;; quickutils.
(defun weave (&rest lists)
  (apply #'mapcan #'list lists))


;; onlisp 4.7 Symbols and Strings
(defun mkstr (&rest args)
  (with-output-to-string (s)
			 (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
		 (intern (make-string 1
				      :initial-element c)))
       (symbol-name sym)))

;; http://www.cl-user.net/asp/en9o/sdataQvUv$7E2RujsDQ3YNypX8yBX8yBXnMq=/sdataQu3F$sSHnB==
;; How to read and process a file -- marc.battyani 
(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))
