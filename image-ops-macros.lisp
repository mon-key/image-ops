;;; :FILE-CREATED <Timestamp: #{2011-08-29T13:19:20-04:00Z}#{11351} - by MON>
;;; :FILE image-ops/image-ops-macros.lisp
;;; ==============================

(in-package #:image-ops)
;; *package*

(defmacro with-hash-table-op ((hash-var hash-table) &body body)
  #-sbcl `(let ((,hash-var ,hash-table))
            (declare (hash-table ,hash-var))
            ,@body)
  #+sbcl `(let ((,hash-var ,hash-table))
            (declare (hash-table ,hash-var))
            (if (sb-ext:hash-table-synchronized-p ,hash-var)
                (sb-ext:with-locked-hash-table (,hash-var)
                  ,@body)
                ,@body)))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
