;;; :FILE-CREATED <Timestamp: #{2012-02-28T20:52:43-05:00Z}#{12092} - by MON>
;;; :FILE image-ops/image-ops-iphone.lisp
;;; ==============================

(in-package #:image-ops)

(defun translate-pathname-iphone-image (pathname-or-namestring)
  (let ((fname (file-namestring pathname-or-namestring)))
    (when (>= (mon:string-length fname) 12)
      (destructuring-bind (img num ext &rest rest) (mon:string-subdivide fname 4)
        (and (not rest)
             (string= img "IMG_")
             (every #'digit-char-p num)
             (string-equal ext ".JPG")
             (make-pathname :directory (pathname-directory pathname-or-namestring)
                            :name (format nil "~A-~D" num (file-write-date pathname-or-namestring))
                            :type "jpg"))))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
