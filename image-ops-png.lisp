;;; :FILE-CREATED <Timestamp: #{2012-05-29T12:34:06-04:00Z}#{12222} - by MON>
;;; :FILE image-ops/image-ops-png.lisp
;;; ==============================

;; (ql:quickload '(zpng png-read))

(in-package #:image-ops)

;; :PASTE-URL (URL `http://paste.lisp.org/display/129604')
;; :PASTE-DATE 2012-05-23
;; :PASTE-AUTHOR stassats
(defun png-filter (source-png dest-png filter-func)
  (macrolet ((colors (array &rest ns)
               `(values ,@(loop for i in ns
                                collect `(aref ,array row column ,i)))))
    (loop for row below (png-read:height source-png)
          do
          (loop for column by 4 below (png-read:width source-png)
                do
                (setf (colors (zpng:data-array dest-png) 0 2 1 3)
                      (multiple-value-call filter-func
                        (colors (png-read:image-data source-png)
                                0 1 2 3)))))))
;; :PASTE-DATE 2012-05-23
;; :PASTED-AUTHOR robot-beethoven
;; :PASTE-URL (URL `http://paste.lisp.org/+2RZZ')

(defun png-rgba-paint (png painter-func)
  (do ((row 0 (1+ row)))
      ((= row (zpng:height png)))
    (do ((column 0 (+ column 4)))
	 ((= column (zpng:width png)))
      (funcall painter-func
	       (aref (zpng:data-array png) row column 0)
	       (aref (zpng:data-array png) row column 1)
	       (aref (zpng:data-array png) row column 2)
	       (aref (zpng:data-array png) row column 3)))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:


;;; ==============================
;;; EOF
