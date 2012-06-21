;;; :FILE-CREATED <Timestamp: #{2012-05-08T15:30:11-04:00Z}#{12192} - by MON>
;;; :FILE image-ops/image-ops-nikon-nef.lisp
;;; ==============================

(in-package #:image-ops)

(defun translate-pathname-nef-image (pathname-or-namestring &key (case-mode :upcase))
  (declare (type (or (eql :upcase)
                     (eql :downcase)
                     (eql :insensitive)) case-mode))
  (let ((fname (file-namestring pathname-or-namestring)))
    (flet ((ext-compare (extension)
             (ecase case-mode
               (:upcase (string= extension ".NEF"))
               (:downcase (string= extension ".nef"))
               (:insensitive (string-equal extension ".NEF")))))
      (when (>= (mon:string-length fname) 12)
        (destructuring-bind (img num ext &rest rest) (mon:string-subdivide fname 4)
          (and (not rest)
               (string= img "DSC_")
               (every #'digit-char-p num)
               (ext-compare ext)
               (let ((buffer (make-array 27 :element-type 'character :fill-pointer 0)))
                 (with-output-to-string (output buffer)
                   ;;(format output "~4,'0d-" num)
                   (format output "~6,'0d-nk-" (parse-integer num))
                   (mon::format-timestring output (local-time:universal-to-timestamp (file-write-date pathname-or-namestring))
                                           :format mon:*timestamp-for-file-format*))
                 (make-pathname :directory (pathname-directory pathname-or-namestring)
                                :name buffer
                                :type "nef"))))))))

;; (defun directory-nef-images (base-directory &key (wilden nil))
;; (let ((wild-nef-scanner (cl-ppcre:create-scanner "(?i)^nef$" :case-insensitive-mode t)))
;; find all pathnames  beneath BASE-DIRECTORY with pathname-tyeps matching the regular expression
;; \"^nef$\"
(defun directory-nef-images (base-directory &key (wilden nil)
                                                 (case-mode nil))
  (declare (type (or boolean (eql :wild) (eql :wild-inferiors)) wilden)
           (type (or null
                     (eql :upcase)
                     (eql :downcase)
                     (eql :insensitive)) case-mode))
  (let* ((maybe-wilden-directory
           (if (probe-file base-directory)
               (make-pathname :directory (ecase wilden
                                           ((:wild-inferiors :wild)
                                            `(,@(pathname-directory base-directory) ,wilden))
                                           ((t)
                                            `(,@(pathname-directory base-directory) ,:wild))
                                           ((nil) `(,@(pathname-directory base-directory))))
                              :name :wild
                              :type :wild)
               (error ":FUNCTION `directory-nef-images' -- ~
                       arg BASE-DIRECTORY non-existent~% got: ~S"
                      base-directory)))
         (wild-nef-scanner
           (cl-ppcre:create-scanner (ecase case-mode
                                      (:upcase
                                       (cl-ppcre:create-scanner "^NEF$" :case-insensitive-mode nil))
                                      (:downcase
                                       (cl-ppcre:create-scanner "^nef$" :case-insensitive-mode nil))
                                      ((:insensitive nil)
                                       (cl-ppcre:create-scanner "^nef$" :case-insensitive-mode t))))))
    (flet ((maybe-match (path-type)
             (cl-ppcre:scan wild-nef-scanner path-type)))
      (remove-if-not #'maybe-match
                     (directory maybe-wilden-directory)
                     :key #'pathname-type))))

(defun rename-file-nef-images-in-directory (base-directory &key (wilden nil)
                                                                (case-mode :upcase))
  (declare (type (or boolean (eql :wild) (eql :wild-inferiors)) wilden)
           (type (or (eql :upcase)
                     (eql :downcase)
                     (eql :insensitive)) case-mode))
  (unless (probe-file base-directory)
    (error ":FUNCTION `rename-file-nef-images-in-directory' -- ~
             arg BASE-DIRECTORY non-existent~% got: ~S"
           base-directory))
  ;; (let* ((wild-nefs (make-pathname :directory (ecase wilden
  ;;                                               ((:wild-inferiors :wild)
  ;;                                                `(,@(pathname-directory base-directory) ,wilden))
  ;;                                               ((t)
  ;;                                                `(,@(pathname-directory base-directory) ,:wild))
  ;;                                               ((nil) `(,@(pathname-directory base-directory))))
  ;;                                  :name :wild
  ;;                                  :type "NEF"))
  ;;        (maybe-find-jpgs (directory wild-nefs)))
  (let ((maybe-find-nefs (directory-nef-images base-directory
                                               :wilden wilden
                                               :case-mode case-mode)))
    (if (null maybe-find-nefs)
        nil
        (flet ((maybe-translate-pathname-nikon-nef-image (pathname)
                 (let ((maybe-transformed (translate-pathname-nef-image pathname :case-mode case-mode)))
                   (list
                    (and maybe-transformed (rename-file pathname maybe-transformed))
                    pathname))))
          (map 'list #'maybe-translate-pathname-nikon-nef-image maybe-find-nefs)))))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
