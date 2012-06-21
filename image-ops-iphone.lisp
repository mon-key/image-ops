;;; :FILE-CREATED <Timestamp: #{2012-02-28T20:52:43-05:00Z}#{12092} - by MON>
;;; :FILE image-ops/image-ops-iphone.lisp
;;; ==============================

#|

 :NOTE sbcl allows this:
 (directory "foo/*.[jJ][Pp][Gg]")
 (directory "foo/*.[jJ][Pp][Gg]")
 (directory "foo/*.[jJ][Pp]*[Gg]")

 remove files found by matching on pathname-type
 (remove-if-not (lambda (path-type) (cl-ppcre:scan "(?i)^jpe?g$" path-type))
                (directory #P"/foo/**/*.*")
                :key #'pathname-type)

 remove files found by matching on pathname-name
 (remove-if-not (lambda (path-name) (cl-ppcre:scan "<TARGET-NAMESTRING-REGEX>" path-name))
                (directory #P"/foo/**/*.*")
                :key #'pathname-name)

|#

(in-package #:image-ops)

;; "([1-2][0-9][0-9][0-9])-([0-1][0-9])-([0-3][0-9])T([0-2][0-9])([0-5][0-9])([0-5][0-9])(([+-][0-2][0-9])([0-9][0-9]))?$"
;; longform matches any of these:
;; "2012-03-02T181325"
;; "2012-03-02T181325+0500"
;; "2012-03-03T181325-0400"
(defvar *regex-iphone-image-shortform-timestamp* nil)

;; "([1-2][0-9][0-9][0-9])-([0-1][0-9])-([0-3][0-9])T([0-2][0-9])([0-5][0-9])([0-5][0-9])$"
;; shortform only matches this:
;; "2012-03-02T181325"
(defvar *regex-iphone-image-longform-timestamp* nil)

;; (cl-ppcre:parse-string "([1-2][0-9][0-9][0-9])-([0-1][0-9])-([0-3][0-9])(T)([0-2][0-9])([0-5][0-9])([0-5][0-9])$")
;;
;;    YYYY                 -  MONTH       -   DAY        T   HOUR       MINUTE      SECOND
;; "([1-2][0-9][0-9][0-9])-([0-1][0-9])-([0-3][0-9])(T)([0-2][0-9])([0-5][0-9])([0-5][0-9])$"
;;
;; (length "3539462658") => 10
;; (length "005897-3539462658") => 17
;; (length "5897-3539462658")   => 15
;;
;; (cl-ppcre:scan-to-strings "^(\\d{2,6})-(\\d{10})$" "5897-3539462658")
;; (cl-ppcre:scan-to-strings "^(\\d{2,6})-(\\d{10})$" "005897-3539462658")
;; (cl-ppcre:scan-to-strings "^(\\d{2,6})-(\\d{10})$" "!!!005897-3539462658")
;;
;; (cl-ppcre:register-groups-bind (year month day hour minute second)
;;      ("([1-2][0-9][0-9][0-9])-([0-1][0-9])-([0-3][0-9])T([0-2][0-9])([0-5][0-9])([0-5][0-9])$"
;;       "2012-03-02T191133")
;;    ;; (list second minute hour day month year))
;;    (when year ;; (and year month day hour minute second)
;;      (decode-universal-time
;;       (apply #'encode-universal-time
;;             (map 'list #'parse-integer
;;                  (list second minute hour day month year))
;;             ))))
(defun translate-pathname-iphone-image (pathname-or-namestring &key (case-mode :upcase))
  (declare (type (or (eql :upcase)
                     (eql :downcase)
                     (eql :insensitive)) case-mode))
  (let ((fname (file-namestring pathname-or-namestring)))
    (flet ((ext-compare (extension)
             (ecase case-mode
               (:upcase (string= extension ".JPG"))
               (:downcase (string= extension ".jpg"))
               (:insensitive (string-equal extension ".JPG")))))
      (when (>= (mon:string-length fname) 12)
        (destructuring-bind (img num ext &rest rest) (mon:string-subdivide fname 4)
          (and (not rest)
               (string= img "IMG_")
               (every #'digit-char-p num)
               (ext-compare ext)
               (let ((buffer (make-array 27 :element-type 'character :fill-pointer 0)))
                 (with-output-to-string (output buffer)
                   ;;(format output "~4,'0d-" num)
                   (format output "~6,'0d-ip-" (parse-integer num))
                   (mon::format-timestring output (local-time:universal-to-timestamp (file-write-date pathname-or-namestring))
                                           :format mon:*timestamp-for-file-format*))
                 (make-pathname :directory (pathname-directory pathname-or-namestring)
                                :name buffer
                                :type "jpg"))))))))

;; (defun directory-jpg-images (base-directory &key (wilden nil))
;; (let ((wild-jpeg-scanner (cl-ppcre:create-scanner "(?i)^jpe?g$" :case-insensitive-mode t)))
;; find all pathnames  beneath BASE-DIRECTORY with pathname-tyeps matching the regular expression
;; \"^jpe?g$\"
(defun directory-jpg-images (base-directory &key (wilden nil) (case-mode nil))
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
               (error ":FUNCTION `directory-jpg-images' -- ~
                       arg BASE-DIRECTORY non-existent~% got: ~S"
                      base-directory)))
         (wild-jpeg-scanner
           (cl-ppcre:create-scanner (ecase case-mode
                                      (:upcase
                                       (cl-ppcre:create-scanner "^JPE?G$" :case-insensitive-mode nil))
                                      (:downcase
                                       (cl-ppcre:create-scanner "^jpe?g$" :case-insensitive-mode nil))
                                      ((:insensitive nil)
                                       (cl-ppcre:create-scanner "^jpe?g$" :case-insensitive-mode t))))))
    (flet ((maybe-match (path-type)
             (cl-ppcre:scan wild-jpeg-scanner path-type)))
      (remove-if-not #'maybe-match
                     (directory maybe-wilden-directory)
                     :key #'pathname-type))))

(defun rename-file-iphone-images-in-directory (base-directory &key (wilden nil)
                                                                   (case-mode :upcase))
  (declare (type (or boolean (eql :wild) (eql :wild-inferiors)) wilden)
           (type (or (eql :upcase)
                     (eql :downcase)
                     (eql :insensitive)) case-mode))
  (unless (probe-file base-directory)
    (error ":FUNCTION `rename-file-iphone-images-in-directory' -- ~
             arg BASE-DIRECTORY non-existent~% got: ~S"
           base-directory))
  ;; (let* ((wild-jpgs (make-pathname :directory (ecase wilden
  ;;                                               ((:wild-inferiors :wild)
  ;;                                                `(,@(pathname-directory base-directory) ,wilden))
  ;;                                               ((t)
  ;;                                                `(,@(pathname-directory base-directory) ,:wild))
  ;;                                               ((nil) `(,@(pathname-directory base-directory))))
  ;;                                  :name :wild
  ;;                                  :type "JPG"))
  ;;        (maybe-find-jpgs (directory wild-jpgs)))
  (let ((maybe-find-jpgs (directory-jpg-images base-directory
                                               :wilden wilden
                                               :case-mode case-mode)))
    (if (null maybe-find-jpgs)
        nil
        (flet ((maybe-translate-pathname-iphone-image (pathname)
                 (let ((maybe-transformed (translate-pathname-iphone-image pathname :case-mode case-mode)))
                   (list
                    (and maybe-transformed (rename-file pathname maybe-transformed))
                    pathname))))
          (map 'list #'maybe-translate-pathname-iphone-image maybe-find-jpgs)))))



;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
