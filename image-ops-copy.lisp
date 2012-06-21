;;; :FILE-CREATED <Timestamp: #{2012-05-30T20:31:08-04:00Z}#{12223} - by MON>
;;; :FILE image-ops/image-ops-copy.lisp
;;; ==============================

(in-package #:image-ops)

;; adapted from `copy-byte-stream' in clime/copy-bytes.lisp
(defun copy-image-byte-stream (from-byte-stream to-byte-stream &key (element-type 'unsigned-byte))
  (let ((byte-stream-bfr (make-array 4096 :element-type element-type)))
    (do ((byte-stream-pos (read-sequence byte-stream-bfr from-byte-stream)
                          (read-sequence byte-stream-bfr from-byte-stream)))
        ((zerop byte-stream-pos) nil)
      (write-sequence byte-stream-bfr to-byte-stream :end byte-stream-pos))))

;; adapted from `copy-byte-file' in clime/copy-bytes.lisp
(defun copy-image-byte-file (source-byte-file dest-byte-file
                             &key (if-exists :supersede) ;; :error
                                  (element-type    'unsigned-byte)
                                  (set-dest-byte-file-write-date nil))
  ;; (external-format :default)
  ;; (report-stream   *standard-output*))
  ;; (verify-element-type-for-copy-byte element-type :stream report-stream)
  (with-open-file (byte-input source-byte-file
                              :direction         :input
                              :if-does-not-exist :error
                              ;; :external-format   external-format ; Is this ever applicable?
                              :element-type      element-type)
    (with-open-file (byte-output dest-byte-file
                                 :direction         :output
                                 :if-does-not-exist :create
                                 :if-exists         if-exists
                                 ;; :external-format   external-format ; Is this ever applicable?
                                 :element-type      element-type)
      (copy-image-byte-stream byte-input
                              byte-output
                              :element-type element-type)))
  ;; (probe-file dest-byte-file)
  (and
   (probe-file dest-byte-file)
   (and set-dest-byte-file-write-date
        (or (mon::set-file-write-date-using-file  (namestring dest-byte-file) (namestring source-byte-file))
            t))
   dest-byte-file))


;; (copy-image-cmg-nefs :image-directory-pathname-source #P"<CMG-SOURCE-PATHNAME>"
;;                      :image-directory-pathname-base-target #P"<CMG-TARGET-PATHNAME>"
;;                      :image-match-regex (cl-ppcre:create-scanner "(cmg-\\d{4})(-\\d{1,2})"))
(defun copy-image-cmg-nefs (&key image-directory-pathname-source
                                 image-directory-pathname-base-target
                                 image-match-regex
                                 (delete-file-image-source t))
"Copy nef images matching IMAGE-MATCH-REGEX pattern
from IMAGE-DIRECTORY-PATHNAME-SOURCE to a corresponding subdir beneath
IMAGE-DIRECTORY-PATHNAME-BASE-TARGET (if it exists).
When DELETE-FILE-IMAGE-SOURCE is non-nil (the defalut) deletes each matched image
in source directory prior to returning.
IMAGE-MATCH-REGEX is a regular expression \(a string or cl-ppcre scanner\)
comprised of two register groups the first of which matches a file's image-name
with a target directory beneath IMAGE-DIRECTORY-PATHNAME-BASE-TARGET, the second
value is currently ignored but should not contain a pattern matching the pathname type.
:EXAMPLE
 \(copy-image-cmg-nefs :image-directory-pathname-source #P\"/mnt/foo/bar/baz/\"
                      :image-directory-pathname-base-target #P\"/mnt/quux/zomp/blarg/\"
                      :image-match-regex \(cl-ppcre:create-scanner \"\(cmg-\\\\d{4}\)\(-\\\\d{1,2}\)\"\)\)
:SEE-ALSO `copy-image-byte-file'.~%▶▶▶"
  (declare (pathname image-directory-pathname-source
                     image-directory-pathname-base-target))
  (let ((results nil)
        (delete-results nil))
    (setf results
          (loop
            for image-path in (directory-nef-images image-directory-pathname-source :case-mode :downcase)
            for image-name = (pathname-name image-path)
            when (let ((maybe-target-nef-directory nil))
                   (cl-ppcre:register-groups-bind (cmg cmg-suffix) (image-match-regex image-name)
                     (and cmg
                          cmg-suffix
                          (setf maybe-target-nef-directory
                                (nth-value 0
                                           ;; :NOTE repeatedly probing the same directory is likely a bottleneck
                                           (mon:probe-directory
                                            (merge-pathnames (make-pathname :directory (list :relative cmg))
                                                             image-directory-pathname-base-target))))
                          ;; (cons image-path
                          ;;                     (merge-pathnames (make-pathname :name image-name :type "nef")
                          ;;                                      maybe-target-nef-directory)))))
                          ;; collect it into gthr
                          ;; finally (return  gthr)))
                          (cons image-path
                                (copy-image-byte-file image-path
                                                      (merge-pathnames (make-pathname :name image-name :type "nef")
                                                                       maybe-target-nef-directory)
                                                      :set-dest-byte-file-write-date t)))))
            collect it into gthr))
    (if delete-file-image-source
        (unwind-protect
             (values results
                     (progn
                       (map nil #'(lambda (x) (or (and (delete-file (car x))
                                                       (push t delete-results))
                                                  (push (cons nil (car x)) delete-results)))
                            results)
                       delete-results))
          (values results delete-results))
        (values results delete-results))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
