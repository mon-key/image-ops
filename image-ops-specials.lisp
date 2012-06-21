;;; :FILE-CREATED <Timestamp: #{2011-08-26T11:55:15-04:00Z}#{11345} - by MON>
;;; :FILE image-ops/image-ops-specials.lisp
;;; ==============================

(in-package #:image-ops)
;; *package*


(defparameter *rotate-images-thread* '())

(defparameter *read-image-file-list* '())

(defparameter *valid-image-types* (list "jpg" "jpeg" "tiff" "tif" "bmp" "png" "nef" "psd"))

(defparameter *image-output-default-thumb-type* "jpg")

(defparameter *image-magick-convert-path*
  ;; :NOTE MS-windows has a convert that is _not_ equivalent to ImageMagick's:
  #+(or win32 windows) nil
  ;;
  #-sbcl (and (probe-file #P"/usr/bin/convert") "/usr/bin/convert")
  ;;
  ;; We assume that any of the following have convert in /user/bin/ if
  ;; `mon:executable-find' doesn't return T:
  ;; #+(or linux darwin netbsd openbsd freebsd)
  #+sbcl (or (mon:executable-find "convert")
             (and (probe-file #P"/usr/bin/convert")
                  "/usr/bin/convert")))

(defparameter *exiftool-path*
  #+(or win32 windows) nil
  #-sbcl (and (probe-file #P"/usr/bin/exiftool") "/usr/bin/exiftool")
  #+sbcl (or (mon:executable-find "exiftool")
             (and (probe-file #P"/usr/bin/exiftool")
                  "/usr/bin/exiftool")))


;; :NOTE There is reader conditional in image-ops/image-ops.asd around #+IS-MON
;; when when satisfied laods additional less generic system specific ignorables
;; from :FILE image-ops/image-ops-loadtime-bind.lisp
(defparameter *walk-directory-ignorables* (list "lost+found/"
                                                ;; :NOTE elements of this list are checked with `cl:string='.
                                                ;; "SCCS" "MCVS"
                                                ".git/" ".bzr/" ".hg/" ".svn/" "_darcs/" "RCS/" "CVS/" "rcs/" "cvs/"
                                                ".gitignore" ".hgignore" ".bzrignore"
                                                ;;
                                                ".BridgeCache" ".BridgeCacheT" "Thumbs.db"))

(defvar *psd-scanner*   (cl-ppcre:create-scanner "^/.*\\.(psd|PSD)$"))

(defvar *jpg-scanner*    (cl-ppcre:create-scanner "^/.*\\.(jpe?g|JPE?G)$"))

(defvar *jpg-gz-scanner* (cl-ppcre:create-scanner "^/.*\\.(jpe?g\\.gz)$"))

(defvar *bmp-scanner*    (cl-ppcre:create-scanner "^/.*\\.(bmp|BMP)$"))

(defvar *bmp-gz-scanner* (cl-ppcre:create-scanner "^/.*\\.(bmp\\.gz)$"))

(defvar *nef-scanner*    (cl-ppcre:create-scanner "^/.*\\.(nef|NEF)$"))

;; :NOTE Why isn't this "^/.*\\.(tiff?|TIFF?)$" instead?
(defvar *tiff-scanner*   (cl-ppcre:create-scanner "^/.*\\.(tiff?|TIFF)$"))

;; Matches bmp.gz jpg.gz jpeg.gz
;; :EXAMPLE
;; (cl-ppcre:register-groups-bind (a b c) (*extension-gz-scanner* "bmp.gz")
;;  (list a b c))
;; :NOTE barring the zero-length string we could also prob. do:
;;  (string= "gz" <STRING-2> :start2 (- (length <STRING-2>)  2))
(defvar *extension-gz-scanner* (cl-ppcre:create-scanner "^(.{3,4})(\\.)(gz)$"))

(defvar *bmp-hash*    (make-hash-table :test #'equal :synchronized t))

(defvar *bmp-gz-hash* (make-hash-table :test #'equal :synchronized t))

(defvar *nef-hash*    (make-hash-table :test #'equal :synchronized t))

(defvar *jpg-hash*    (make-hash-table :test #'equal :synchronized t))

(defvar *jpg-gz-hash* (make-hash-table :test #'equal :synchronized t))

(defvar *tiff-hash*   (make-hash-table :test #'equal :synchronized t))

(defvar *psd-hash*    (make-hash-table :test #'equal :synchronized t))

(defvar *other-hash*  (make-hash-table :test #'equal :synchronized t))

(defvar *source-dest-conversion-hash* (make-hash-table :test #'equal :synchronized t))




;;; ==============================
;; :NOTE This is in clime/clime-specials.lisp
;;
;; (defparameter *FILE-VALID-IMAGE-MIME-TYPES*
;;   ;;(setq *FILE-VALID-IMAGE-MIME-TYPES*
;;   (list "tiff" "tif"
;;         "jpeg" "jpg"                         ;; "pjpeg"
;;         "bmp"  "x-bmp" "x-ms-bmp" "x-MS-bmp" ;; x-win-bitmap
;;         "nef"  "x-nikon-nef"                 ;; x-niff -- Nikon
;;         "dng"                                ;; x-adobe-dng -- Adobe
;;         "png"
;;         "svg" ;; svg+xml
;;         "psd" "x-psd"
;;         ;; "x-dcraw"
;;         ;; "crw" "cr2"                       ;; x-canon-cr2 x-canon-crw -- Cannon
;;         ;; "gif"
;;         ))
;;
;;; ==============================


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
