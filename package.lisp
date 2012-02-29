;;-*- Mode: LISP; Syntax: COMMON-LISP; Encoding: utf-8; Base: 10; -*-
;;; :FILE image-ops/package.lisp
;;; ==============================

;; (in-package #:image-ops) ;; for Slime
;; *package*


(defpackage #:image-ops (:use #:common-lisp) ;; #+sbcl #:sb-int
            (:export 
             ;;
             #:*exiftool-path*
             #:*image-magick-convert-path*
             #:*image-output-default-thumb-type*
             #:*valid-image-types*
             ;;
           ;; image-ops-specials.lisp
             ;;
             #:*PSD-SCANNER* 
             #:*JPG-GZ-SCANNER* 
             #:*JPG-SCANNER* 
             #:*BMP-SCANNER* 
             #:*BMP-GZ-SCANNER* 
             #:*NEF-SCANNER* 
             #:*TIFF-SCANNER*
             #:*EXTENSION-GZ-SCANNER*
             ;;
             #:*BMP-HASH*  
             #:*BMP-GZ-HASH*
             #:*NEF-HASH*  
             #:*JPG-HASH* 
             #:*JPG-GZ-SCANNER* 
             #:*TIFF-HASH*  
             #:*PSD-HASH*   
             #:*OTHER-HASH*
             #:*SOURCE-DEST-CONVERSION-HASH*
             ;;
             ;; 
             ;;
           ;; image-ops/image-ops-macros.lisp
             ;; 
             ;; #:with-hash-table-op
             ;;
           ;; image-hash-directory-tree.lisp
             ;;
             ;; #:%absolute-existent-file-or-directory
             ;; #:%ensure-simple-namestring
             ;; #:%walk-directory-filter-ignorables
             ;; #:%partition-walked-files
             #:image-hash-write-to-file
             #:image-hash-write-all-to-file
             #:image-hash-write-conversion-hash-to-file
             #:image-hash-reset-all
             #:image-hash-counts-report
             #:walk-directory-images-to-hash
             #:image-hash-map-conversion-extension
             ;;
           ;; image-ops-rotate.lisp
             #:verify-image-magic-convert-path
             #:verify-image-file-output-type
             #:verify-image-file-file-kind
             #:unset-special-param-read-image-file-list
             #:read-image-file-list-from-file
             #:read-image-file-list-from-fprint0-file
             #:make-target-pathname-for-image-resize
             #:make-pathname-source-destination-resize-pairs
             #:write-fprint0-file-for-image-files-in-pathname
             #:resize-image-files-in-fprint0-file
             #:rotate-image-files-in-dir-list
             ;; image-ops-iphone.lisp
             #:translate-pathname-iphone-image
           ;; image-ops-docs.lisp
             
             ))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
