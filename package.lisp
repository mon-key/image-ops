;;-*- Mode: LISP; Syntax: COMMON-LISP; Encoding: utf-8; Base: 10; -*-
;;; :FILE mon-systems/package.lisp
;;; ==============================

;; (in-package #:mon) ;; for Slime
;; *package*


(defpackage #:image-ops (:use #:common-lisp) ;; #+sbcl #:sb-int
            (:export 

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

             ;; #:%absolute-existent-file-or-directory
             ;; #:%ensure-simple-namestring
             ;; #:%walk-directory-filter-ignorables
             ;; #:%partition-walked-files
             #:image-hash-reset-all
             #:image-hash-counts-report
             #:walk-directory-images-to-hash

             ;; mon-image-ops/image-ops-docs.lisp

             )
;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
