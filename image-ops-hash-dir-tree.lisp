;;; :FILE-CREATED <Timestamp: #{2011-08-26T11:51:33-04:00Z}#{11345} - by MON>
;;; :FILE mon-image-ops/image-hash-directory-tree.lisp
;;; ==============================


;; (in-package :image-ops)

;; :NOTE Our main function here is `walk-directory-images-to-hash'

(declaim (inline %ensure-simple-string
                 %walk-directory-filter-ignorables
                 %absolute-existent-file-or-directory
                 image-hash-reset-all
                 image-hash-counts-report))

(defun %ensure-simple-namestring (namething)
  (declare ((or pathname string) namething)
           (optimize (speed 3)))
  (when (simple-string-p namething)
    (return-from %ensure-simple-namestring (the simple-string namething)))
  (let ((string-thing (if (pathnamep namething)
                          (namestring namething)
                          namething)))
    (declare (string string-thing))
    (the simple-string
      (make-array (length string-thing)
                  :element-type 'character
                  :initial-contents string-thing))))

(defun %walk-directory-filter-ignorables (path-or-namestring)
  ;; (%walk-directory-filter-ignorables "lost+found/")
  ;; (%walk-directory-filter-ignorables "lost+found")
  (declare (inline %ensure-simple-namestring)
           (optimize (speed 3)))
  (let ((ensured-simple (%ensure-simple-namestring path-or-namestring)))
    (declare (simple-string ensured-simple))
    (flet ((chk-ignore (maybe-ignore)
             (declare (simple-string maybe-ignore))
             (string= maybe-ignore ensured-simple)))
      (declare (list *walk-directory-ignorables*))
      (notany #'chk-ignore *walk-directory-ignorables*))))

;; :NOTE Should this explicitly pass value of osicat:current-directory to
;; osicat:absolute-pathname? When invoked within the body of
;; osicat:walk-directory *default-pathname-defaults* is already dynamically
;; bound to osicat:current-directory and the extra overhead is likely costly.
(defun %absolute-existent-file-or-directory (maybe-file-or-directory)
  (declare (inline %ensure-simple-namestring
                   %walk-directory-filter-ignorables))
  (let* ((abs      (osicat:absolute-pathname maybe-file-or-directory)) ;; (osicat:current-directory)
         (abs-kind (osicat::get-file-kind abs nil)))
    (and abs-kind 
         (or (eql abs-kind :regular-file)
             (eql abs-kind :directory))
         (%walk-directory-filter-ignorables maybe-file-or-directory)
         t)))
      
(defun %partition-walked-files (rel-file-or-directory-pathname)
  (declare 
   (special *bmp-hash* *bmp-gz-hash* 
            *jpg-hash* *jpg-gz-hash*
            *nef-hash*  *tiff-hash*
            *psd-hash*  *other-hash*
            ;;
            *psd-scanner*    
            *jpg-gz-scanner* 
            *jpg-scanner*    
            *bmp-scanner*    
            *bmp-gz-scanner* 
            *nef-scanner*    
            *tiff-scanner*)
   (pathname rel-file-or-directory-pathname) 
   (pathname rel-file-or-directory-pathname)
   (inline %ensure-simple-namestring) 
   (optimize (speed 3))) 
  (let* ((regular-p          (osicat:regular-file-exists-p 
                              (osicat:absolute-pathname rel-file-or-directory-pathname))) ;*default-pathname-defaults*)))
         (regular-namestring (if regular-p
                                 (%ensure-simple-namestring regular-p)
                                 (return-from %partition-walked-files nil))))
    (declare (simple-string regular-namestring))
    ;; (print regular-p *standard-output*) (print regular-namestring *standard-output*)
    (labels ((cache-if-image (scanner hash)
               ;; (declare (function scanner) (hash-table hash))
               (declare (hash-table hash))
               (multiple-value-bind (match-string extension) (cl-ppcre:scan-to-strings scanner regular-namestring :sharedp t)
                 ;; (print scanner *standard-output*)
                 (when match-string
                   ;; (print match-string *standard-output*)
                   ;;
                   ;; Check if were looking at a match from *bmp-gz-scanner*,
                   ;; *jpg-gz-scanner* if so, its exension is bmp.gz, jpg.gz, or
                   ;; jpeg.gz and we need to take the pathname-name of its
                   ;; pathname-name
                   ;; (if gz  (cl-ppcre:register-groups-bind (ext dot gz)  (*extension-gz-scanner* (aref extension 0))
                   (let ((extension0 (aref extension 0)))
                     (sb-ext:with-locked-hash-table (hash)
                       (if (string= "gz" extension0 :start2 (- (length extension0)  2))
                           (setf (gethash match-string hash)
                                 (list (directory-namestring match-string)
                                       (pathname-name (pathname-name match-string))
                                       extension0))
                       
                           (setf (gethash match-string hash)
                                 (list (directory-namestring match-string)
                                       (pathname-name match-string)
                                       extension0)))))
                   t)))
             (push-other ()
               (sb-ext:with-locked-hash-table (*other-hash*)
                 (and (setf (gethash regular-namestring *other-hash*) 
                            (list (directory-namestring regular-namestring)
                                  (pathname-name regular-namestring)
                                  (pathname-type regular-namestring)))
                      t)))
             (map-pairs ()
               (loop 
                  for (fun . cache) in ;; `((,*psd-scanner*    . ,*psd-hash*)
                                       ;;   (,*jpg-gz-scanner* . ,*jpg-gz-hash*)
                                       ;;   (,*jpg-scanner*    . ,*jpg-hash*)
                                       ;;   (,*bmp-scanner*    . ,*bmp-hash*)
                                       ;;   (,*bmp-gz-scanner* . ,*bmp-gz-hash*)
                                       ;;   (,*nef-scanner*    . ,*nef-hash*)
                                       ;;   (,*tiff-scanner*   . ,*tiff-hash*))
                    (list (cons *psd-scanner*     *psd-hash*)
                          (cons *jpg-gz-scanner*  *jpg-gz-hash*)
                          (cons *jpg-scanner*     *jpg-hash*)
                          (cons *bmp-scanner*     *bmp-hash*)
                          (cons *bmp-gz-scanner*  *bmp-gz-hash*)
                          (cons *nef-scanner*     *nef-hash*)
                          (cons *tiff-scanner*    *tiff-hash*))
                  ;; for chk = (cache-if-image (symbol-value fun) (symbol-value cache))
                  for chk = (cache-if-image fun cache)
                  ;do (print chk *standard-output*)
                  when chk do (loop-finish) ;; (return-from map-pairs chk) ;;do 
                  finally (return
                            (if chk
                                chk
                                (push-other))))))
      (map-pairs)))) 

(defun image-hash-reset-all ()
  ;; (image-hash-reset-all)
  (declare 
   (special *bmp-hash* *bmp-gz-hash* 
            *jpg-hash* *jpg-gz-hash*
            *nef-hash*  *tiff-hash*
            *psd-hash*  *other-hash*)
   (optimize (speed 3)))
  (mapc #'(lambda (hash)
            (sb-ext:with-locked-hash-table (hash)
              (clrhash hash)))
        (list *nef-hash*
              *bmp-hash*
              *bmp-gz-hash*
              *jpg-hash*
              *jpg-gz-hash*
              *psd-hash*
              *tiff-hash*
              *other-hash*)))

(defun image-hash-counts-report ()
  ;; (image-hash-counts-report)
  (declare (special *bmp-hash* *bmp-gz-hash* 
                    *jpg-hash* *jpg-gz-hash*
                    *nef-hash*  *tiff-hash*
                    *psd-hash*  *other-hash*)
           (optimize (speed 3)))
  (flet ((ht-and-count (ht)
           (sb-ext:with-locked-hash-table (ht)
             (hash-table-count ht))))
    (pairlis (list 
              '*nef-hash*
              '*bmp-hash*
              '*bmp-gz-hash*
              '*jpg-hash*
              '*jpg-gz-hash*
              '*psd-hash*
              '*tiff-hash*
              '*other-hash*)
             (mapcar #'ht-and-count (list *nef-hash* *bmp-hash* *bmp-gz-hash* *jpg-hash*
                                          *jpg-gz-hash* *psd-hash* *tiff-hash* *other-hash*)))))

;; Our main function 
(defun %walk-directory-images-to-hash (directory-pathname &key (clear-count t))
  (declare ((or pathname string) directory-pathname)
           (boolean clear-count)
           (inline 
             %absolute-existent-file-or-directory
             %ensure-simple-namestring             
             image-hash-counts-report
             image-hash-reset-all)
           (optimize (speed 3)))
  (when clear-count (image-hash-reset-all))
  (osicat:walk-directory (%ensure-simple-namestring directory-pathname)
                         #'%partition-walked-files
                         :directories :breadth-first ;;  :depth-first
                         ;; :directories :depth-first
                         :test #'%absolute-existent-file-or-directory)
  (image-hash-counts-report))

(defun walk-directory-images-to-hash (directory-pathname &key (clear-count t))
  (sb-thread:make-thread 
   #'(lambda () 
       (%walk-directory-images-to-hash directory-pathname :clear-count clear-count))
   :name (format nil "WALK-DIRECTORY-IMAGES-TO-HASH-~D" (random most-positive-fixnum))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
