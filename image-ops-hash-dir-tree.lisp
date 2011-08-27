;;; :FILE-CREATED <Timestamp: #{2011-08-26T16:44:51-04:00Z}#{11345} - by MON>
;;; :FILE image-ops/image-ops-hash-dir-tree.lisp
;;; ==============================

;; :NOTE Our main function here is `walk-directory-images-to-hash' at BOF

(in-package #:image-ops)
;; *package*

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

(defun image-hash-write-to-file (hash-table directory-pathname hash-table-name &key (external-format :default))
  (declare (mon:pathname-or-namestring directory-pathname))
  (let* ((dir-ensure     (ensure-directories-exist (pathname directory-pathname)))
         (hash-file-name (and dir-ensure 
                              (make-pathname :directory (pathname-directory dir-ensure)
                                             :name (concatenate 'string hash-table-name '(#\-) (mon:time-string-yyyy-mm-dd)))))
         (delim          (make-string 68 :initial-element #\;)))
    (with-open-file (f hash-file-name
                       :direction         :output
                       :if-exists         :supersede
                       :if-does-not-exist :create
                       :element-type       'character
                       :external-format   external-format)
      (maphash 
       #'(lambda (key val) 
           (format f "~%~A~%(:FILE      ~S~% :DIRECTORY ~S~% :NAME      ~S~% :TYPE      ~S)~%"
                   delim key (elt val 0) (elt val 1) (elt val 2)))
       hash-table)
      hash-file-name)))

(defun image-hash-write-all-to-file (directory-pathname)
  (flet ((writer (hash-table)
           (image-hash-write-to-file (symbol-value hash-table)
                                     directory-pathname
                                     (string-trim '(#\*) (string-downcase hash-table)))))
    (mapcar #'writer
            (list '*bmp-hash* '*bmp-gz-hash*
                  '*jpg-hash* '*jpg-gz-hash*
                  '*tiff-hash* '*nef-hash* 
                  '*psd-hash* '*other-hash*))))

(defun image-hash-map-conversion-extension (source-hash conversion-hash conversion-extension &key (clear-conversion nil))
  (declare (string conversion-extension)
           (boolean clear-conversion))
  (unless (member conversion-extension *valid-image-types* :test #'string=)
    (error ":FUNCTION `image-hash-map-conversion-extension' ~
            -- Arg CONVERSION-EXTENSION not member of `*valid-image-types*', got: ~S"
           conversion-extension))
  (when clear-conversion (clrhash conversion-hash))
  (maphash #'(lambda (key val)
               ;; our file to convert
               (setf (gethash key conversion-hash)
                     (namestring
                      (make-pathname :directory (pathname-directory (elt val 0))
                                     :name (elt val 1)
                                     :type conversion-extension))))

           source-hash)
  conversion-hash)

(defun image-hash-conversion-perform (conversion-hash log-file &key (delete-on-success nil) 
                                                                    (external-format :default))
  (declare (boolean delete-on-success))
  (let ;; ((enure-log-file (probe-file log-file))
       ;;   (log-ensured (if enure-log-file
       ;;                    enure-log-file
       ;;                    (error "Arg LOG-FILE does not exist")))
      ((delim (make-string 68  :initial-element #\;)))
    (labels ((log-metadata (fname)
               (let* ((jstream           (make-string-output-stream))
                      (*standard-output* jstream))
                 (format jstream "~%;;~%;; exiftool results for ~A~%;;~%~%" fname)
                 (when
                     (zerop (sb-ext:process-exit-code
                             (sb-ext:run-program "/usr/bin/exiftool" 
                                                 (list "-j" fname)
                                                 :output *standard-output*)))
                   (get-output-stream-string jstream))))
             (log-results (status from to)
               (with-open-file (s log-file
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create
                                  :external-format external-format
                                  :element-type 'character)
                 (if status
                     (progn 
                       (format s "~&~A~%;; Successfull conversion ~%;; :FROM ~S~%;; :TO ~S~%" delim from to)
                       (format t "~&~A~%;; Successfull conversion ~%;; :FROM ~S~%;; :TO ~S~%" delim from to)
                       (princ (log-metadata from) s)
                       (princ (log-metadata to)  s)
                       (terpri s))
                     (progn
                       (format s "~&~A~%;; Failed conversion ~%;; :FROM ~S~%;; :TO ~S~%" delim from to)
                       (format t "~&~A~%;; Failed conversion ~%;; :FROM ~S~%;; :TO ~S~%" delim from to)))))
             (hash-pair-to-args (key val)
               (if 
                (zerop 
                 (sb-ext:process-exit-code
                  (sb-ext:run-program *image-magick-convert-path* (list key "-compress" "zip" val))))
                (progn 
                  (log-results t key val)
                  (when delete-on-success 
                    (delete-file key))
                  t)
                (log-results nil key val)))
             (map-with-lock ()
               (sb-ext:with-locked-hash-table (conversion-hash)
                 (maphash #'hash-pair-to-args conversion-hash))))
      (sb-thread:make-thread #'map-with-lock
                             :name (format nil "image-hash-conversion-perform-~D" (random most-positive-fixnum))))))
  

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
