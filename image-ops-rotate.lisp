;;; :FILE-CREATED <Timestamp: #{2011-08-02T19:25:49-04:00Z}#{11312} - by MON>
;;; :FILE image-ops/image-ops-rotate.lisp
;;; ==============================

;; :TODO incorporate the metafs/MeDaMA abstractions for image files.

(in-package #:image-ops)
;; *package*



(defun verify-image-magic-convert-path ()
  ;; 
  (declare (inline mon:string-not-empty-p)
           (optimize (speed 3)))
  (unless (and (boundp '*image-magick-convert-path*) 
               *image-magick-convert-path* 
               (mon:string-not-empty-p *image-magick-convert-path*))
    (error "Variable `mon:*image-magick-convert-path*' not bound.~%~
            A path to ImageMagick's `convert` command must be provided.~%~
            If this is a Windows machine it must be done so explicitly.~%~
            If this is not a Windows machine then both `mon:executable-find' and ~
            cl:probe-file of #P\"/usr/bin/convert\" have failed~%"))
  t)

(defun verify-image-file-output-type (maybe-valid-output-extension)
  (declare (string maybe-valid-output-extension))
  (unless (member maybe-valid-output-extension image-ops::*valid-image-types* :test #'string-equal)
    (mon:simple-error-mon  :w-sym "verify-image-file-output-type"
                           :w-type 'function
                           :w-spec "arg MAYBE-VALID-OUTPUT-EXTENSION not string-equal any of:~% ~S"
                           :w-args  (list image-ops::*valid-image-types*)
                           :w-got   maybe-valid-output-extension
                           :w-type-of t
                           :signal-or-only nil))
  (string-downcase maybe-valid-output-extension))

(defun verify-image-file-file-kind (maybe-image-file-file &optional (error-on-wild-empty-dotted t))
  (declare (mon:pathname-or-namestring maybe-image-file-file))
  (unless (mon:pathname-or-namestring-not-empty-dotted-or-wild-p maybe-image-file-file)
    (if error-on-wild-empty-dotted
        (mon:simple-error-mon  :w-sym "verify-image-file-file-kind"
                               :w-type 'function
                               :w-spec "arg MAYBE-IMAGE-FILE-FILE not `mon:pathname-or-namestring-not-empty-dotted-or-wild-p'"
                               :w-got   maybe-image-file-file
                               :w-type-of t
                               :signal-or-only nil)
        (return-from verify-image-file-file-kind nil)))
  (case (osicat:file-kind maybe-image-file-file)
    (:regular-file (pathname maybe-image-file-file))
    (t nil)))

(defun unset-special-param-read-image-file-list (special-param) 
  (declare (special special-param))
  (when 
      (and (boundp special-param)
           (symbol-value special-param))
    (set special-param nil)))

(defun read-image-file-list-from-file (pathname-or-namestring &key (special-param '*read-image-file-list*)
                                       ;;(element-type 'character))
                                       (external-format :default))

  (declare (mon:pathname-or-namestring pathname-or-namestring)
           (special special-param))
  (with-open-file (img-files  pathname-or-namestring 
                              :direction         :input 
                              :if-does-not-exist :error
                              :external-format   external-format
                              :element-type      'character)
    ;; Make sure that :if-does-not-exist has a chance to run
    (unset-special-param-read-image-file-list special-param)
    (set special-param (read  img-files))))

(defun read-image-file-list-from-fprint0-file (pathname-or-namestring &key (special-param 'image-ops::*read-image-file-list*)
                                               ;;(element-type 'character))
                                               (external-format :default))
  (declare (mon:pathname-or-namestring pathname-or-namestring)
           (special special-param))
  (unset-special-param-read-image-file-list special-param)
  (set special-param
       (mon:read-file-list-from-fprint0-file pathname-or-namestring :external-format external-format)))

(defun make-target-pathname-for-image-resize (source-pathname &key target-directory target-type
                                                               (prefix-name-with "") 
                                                               (suffix-name-with ""))
  (declare (mon:pathname-or-namestring target-directory target-type)
           (string prefix-name-with suffix-name-with))
  (let ((dest-dir  (pathname-directory target-directory))
        (dest-name (concatenate 'string prefix-name-with (pathname-name source-pathname) suffix-name-with))
        (dest-type (verify-image-file-output-type target-type)))
    (cons  source-pathname
           (make-pathname :directory dest-dir
                          :name      dest-name
                          :type      dest-type))))

(defun make-pathname-source-destination-resize-pairs (read-source-files-from &key target-directory 
                                                                                  target-type
                                                                                  (prefix-name-with "")
                                                                                  (suffix-name-with ""))
  (declare (mon:pathname-or-namestring read-source-files-from target-directory)
           (string  target-type prefix-name-with suffix-name-with))
  (flet ((mk-rsz-path (source-image)
           (declare (mon:pathname-or-namestring source-image))
           (make-target-pathname-for-image-resize
            source-image
            :target-directory target-directory 
            :prefix-name-with prefix-name-with
            :suffix-name-with suffix-name-with
            :target-type      target-type)))
    (loop 
       for file in (read-image-file-list-from-fprint0-file read-source-files-from)
       collecting (mk-rsz-path (pathname file)))))

(defun write-fprint0-file-for-image-files-in-pathname (&key search-directory search-type append-suffix dest-pathname) 
  (declare (mon:filename-designator search-directory)
           (string search-type)
           (mon:string-or-null append-suffix)
           ((or null mon:filename-designator) dest-pathname))
  (let* ((directory
          (multiple-value-bind (type maybe-dir) (mon:pathname-native-file-kind search-directory)
            (if (eql type :directory)
                (mon:pathname-as-directory maybe-dir)
                (mon:simple-error-mon :w-sym "write-fprint0-file-for-image-files-in-pathname"
                                      :w-type 'function
                                      :w-spec "Arg SEARCH-DIRECTORY non-existent or not a directory"
                                      :w-got   maybe-dir
                                      :w-type-of t
                                      :signal-or-only nil))))
         (dir-wild-type   
          (cons (make-pathname :directory `(,@(pathname-directory directory))
                               :name :wild :type (verify-image-file-output-type search-type))
                directory))
         (suffix (or (and (mon:string-empty-p append-suffix) "")
                     (and append-suffix 
                          (let ((chk-empty (string-left-trim (list #\- #\.) append-suffix)))
                            (and (mon:string-not-empty-or-all-whitespace-p chk-empty) chk-empty)))
                     (mon:time-string-yyyy-mm-dd)))
         (dest 
          (if dest-pathname
              (if (mon:pathname-or-namestring-not-empty-dotted-or-wild-p dest-pathname)
                  dest-pathname
                  (mon:simple-error-mon :w-sym "write-fprint0-file-for-image-files-in-pathname"
                                        :w-type 'function
                                        :w-spec "Arg DEST-PATHNAME did not satisfy ~
                                                `mon:pathname-or-namestring-not-empty-dotted-or-wild-p'"
                                        :w-got dest-pathname
                                        :w-type-of t
                                        :signal-or-only nil))
              (format nil "~Aprocess-files-~A-~A"  (namestring (cdr dir-wild-type)) search-type suffix))))
    ;; (list dest dir-wild-type)))
    (with-open-file (nulls dest
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :external-format :UTF-8)
      (loop 
         for file in (directory (car dir-wild-type))
         do (format nulls "~A~C" (namestring file) #\NUL)
         finally (return dest)))))

;; :TODO Add additional key resize-y and adapt body accordingly.
;; :NOTE The current interaction with SUFFIX-NAME-WITH may not always be what we want.
(defun resize-image-files-in-fprint0-file (fprint0-file &key target-directory 
                                                             target-type
                                                             (prefix-name-with "")
                                                             (suffix-name-with "" suffix-supplied)
                                                             resize-x)
  (declare (mon:pathname-or-namestring  fprint0-file target-directory target-type)
           (string prefix-name-with suffix-name-with)
           ((mon:unsigned-byte-32) resize-x))
  (verify-image-magic-convert-path)
  (let* ((resize-arg            (format nil "~D" resize-x))
         (suffix-with           (or (and suffix-supplied suffix-name-with)
                                    (concatenate 'string '(#\-) (the string resize-arg))))
         (base-resize-arg-list (list "-resize" resize-arg))
         (resize-pairs         (make-pathname-source-destination-resize-pairs fprint0-file 
                                                                              :target-directory target-directory
                                                                              :target-type      target-type
                                                                              :prefix-name-with prefix-name-with
                                                                              :suffix-name-with suffix-with))
         ;(convert-path   "/usr/bin/convert")
         (proc-stack (make-array (length resize-pairs) :fill-pointer 0)))
    ;(declare (string convert-path))
    (labels ((convert-resize (pathname-pairs)
               (declare 
                ;; (special *image-magick-convert-path*)
                (cons pathname-pairs))
               (let* ((source-dest (list (namestring (car pathname-pairs))
                                         (namestring (cdr pathname-pairs))))
                      (arglist (append base-resize-arg-list source-dest))
                      (proc-stat '()))
                 (setf proc-stat
                       `(,(sb-ext:process-exit-code (sb-ext:run-program *image-magick-convert-path* arglist))
                          ,@source-dest))))
             (all-resized ()
               (dolist (rsz resize-pairs
                        (setf proc-stack (coerce proc-stack 'list)))
                 (vector-push (convert-resize rsz) proc-stack)))
             (all-resized-in-thread ()
               (sb-thread:make-thread #'all-resized :name "resize-image-files-in-fprint0-file")))
    (all-resized-in-thread))))

;; :NOTE This should really be installed to Clime...
;; :NOTE This should check if image is jpeg and maybe use `exiftran` which won't kill the EXIF data.
;; `exiftran -ai' will process all files in a directory. `exiftran -d` will dump
;; the corresponding exifdata as well which could be useful for use with metafs/MeDaMa
;; Likewise, consider `renrot`, `jpegtran -copy`, etc.
;; Note `exiftool -stay_open 1 -@ <ARGFILE>` flag will pipe in arguments from
;; standard input and/or a file such that we can keep pumping commands to it by
;; simply writing a new arg to a CL stream.
(defun rotate-image-files-in-dir-list (dir-list &key image-type degrees positive-or-negative 
                                       (special-thread-param '*rotate-images-thread*)
                                       (report-stream *standard-output*))
  (declare ((integer 1 359) degrees)
           (string image-type)
           ((or symbol keyword) positive-or-negative))
  (verify-image-magic-convert-path)
  (unless (member positive-or-negative (list :positive :negative :clockwise :counter-clockwise))
    (error "keyword POSITIVE-OR-NEGATIVE not one of:~%~T~
                      :positive :negative :clockwise :counter-clockwise~%"))
  (unless (member image-type *valid-image-types* :test #'string=)
    (error "keyword type not member of:~%~T~S~%" *valid-image-types*))
  (let ((rotation-string (format nil "~C~D" (ecase positive-or-negative
                                              ((:positive :clockwise) #\+)
                                              ((:negative :counter-clockwise) #\-))
                                 degrees))
        (wild-file-type (make-pathname :name :wild :type image-type))
        (proc-stack '()))
    (declare (string rotation-string)
             (pathname wild-file-type))
    (labels ((merge-wild-ftype (merge-dir)
               (merge-pathnames wild-file-type merge-dir))
             (make-rotation-list ()
               (loop 
                  for dir in dir-list
                  for merge-dir = (merge-wild-ftype dir)
                  nconcing (directory merge-dir :resolve-symlinks nil) into rtn
                  finally (return (loop for pths in rtn collect (namestring pths)))))
             (process-rotation (pathname-native)
               (let ((proc-stat 
                      (sb-ext:process-exit-code 
                       (sb-ext:run-program *image-magick-convert-path* (list "-rotate" rotation-string pathname-native pathname-native) ))))
                 ;; :NOTE Following could prob. be accomplished with an :if-error-exists arg.
                 (if (zerop proc-stat)
                     (format report-stream "~&~%successful rotation ~A of image at pathname: ~A" rotation-string pathname-native)
                     (format report-stream "~&~%failed to rotate image at pathname: ~A~&~%process exited with code ~D"
                             pathname-native proc-stat))
                 (push (cons pathname-native proc-stat)  proc-stack)))
             (all-rotations ()
               ;; (make-rotation-list)
               (dolist (dr (make-rotation-list)
                        ;;(print (setf proc-stack (nreverse proc-stack)) report-stream))
                        (setf proc-stack (nreverse proc-stack)))
                 (process-rotation dr)))
             (all-rotations-in-thread  ()
               (sb-thread:make-thread #'all-rotations  
                                      :name "rotate-image-files-in-dir-list")))
      (set special-thread-param
           (all-rotations-in-thread)))))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
