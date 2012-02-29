;;; :FILE-CREATED <Timestamp: #{2011-08-26T11:55:57-04:00Z}#{11345} - by MON>
;;; :FILE image-ops/image-ops-docs.lisp
;;; ==============================

(in-package #:image-ops)
;; *package*



;;; ==============================
;;; :VARIABLES-DOCUMENTATION
;;; ==============================

(mon:vardoc '*read-image-file-list*
        "Variable holding a list of pathnames.
Variable is set by `read-image-file-list-from-file'.
It is unset with `unset-special-param-read-image-file-list'.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:vardoc '*valid-image-types*
"List of strings designating valid pathname-types.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:vardoc '*image-output-default-thumb-type*
"String designating a defualt `cl:pathname-type' to use when outputing an image type.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:vardoc '*image-magick-convert-path*
"Path to an ImageMagick exectuable.~%~@
Its value is used by procedures which exectue `sb-ext:run-program'.~%~@
:SEE-ALSO `mon::verify-image-magic-convert-path'.~%▶▶▶")

(mon:vardoc '*walk-directory-ignorables* 
"List of directory or filename components which osicat:walk-directory should ignore.~%~@
Directory names should include traling #\/ \(solidus\).~%~@
Filenames should _not_ be preceded with leading #\/ \(solidus\).~%~@
:NOTE elements of this list are checked with `cl:string='.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:vardoc '*psd-hash*
"A hash-table storing pathname information of located files with a `cl:pathname-type' `cl:string-equal' to \"psd\".~%~@
Its `cl:hash-table-test' is `cl:equal'.~%~@
 It should satisfy `sb-ext:hash-table-synchronized-p'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `*jpg-gz-hash*', `*jpg-hash*', `*bmp-hash*', `*bmp-gz-hash*', `*nef-hash*',
`*tiff-hash*', `*psd-hash*', `*other-hash*'.~%▶▶▶")

(mon:vardoc '*jpg-gz-hash*
        "A hash-table storing pathname information of located JPEG files with `cl:pathname-type' \".gz\".~%~@
Its `cl:hash-table-test' is `cl:equal'.~%~@
 It should satisfy `sb-ext:hash-table-synchronized-p'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `*jpg-gz-hash*', `*jpg-hash*', `*bmp-hash*', `*bmp-gz-hash*', `*nef-hash*',
`*tiff-hash*', `*psd-hash*', `*other-hash*'.~%▶▶▶")

(mon:vardoc '*jpg-hash*
"A hash-table storing pathname information located JPEG files with
`cl:pathname-type' which is `cl:string-equal' to either\"jpg\" or \"jpeg\".~%~@
Its `cl:hash-table-test' is `cl:equal'.~%~@
 It should satisfy `sb-ext:hash-table-synchronized-p'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `*jpg-gz-hash*', `*jpg-hash*', `*bmp-hash*', `*bmp-gz-hash*', `*nef-hash*',
`*tiff-hash*', `*psd-hash*', `*other-hash*'.~%▶▶▶")

(mon:vardoc '*bmp-hash*
"A hash-table storing pathname information of located files with a `cl:pathname-type' which is `cl:string-equal' to .~%~@
Its `cl:hash-table-test' is `cl:equal'.~%~@
 It should satisfy `sb-ext:hash-table-synchronized-p'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `*jpg-gz-hash*', `*jpg-hash*', `*bmp-hash*', `*bmp-gz-hash*', `*nef-hash*',
`*tiff-hash*', `*psd-hash*', `*other-hash*'.~%▶▶▶")

(mon:vardoc '*bmp-gz-hash*
"A hash-table storing pathname information of located bitmap files with a
`cl:pathname-type' which is `cl:string-equal' to \"gz\".~%~@
Its `cl:hash-table-test' is `cl:equal'.~%~@
 It should satisfy `sb-ext:hash-table-synchronized-p'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `*jpg-gz-hash*', `*jpg-hash*', `*bmp-hash*', `*bmp-gz-hash*', `*nef-hash*',
`*tiff-hash*', `*psd-hash*', `*other-hash*'.~%▶▶▶")

(mon:vardoc '*nef-hash*
        "A hash-table storing pathname information of located NEF files with a `cl:pathname-type' which is `cl:string-equal' to \"nef\".~%~@
Its `cl:hash-table-test' is `cl:equal'.~%~@
 It should satisfy `sb-ext:hash-table-synchronized-p'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `*jpg-gz-hash*', `*jpg-hash*', `*bmp-hash*', `*bmp-gz-hash*', `*nef-hash*',
`*tiff-hash*', `*psd-hash*', `*other-hash*'.~%▶▶▶")

(mon:vardoc '*tiff-hash*
"A hash-table storing pathname information of located TIFF files with a
`cl:pathname-type' which is `cl:string-equal' to either \"tif\" or \"tiff\".~%~@
Its `cl:hash-table-test' is `cl:equal'.~%~@
 It should satisfy `sb-ext:hash-table-synchronized-p'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `*jpg-gz-hash*', `*jpg-hash*', `*bmp-hash*', `*bmp-gz-hash*', `*nef-hash*',
`*tiff-hash*', `*psd-hash*', `*other-hash*'.~%▶▶▶")

(mon:vardoc '*other-hash*
        "A hash-table storing pathname information of located files whch does not match any of the existing image regexps:
 `*jpg-scanner*'`*jpg-gz-scanner*'
 `*bmp-scanner*' `*bmp-gz-scanner*'
 `*nef-scanner*' `*tiff-scanner*'
 `*psd-scanner*'~%~@
Its `cl:hash-table-test' is `cl:equal'.~%~@
 It should satisfy `sb-ext:hash-table-synchronized-p'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `*jpg-gz-hash*', `*jpg-hash*', `*bmp-hash*', `*bmp-gz-hash*', `*nef-hash*',
`*tiff-hash*', `*psd-hash*', `*other-hash*'.~%▶▶▶")

;; (mon:vardoc '*psd-scanner*
;; (mon:vardoc '*jpg-gz-scanner*
;; (mon:vardoc '*jpg-scanner*
;; (mon:vardoc '*bmp-scanner*
;; (mon:vardoc '*bmp-gz-scanner*
;; (mon:vardoc '*nef-scanner*
;; (mon:vardoc '*tiff-scanner*


;;; ==============================
;;; :FUNCTIONS-DOCUMENTATION
;;; ==============================

(mon:fundoc 'with-hash-table-op
            "Execute BODY with HASH-VAR dynmically bound to HASH-TABLE.~%~@
On SBCL if HASH-TABLE satisfies `sb-ext:hash-table-synchronized-p' BODY is
evaluated inside `sb-ext:with-locked-hash-table'.~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(with-hash-table-op \(bubba *bmp-gz-hash*\)
                  \(with-open-file \(f \(make-pathname :directory
                                                    \(pathname-directory #P\"/tmp/\"\)
                                                    :name \"testing\"\)
                                   :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede
                                   :element-type 'character\)
                    \(maphash #'\(lambda \(key value\)
                                 \(print \(list key value\) f\)\)
                     bubba\)\)\)\)
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'verify-image-magic-convert-path
"Return T if variable `*image-magick-convert-path*' is `boundp' and its value is `string-not-empty-p.~%~@
An error is signaled if not.~%~@
Procedures which execute `sb-ext:run-program' with *image-magick-convert-path*
as the COMMAND arg should invoke this function first.~%~@
:EXAMPLE~%
 \(verify-image-magic-convert-path\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'verify-image-file-file-kind
" Whether MAYBE-IMAGE-FILE-FILE satisfies `mon:pathname-or-namestring-not-empty-dotted-or-wild-p'.~%~@
If so, return its `cl:pathname' representation.
If not when ERROR-ON-WILD-EMPTY-DOTTED is null return nil, else signal an error.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'verify-image-file-output-type
"Whether MAYBE-VALID-OUTPUT-EXTENSION is cl:string-equal an element in `*valid-image-types*'.~%~@
If so, return its `cl:string-downcase'd representation else signal an error.~%~@
:EXAMPLE~%
 \(mapcar #'verify-image-file-output-type 
         \(mapcar #'string-upcase *valid-image-types*\)\)~%
 \(verify-image-file-output-type \"bubba\"\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'unset-special-param-read-image-file-list
"Unset the value of SPECIAL-PARAM.~%~@
:EXAMPLE~%
 \(progn
   \(setf *read-image-file-list* \(list \"bubba\" \"BUBBA\" \"Bubba\"\)\)
   \(unset-special-param-read-image-file-list '*read-image-file-list*\)\) 
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'read-image-file-list-from-file
"Read the list of pathnames stored in PATHNAME-OR-NAMESTRING set the list read
as value of SPECIAL-PARAM.~%~@
Keyword SPECIAL-PARAM is a special parameter to use when holding a list of
image file pathnames. Default is `mon:*read-image-file-list*'.~%~@
Keyword EXTERNAL-FORMAT is as if by `cl:open'. Default value is :default.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'make-target-pathname-for-image-resize
        "Return a pathname for use in image resizing operations~%~@
Return value is a cons of the form:~%
 \( <SOURCE-PATHAME> . <GENERATED-DESTINATION-PATHNAME>\)~%~@
SOURCE-PATHNAME is an object of type `mon:pathname-or-namestring' designating an
existing image file. Its `cl:pathname-name' is used as the template when
generating the returned pathname.~%~@
TARGET-DIRECTORY is an object of type `mon:pathname-or-namestring' designating a
directory in which the resized image will be located.~%
TARGET-TYPE is a string designating a `cl:pathname-type' image file extension.
It should satisfy `mon:verify-image-file-output-type'.~%~@
Keyword PREFIX-NAME-WITH is a string to prepend to SOURCE-PATHNAME's `cl:pathname-name'.~%~@
Keyword SUFFIX-NAME-WITH is a string to append to SOURCE-PATHNAME's `cl:pathname-name'.~%~@
:EXAMPLE~%
 \(make-target-pathname-for-image-resize
  #P\"/some/source/path/to/existing/image-of-type-bitmpap.bmp\" 
  :target-directory #P\"/some/destination/path/for/resized/image/\" 
  :target-type \"jpg\"
  :prefix-name-with \"prepended-\" 
  :suffix-name-with \"-appended\"\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'make-pathname-source-destination-resize-pairs
        "Return a list of consed source/target image pairs.~%~@
READ-SOURCE-FILES-FROM is an object of type `mon:pathname-or-namestring' its
contents are processed with `mon:read-image-file-list-from-fprint0-file'.
Keywords TARGET-DIRECTORY TARGET-TYPE PREFIX-NAME-WITH SUFFIX-NAME-WITH are as
per `mon:make-target-pathname-for-image-resize'.~%~@
:EXAMPLE~%
 \(make-pathname-source-destination-resize-pairs
  #P\"/some/file/with/null-byte/delimited/image-file-names\"
  #P\"/some/source/path/to/existing/image-of-type-bitmpap.bmp\"
  :target-directory #P\"/some/destination/path/for/resized/image/\"
  :target-type \"jpg\"
  :prefix-name-with \"prepended-\"
  :suffix-name-with \"-appended\"\)
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'write-fprint0-file-for-image-files-in-pathname
        "Write a #\Nul terminated list of files for files contained of SEARCH-DIRECTORY matching SEARCH-TYPE.~%~@
Return the pathname of file so written.~%~@
SEARCH-DIRECTORY names an existing directory containing image files of type SEARCH-TYPE
SEARCH-TYPE is a string naming a pathname-type image extension satisfying `verify-image-file-output-type'.
APPEND-SUFFIX is a string to append to the file written. Defaul is as per `time-string-yyyy-mm-dd'.~%~@
DEST-PATHNAME is a pathname-or-namestring specifying the destination to write to 
if provided it must not be `pathname-or-namestring-not-empty-dotted-or-wild-p'.
When DEST-PATHNAME is not provided a pathname is generated and written to
SEARCH-DIRECTORY its namestring has the following form:
 <SEARCH-DIRECTORY>/process-files-<SEARCH-TYPE>-<APPEND-SUFFIX>
:EXAMPLE~%
 \(write-fprint0-file-for-image-files-in-pathname 
  :search-directory \"/some/path/full/of/tif/files/\"
  :search-type \"tif\"\)~%
 \(write-fprint0-file-for-image-files-in-pathname 
  :search-directory \"/some/path/full/of/tif/files/\"
  :search-type \"tif\"
  :append-suffix \"bubba\"\)~%
 \(write-fprint0-file-for-image-files-in-pathname 
  :search-directory \"/some/path/full/of/tif/files/\"
  :search-type \"tif\"
  :dest-pathname \"/some/path/full/of/tif/files/dump-file\"\)~%
:SEE-ALSO `read-image-file-list-from-fprint0-file', `resize-image-files-in-fprint0-file'.~%▶▶▶")

(mon:fundoc 'read-image-file-list-from-fprint0-file
        "Read the #\\Nul character terminated pathnames contained of PATHNAME-OR-NAMESTRING.~%~@
Return a list of strings with each null terminated pathname split on the
terminating #\\Nul character with #\\Nul char removed.~%~@
Occurences of #\\Newline and #\\Return are elided from results.~%~@
:NOTE A #\\Nul character terminated pathname is the default output for the unix
command `find` when it used invoked the -frint0 arg.~%~@
Keyword SPECIAL-PARAM is a special variable to bind results to. Default is `mon::*read-image-file-list*'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `write-fprint0-file-for-image-files-in-pathname', `resize-image-files-in-fprint0-file'.~%▶▶▶")

(mon:fundoc 'resize-image-files-in-fprint0-file
"Resize each null-terminated pathname in FPRINT0-FILE.~%~@
Keyword resize-x is an unsigned integer value.
Keywords TARGET-DIRECTORY, TARGET-TYPE, PREFIX-NAME-WITH, and SUFFIX-NAME-WITH are as per 
`mon:make-pathname-source-destination-resize-pairs'.
When SUFFIX-NAME-WITH is not explicitly provided the value of RESIZE-X is
appended to the resized imaged saved to TARGET-DIRECTORY.~%~@
:EXAMPLE~%
 \(let* \(\(null-list-directory #P\"/some/directory/with/bitmaps/\" \)
       \(null-list-pathname  \(merge-pathnames 
                             \(make-pathname :name \"null-terminated-file-list\"\)
                             null-terminated-file-list\)\)
       \(sb-ext:run-program \"/usr/bin/find\" \(list \(namestring null-list-directory\)
                                                 \"-type\" \"f\" \"-name\" \"*.bmp\" \"-fprint0\"
                                                 \(namestring null-list-pathname\)\)\)
       \(mon::resize-image-files-in-fprint0-file null-list-pathname 
                                                :target-directory null-list-directory 
                                                :target-type \"jpg\" 
                                                :resize-x 1000\)\)\)~%
\(resize-image-files-in-fprint0-file 
 \(write-fprint0-file-for-image-files-in-pathname 
  :search-directory \"/some/path/full/of/tif/files/\"
  :search-type \"tif\"\)
 :target-directory \"/some/path/soon/full/of/jpg/files/\"
 :target-type \"jpg\" 
 :resize-x 1000\)~%~@
:SEE-ALSO `read-image-file-list-from-fprint0-file', `write-fprint0-file-for-image-files-in-pathname'.~%▶▶▶")

(mon:fundoc 'rotate-image-files-in-dir-list
"Rotate each image found in the directories of DIR-LIST by DEGREES POSITIVE-OR-NEGATIVE.~%~@
Rotation is performed with external command `convert` from the ImageMagick suite.~%~@
DIR-LIST is a list of pathnames designating existing directories. No symlink detection is performed.
Keyword IMAGE-TYPE names and image file extension.
It is a string with the same format as the :type argument to `cl:make-pathname'.
Valid values are limited to the following case sensitve image type extensions:~%
 \"jpg\" \"jpeg\" \"tiff\" \"tif\" \"bmp\" \"png\"~%~@
Keyword DEGREES is an integer in the range [1,359] designating a degree of
rotation to apply to images.~%~@
Keyword POSITIVE-OR-NEGATIVE is a keyword designating whether rotation is
positive or negative. Valid values are:~%
 :POSITIVE :NEGATIVE :CLOCKWISE :COUNTER-CLOCKWISE~%~@
SPECIAL-THREAD-PARAM is a symbol naming a special variable which holds the
thread object this function exececutes in. Default is mon::*rotate-images-thread*.~%
:USAGE~%
 \(rotate-image-files-in-dir-list
 \(list #P\"/mnt/some/path/to/goofy/1351/\"
        #P\"/mnt/some/path/to/goofy/1353/\"
        #P\"/mnt/some/path/to/goofy/1515/\"
        #P\"/mnt/some/path/to/goofy/1535/\"\)
 :image-type \"jpeg\" :degrees 90 :positive-or-negative :counter-clockwise\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'walk-directory-images-to-hash
        "Walk each directory in DIRECTORY-PATHNAME find all files which match a regexp
closure and set the filename of each match as the key in an appropriate
hash-table.~%~@
If file is found which does not match a regexp closure and it is not a
symbolic-linke push it to the hash-table `*other-hash*'.~%~@
Return a list hash-table counts as if by `image-hash-counts-report'.~%~@
 REGEXP-CLOSURE     HASH-TABLE
 -------------------------------
 `*psd-scanner*'     `*psd-hash*'
 `*jpg-gz-scanner*'  `*jpg-gz-hash*'
 `*jpg-scanner*'     `*jpg-hash*'
 `*bmp-scanner*'     `*bmp-hash*'
 `*bmp-gz-scanner*'  `*bmp-gz-hash*'
 `*nef-scanner*'     `*nef-hash*'
 `*tiff-scanner*'    `*tiff-hash*'~%~@
Unless keyword CLEAR-COUNTS is nil on each entry to this function the
existing counts are cleared with `image-hash-reset-all'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'image-hash-reset-all
"Evaluate `cl:clrhash' for the following hash-tables:
 `*jpg-hash*'   `*jpg-gz-hash*'
 `*bmp-hash*'   `*bmp-gz-hash*'
 `*nef-hash*'   `*tiff-hash*'
 `*other-hash*' `*psd-hash*'
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `image-hash-counts-report'.~%▶▶▶")

(mon:fundoc 'image-hash-counts-report
"Return a list of `cl:hash-table-count's for following hash-tables:~%
 `*jpg-hash*'   `*jpg-gz-hash*'
 `*bmp-hash*'   `*bmp-gz-hash*'
 `*nef-hash*'   `*tiff-hash*'
 `*other-hash*' `*psd-hash*'~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `image-hash-reset-all'.~%▶▶▶")

(mon:fundoc 'image-hash-write-to-file
 "Write contents of the image-hash HASH-TABLE to a time-stamped file in a deserializable format.~%~@
Return pathname of file written to.~%~@
DIRECTORY-PATHNAME names a directory on is created it if it does not already exist.~%~@
HASH-TABLE-NAME is a string describing a hash-table it is used when generating
the filename to be written to DIRECTORY-PATHNAME.~%~@
The namestring of the file written to will have the format:~%
 <DIRECTORY-PATHNAME><HASH-TABLE-NAME>-YYYY-MM-DD~%~@
For each key value pair in HASH-TABLE write a string of 68 #\\; characters
followed by a sequence with the form:~%
 \(:FILE      \"<FULLY-QUALIFED-NAMESTRING>\"
  :DIRECTORY \"<FULLY-QUALIFED-DIRECTORY-NAMESTRING>\"
  :NAME      \"<PATHNAME-NAME>\"
  :TYPE      \"<PATHNAME-TYPE>\"\)~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `image-hash-write-all-to-file'.~%▶▶▶")

(mon:fundoc 'image-hash-write-all-to-file
        "Write as if by `image-hash-write-to-file' the contents of the all hash-tables
in the following set of special variables:~%
 `*bmp-hash*'  `*bmp-gz-hash*'
 `*jpg-hash*'  `*jpg-gz-hash*'
 `*tiff-hash*' `*nef-hash*' 
 `*psd-hash*'  `*other-hash*'~%~@
Return a list of the pathnames written to.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'image-hash-map-conversion-extension
        "With filename keys of SOURCE-HASH convert the value to a filename with
CONVERSION-EXTENSION and storing the original as a new key in CONVERSION-HASH
with the conversion filename as value.~%~@
Return CONVERSION-HASH.~%~@
Keyword CLEAR-CONVERSION is a boolean, when T keys of CONVERSION-HASH will be
reset as if by `cl:clrhash' prior to mapping new keys.~%~@
:EXAMPLE~%
 \(image-hash-map-conversion-extension *bmp-gz-hash* *source-dest-conversion-hash* \"tif\"\)~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'image-hash-conversion-perform
"For each key/value pair in CONVERSION-HASH convert from key to val logging results to LOG-FILE.~%~@
:EXAMPLE~%~@
 \(defparameter *long-walking-thread*
   \(image-hash-map-conversion-perform
    \(image-hash-map-conversion-extension 
     *bmp-gz-hash* *source-dest-conversion-hash*
     \"tif\"
     :clear-conversion t\)
    \(make-pathname :directory \(pathname-directory #P\"/some/logging/path/\"\)
                   :name \(concatenate 'string \"bmp-to-tiff-conversion-\" \(mon:time-string-yyyy-mm-dd\)\)\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(mon:fundoc 'image-hash-write-conversion-hash-to-file
"Like `image-hash-write-to-file' but prints a sexp for each key/value pair
contained of CONVERSION-HASH-TABLE.~%~@
Unlike `image-hash-write-to-file' prints the `cl:pathname' of
CONVERSION-HASH-TABLE's key/value pairs.~%~@
Return pathname of file written to where files `cl:pathname-directory' is
DIRECTORY-PATHNAME and where files `cl:pathname-name' is generated by
cocatenating CONVERSION-HASH-TABLE-NAME with the current date as if by
`mon:time-string-yyyy-mm-dd'~%~@
Keyword W-COMMENT-DELIMIT is a boolean. When T each key/val sexp written is
preceded by a comment line contained fo 68 #\\; characters.~%~@
:EXAMPLE~%
 \(image-hash-write-conversion-hash-to-file *source-dest-conversion-hash* 
                                           \(make-pathname :directory '\(:absolute \"tmp\"\)\)
                                           \"deserializable-conversion-log\"\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;; ==============================
;; image-ops/image-ops-iphone.lisp
;;; ==============================

(mon:fundoc 'translate-pathname-iphone-image
"Return a converted iphone image pathname.~%~@
Given a pathname having either of the forms:~%
 #P\"/foo/bar/IMG_NNNN.JPG\"
 #P\"/foo/bar/IMG_NNNN.jpg\"
return value is of the form:~%
 #P\"/foo/bar/NNNN-<FILE-WRITE-DATE>.jpg\"~%
If PATHNAME-OR-NAMESTRING does not match the patter above return NIL.~%~@
PATHNAME-OR-NAMESTRING must name an existing file, an error is signaled if not.~%~@
:EXAMPLE~%
 \(translate-pathname-iphone-image \"/foo/bar/3936_SOLD/IMG_0319.jpg\"\)~%
 \(translate-pathname-iphone-image \"/foo/bar/IMG_0319.JPG\"\)~%
 \(null \(translate-pathname-iphone-image \"/foo/bar/IMG_0319mmmmm.JPG\"\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
