;;; -*- mode: lisp -*-
;;; :FILE image-ops/image-ops.asd
;;; ==============================

;; ,----
;; | "I am sick to death of knee-jerk anti-LOOPism and I am beginning to
;; |  irrationally regard it as a plot to disable me as a programmer by
;; |  excommunicating my useful tools."
;; |
;; |     :SOURCE "Knee-jerk Anti-LOOPism and other E-mail Phenomena" p 17 
;; `---- :SEE http://ccs.mit.edu/papers/CCSWP150.html



(defpackage #:image-ops-build-system (:use :common-lisp :asdf))

(in-package #:image-ops-build-system)

(defsystem :image-ops
  ;; :name ""
  :author  "MON KEY"
  :maintainer "MON KEY"
  :license "MIT" 
  :description "image operations agglomerated"
  :version "1.0.0"
  :depends-on (:mon :tiff4cl) ;; :image-ops-jpeg
  :serial t    
  :components
  ((:file "package") 
   (:file "image-ops-specials")
   (:file "image-ops-macros")
   (:file "image-ops-rotate")
   (:file "image-ops-hash-dir-tree")
   (:file "image-ops-docs")
   #+IS-MON (:file "image-ops-loadtime-bind")
   ))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF

