(in-package :cl-user)

(defpackage :pathnames
  (:use :common-lisp)
  (:export
    :list-directory
    :file-exists-p
    :directory-pathname-p
    :file-pathname-p
    :pathname-as-directory
    :pathname-as-file
    :walk-directory
    :directory-p
    :file-p))
