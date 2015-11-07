#|
  This file is a part of smart-io project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage smart-io-asd
  (:use :cl :asdf))
(in-package :smart-io-asd)

(defsystem smart-io
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:xsubseq
               :flexi-streams
               :uiop)
  :components ((:module "src"
                :components
                ((:file "smart-io"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
