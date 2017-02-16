;;;; cl-nc-ownertree.asd

(asdf:defsystem #:cl-nc-ownertree
  :description "An app to visually view ownership of files/directories"
  :author "Yuri Abzyanov <yuri_abzyanov@fastmail.fm>"
  :license "MIT License"
  :depends-on (#:cffi
               #:uiop
               #:cl-charms)
  :serial t
  :components ((:file "package")
               (:file "fileutil" :depends-on ("package"))
               (:file "ownertreesclass" :depends-on ("package" "fileutil"))
               (:file "cl-nc-ownertree" :depends-on ("fileutil" "ownertreeclass"))))

