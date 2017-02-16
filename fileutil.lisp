;; interface to UNIX system calls, which get file/folder owner and size
(in-package :cl-nc-ownertree)

(cffi:load-foreign-library "ownerutil.so")

(cffi:defcfun get_owner :int
  (filepath :string)
  (str :pointer)
  (size :int))

(cffi:defcfun get_gowner :int
  (filepath :string)
  (str :pointer)
  (size :int))

(defun get-path-owner(path)
  (cffi:with-foreign-pointer-as-string (res 100)
    (get_owner (namestring path) res 100)))

(defun get-path-group-owner(path)
  (cffi:with-foreign-pointer-as-string (res 100)
    (get_gowner (namestring path) res 100)))

(defun get-file-size(path)
  ;; TODO: make it round up to sector size
  ;; TODO: may be use system call to get it instead of this workaround
  (with-open-file (in path
                      :element-type '(unsigned-byte 8))
    (file-length in)))
