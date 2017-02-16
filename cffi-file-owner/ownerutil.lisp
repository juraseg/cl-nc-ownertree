(ql:quickload :cffi)

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
