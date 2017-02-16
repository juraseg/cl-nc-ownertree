;; Classes and functions for representing and creating owner tree
(in-package :cl-nc-ownertree)

;; Basic node for owner tree. Will represent files
(defclass node ()
  ((path :initarg :path
         :initform (error "Must supply path")
         :reader path)
   (name :reader name)
   (owner :initarg :owner
          :initform (error "Must supply owner")
          :reader owner)
   (size :initarg :size
         :reader size)
   (owner-by-count :initform nil
                   :accessor owner-by-count)
   (owner-by-size :initform nil
                  :accessor owner-by-size)
   (container-p :initform nil
                :reader container-p)
   (parent :initarg :parent
           :initform nil
           :reader parent)))

(defclass file-node (node) ())
(defmethod initialize-instance :after
    ((inst file-node) &rest args)
  (setf (slot-value inst 'name)
        (file-namestring (path inst)))
  (setf (slot-value inst 'owner-by-size)
        (acons (slot-value inst 'owner)
               (slot-value inst 'size) '()))
  (setf (slot-value inst 'owner-by-count)
        (acons (slot-value inst 'owner) 1 '())))

;; Directory node - has children
(defclass dir-node (node)
  ((size :initform 0
         :accessor size)
   (children :initform nil
             :accessor children)
   (container-p :initform t)))
(defmethod initialize-instance :after
    ((inst dir-node) &rest args)
  (setf (slot-value inst 'name)
        (concatenate 'string "/" (first (last (pathname-directory (path inst)))))))

(defgeneric node-max-size (node))
(defmethod node-max-size ((node file-node))
  (size node))
(defmethod node-max-size ((node dir-node))
  (apply #'max (mapcar #'node-max-size (children node))))

(defun inc-owner-count (owner owner-assoc &optional (by 1))
  "Make sure to use returned value, not the original list as it's a destructive function"
  (progn
    (if (null (assoc owner owner-assoc :test #'equal))
        (setf owner-assoc (acons owner by owner-assoc))
        (rplacd (assoc owner owner-assoc :test #'equal)
                (+ by (cdr (assoc owner owner-assoc :test #'equal)))))
    owner-assoc))

(defun process-directory (path &optional (parent nil))
  "Process directory and output owner tree root"
  (let ((res (make-instance 'dir-node
                            :path path
                            :owner (get-path-owner path)
                            :parent parent)))
    (loop :for path :in (uiop:directory-files path)
          :do (let* ((owner (get-path-owner (uiop:unix-namestring path)))
                     (size (get-file-size path))
                     (f-node (make-instance 'file-node
                                            :path path
                                            :owner owner
                                            :size size
                                            :parent res)))
                (setf (size res) (+ (size res) size))
                (setf (owner-by-count res)
                      (inc-owner-count owner (owner-by-count res)))
                (setf (owner-by-size res)
                      (inc-owner-count owner (owner-by-size res)
                                       size))
                (push f-node (children res))))
    (loop :for path :in (uiop:subdirectories path)
          :do (let ((subres (process-directory path res)))
               (push subres (children res))
               (setf (owner-by-count res)
                     (inc-owner-count (owner subres)
                                      (owner-by-count res)))
               (setf (size res) (+ (size res) (size subres)))
               (loop :for (owner . size) :in (owner-by-size subres)
                     :do (setf (owner-by-size res)
                              (inc-owner-count owner (owner-by-size res)
                                              size)))
               (loop :for (owner . count) :in (owner-by-count subres)
                     :do (setf (owner-by-count res)
                              (inc-owner-count owner (owner-by-count res)
                                               count)))))
    res))
