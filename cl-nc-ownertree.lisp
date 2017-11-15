;;;; cl-nc-ownertree.lisp

(in-package #:cl-nc-ownertree)

;;; "cl-nc-ownertree" goes here. Hacks and glory await!

(defun main (&optional namestring)
  (let* ((path (if namestring (uiop:parse-unix-namestring namestring)
                  *default-pathname-defaults*))
         (tree-root (process-directory path)))
    (charms:with-curses ()
      (charms:disable-echoing)
      (charms:enable-raw-input :interpret-control-characters t)
      (charms/ll:curs-set 0)
      (let ((window (make-instance 'main-window
                                   :tree-root tree-root)))
        ;; TODO: show calculation window
        ;; a loop which waits for "q" key to quit
        (draw-window window)
        (main-loop window)))))

(defun main-loop (window)
  (let ((wnd (wnd window)))
    (loop :named driver-loop
          :for c := (charms:get-char wnd :ignore-error t)
          :do (progn
                ;; Refresh the window
                (charms:refresh-window wnd)

                ;; Process input
                (case c
                  ((#\q #\Q) (return-from driver-loop))
                  ((#\k \#K) (go-up-list window))
                  ((#\j \#J) (go-down-list window))
                  ((#\i #\I) (go-in-list window))
                  ((#\o #\O) (go-out-list window))
                  (t (draw-window-footer window (format nil "Pressed: ~A ~S" c c))))))))

(defclass main-window()
  ((wnd :reader wnd)
   (path :initarg :path
         :initform ""
         :reader path)
   (tree-root :initarg :tree-root
              :initform (error "Must supply tree root")
              :reader tree-root)
   (items :initform '()
          :reader items)
   (selected :initform 0
             :reader selected)))
(defmethod initialize-instance :after
    ((inst main-window) &rest args)
  (setf (slot-value inst 'path) (path (tree-root inst)))
  (setf (slot-value inst 'wnd) (charms:make-window 0 0 0 0))
  (setf (slot-value inst 'items)
        (sort-items (children (tree-root inst)))))

(defgeneric init-window (window tree-root &optional selected selected-path))
(defmethod init-window ((window main-window) (tree-root node) &optional selected selected-path)
  (setf (slot-value window 'path) (path tree-root))
  (setf (slot-value window 'tree-root) tree-root)
  (setf (slot-value window 'items) (sort-items (children tree-root)))
  (let ((selected-val
          (cond
            ((and (not selected) selected-path)
             (position-if (lambda (item)
                            (equal (path item) selected-path))
                          (items window)))
            (t 0))))
    (setf (slot-value window 'selected) selected-val)))

(defgeneric go-up-list (window))
(defmethod go-up-list ((window main-window))
  (with-slots (selected) window
    (if (> selected 0)
       (progn
         (setf selected (1- selected))
         (draw-window-items window)))))

(defgeneric go-down-list (window))
(defmethod go-down-list ((window main-window))
  (with-slots (selected) window
    (if (< (+ selected 1) (length (children (tree-root window))))
       (progn
         (setf selected (1+ selected))
         (draw-window-items window)))))

(defgeneric go-in-list (window))
(defmethod go-in-list ((window main-window))
  (let ((selected-item (nth (selected window) (items window))))
    (if (not (container-p selected-item))
        (draw-window-footer window
                            (format nil "Not a folder: ~a"
                                    (name selected-item)))
        (progn
          (draw-window-footer window
                              (format nil "Parent of selected item: ~a"
                                      (parent selected-item)))
          (init-window window selected-item)
          (draw-window window)))))

(defgeneric go-out-list (window))
(defmethod go-out-list ((window main-window))
  (let* ((current-item (tree-root window))
         (parent-item (parent current-item)))
    (if parent-item
        (progn
          (init-window window parent-item nil (path current-item))
          (draw-window window))
        (draw-window-footer window
                            (format nil "No parent: ~a" parent-item)))))

(defgeneric draw-window (window))
(defmethod draw-window ((window main-window))
  (cl-charms:clear-window (wnd window))
  (draw-window-title window)
  (draw-window-footer window)
  (draw-window-items window))

(defgeneric draw-window-title (window))
(defmethod draw-window-title ((window main-window))
  (let ((wnd (wnd window))
        (title "nc-owner-tree 0.1 - in progress...")
        (path-namestring (uiop:unix-namestring (path window))))
    (multiple-value-bind (wincols winrows)
        (charms:window-dimensions wnd)
      (draw-white-hline-with-text wnd 0 title)
      (draw-white-hline wnd (1- winrows) wincols)
      (draw-dashed-hline-with-text wnd 1 path-namestring))))

(defgeneric draw-window-footer (window &optional text))
(defmethod draw-window-footer ((window main-window) &optional text)
  (let ((wnd (wnd window))
        (text (if text
                  text
                  (make-total-info-text (tree-root window)))))
    (multiple-value-bind (windcols winrows)
        (charms:window-dimensions wnd)
      (draw-white-hline-with-text wnd (1- winrows) text))))

(defgeneric draw-window-items (window))
(defmethod draw-window-items ((window main-window))
  (let* ((wnd (wnd window))
         (owners (alist-keys (owner-by-size (tree-root window))))
         (max-size (node-max-size (tree-root window)))
         (selected-index (selected window))
         (start 3))
    (multiple-value-bind (wincols winrows)
        (charms:window-dimensions wnd)
      (display-list-header wnd (1- start) owners)
      (loop :for node :in (items window)
            :for i :upfrom 0
            :do (display-node wnd (+ start i) node (= i selected-index) owners max-size)))))

(defun display-list-header (wnd pos owners)
  (display-list-item wnd pos (make-list-item-text "Size" owners "Name") nil))

(defun display-node (wnd pos node selected-p owners max-size)
  (let* ((owners-list (make-node-owner-list-text node owners max-size))
         (text (make-list-item-text (make-size-text (size node))
                                    owners-list (name node))))
    (display-list-item wnd pos text selected-p)))

(defun display-list-item (wnd pos item-txt selected-p)
  (if selected-p
      (draw-white-hline-with-text wnd pos item-txt)
      (draw-black-hline-with-text wnd pos item-txt)))

(defun draw-white-hline (wnd y &optional (n nil))
  (if (not n)
      (multiple-value-bind (wincols winrows)
          (charms:window-dimensions wnd)
        (setf n wincols)))
  (let ((wnd-ptr (charms::window-pointer wnd)))
    (charms/ll:wattron wnd-ptr charms/ll:A_REVERSE)
    (charms/ll:mvwhline wnd-ptr y 0 (char-code #\ ) n)
    (charms/ll:wattroff wnd-ptr charms/ll:A_REVERSE)))

(defun draw-black-hline-with-text (wnd y txt &optional (n nil))
  (draw-hline-with-text wnd y txt nil n))

(defun draw-white-hline-with-text (wnd y txt &optional (n nil))
  (draw-hline-with-text wnd y txt t n))

(defun draw-hline-with-text (wnd y txt white-p &optional (n nil))
  (if (not n)
      (multiple-value-bind (wincols winrows)
          (charms:window-dimensions wnd)
        (setf n wincols)))
  (let ((wnd-ptr (charms::window-pointer wnd)))
    (if white-p (charms/ll:wattron wnd-ptr charms/ll:A_REVERSE))
    (charms/ll:mvwhline wnd-ptr y 0 (char-code #\ ) n)
    (charms/ll:mvwprintw wnd-ptr y 0 txt)
    (if white-p (charms/ll:wattroff wnd-ptr charms/ll:A_REVERSE))))

(defun draw-dashed-hline-with-text (wnd y txt &optional (n nil))
  (if (not n)
      (multiple-value-bind (wincols winrows)
          (charms:window-dimensions wnd)
        (setf n wincols)))
  (let ((wnd-ptr (charms::window-pointer wnd)))
    (charms/ll:mvwhline wnd-ptr y 0 (char-code #\-) n)
    (charms/ll:mvwprintw wnd-ptr y 3 (concatenate 'string " " txt " "))))

(defun make-total-info-text(tree-root)
  (let ((size (loop :for (owner . owner-size) :in (owner-by-size tree-root)
                    :sum owner-size)))
    (format nil "Total size: ~a" (make-size-text size))))

(defun make-node-owner-list-text (node owners max-size)
  (loop for o in owners
         collect
         (let ((owner-size (cdr (assoc o (owner-by-size node) :test #'equal))))
           (make-percentage-bar
            (if (not owner-size)
                0
                (/ owner-size max-size))))))

(defun make-list-item-text (size-text owners other-text)
  (format nil "   ~10@a ~{~14@a ~} ~a" size-text owners other-text))

(defun make-percentage-bar (proportion)
  (format nil "[~{~a~}]" (loop :for i :from 1 :upto 10
                               :if (< proportion (/ i 10))
                                 :collect " "
                               :else
                                 :collect "#")))

(defun make-size-text (size)
  (cond ((< size 1024) (format nil "~1$  B" size))
        ((< size (* 1024 1024)) (format nil "~1$ KB" (/ size 1024)))
        ((< size (* 1024 1024 1024)) (format nil "~1$ MB" (/ size 1024 1024)))
        ((< size (* 1024 1024 1024 1024)) (format nil "~1$ GB" (/ size 1024 1024 1024)))
        ((< size (* 1024 1024 1024 1024 1024)) (format nil "~1$ TB" (/ size 1024 1024 1024 1024)))
        (nil "too big")))

(defun alist-keys (alist)
  (loop :for (key . value) :in alist
        :collect key))

(defun sort-items (items)
  (sort (copy-list items) #'> :key #'size))

