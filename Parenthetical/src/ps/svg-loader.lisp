;;; -*- lisp -*-

(defvar *svg-cache* (create))

;;; *x-m-l-http-request ready-state values
(defconstant +request-not-initialized+  0)
(defconstant +connection-established+   1)
(defconstant +request-received+         2)
(defconstant +processing-request+       3)
(defconstant +response-ready+           4)

;;; HTTP status codes by hundreds
(defconstant +http-ok+         200)
(defconstant +http-relocated+  300)
(defconstant +http-failed+     400)
(defconstant +http-error+      500)

(defun split-chunk-from-u-r-l (url)
  (TODO "Split everything after # off the url and return as a two-part list.")
  (return (list url "")))

(defun position-billboard (object x y z)
  (TODO))

(defun scale-object (object scalar)
  (cond
    ((and (< scalar 1.00001) (> scalar .99999)) object)
    (t (TODO))))

(defun find-number-end (string start-from)
  (string-skip '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)
               (subseq string start-from)))

(defun get-next-number (string)
  (let ((end (if (char= (elt string 0) #\-)
                 (find-number-end string 1)
                 (find-number-end string 0)))
        (value (parse-float string)))
    (list value end)))

(defun string-skip (skips string)
  (let ((i 0))
    (while (< i ((@ string length)))
      (when (not (member char skips))
        (return i))
      (incf i))
    i))

(defun read-coords-s-v-g (string)
  (let ((index 0) x y)
    (let ((got-x (get-next-number string)))
      (setf x (first got-x))
      (incf index (second got-x)))
    (incf index (string-skip '(#\, #\Space) (subseq string index)))
    (let ((got-y (get-next-number (subseq string index))))
      (setf y (first got-y))
      (incf index (second got-y)))
    (list x y index)))

(defun draw-path-s-v-g (dynamic shape path-string)
  (let ((cmd-char (elt path-string 0))
        (next-index 0))
    (ecase cmd-char
      (#\M ; move
       (let ((x y))
         (let ((got-pair (read-coords-s-v-g path-string)))
           (setf x (first got-pair))
           (setf y (second got-pair))
           (setf next-index (third got-pair)))
         (setf (@ dynamic shape-current-x) x)
         (setf (@ dynamic shape-current-y) y)))
      (#\L ; line
       (let ((x y))
         (let ((got-pair (read-coords-s-v-g path-string)))
           (setf x (first got-pair))
           (setf y (second got-pair))
           (setf next-index (third got-pair)))
         (TODO "generate a line from shape-current-x,y to x,y")))
      (#\Q ; spline
       (let ((mid-x mid-y end-x end-y))
         (let ((got-pair (read-coords-s-v-g path-string)))
           (setf mid-x (first got-pair))
           (setf mid-y (second got-pair))
           (setf next-index (third got-pair)))
         (let ((got-pair (read-coords-s-v-g (subseq path-string next-index))))
           (setf end-x (first got-pair))
           (setf end-y (second got-pair))
           (setf next-index (third got-pair)))
         (TODO "generate a line from shape-current-x,y through mid-x,y to end-x,y")))
      (#\Space
       (setf next-index (string-skip '(#\Space) path-string))))
    (if (< next-index (length path-string))
     (draw-path-s-v-g dynamic shape (subseq path-string next-index))
     shape)))

(defun draw-s-v-g (node dynamic shape)
  (ecase (@ node local-name)
    ('path (draw-path-s-v-g dynamic shape ((@ node get-attribute 'd))))))

(defun apply-s-v-g-view-box (view-box-string shape)
  (TODO)
  shape)

(defun parse-s-v-g-document-node (node dynamic)
  (unless (string= ((@ node get-attribute) 'version)
                   "1.1")
    (error "expected SVG 1.1 version only"))
  (dolist (child ((@ node get-child-nodes)))
    (parse-s-v-g child dynamic)))

(defun parse-s-v-g-children (node dynamic)
  (dolist (child ((@ node get-child-nodes)))
    (parse-s-v-g child dynamic)))

(defun parse-s-v-g-symbol (node dynamic)
  (setf (@ dynamic symbols ((@ node get-attribute) 'id))
        (apply-s-v-g-view-box 
         ((@ node get-attribute) 'view-box)
         (let ((shape (create)))
           (dolist (child ((@ node get-child-nodes)))
             (draw-s-v-g child dynamic shape))
           shape))))

(defun apply-transformation-s-v-g (shape dynamic transform-string)
  (cond
    ((string= (subseq transform-string 0 6) "matrix")
     (let ((next-index 6) (mat ([])))
       (incf next-index (string-skip '(#\Space #\() (subseq transform-string next-index)))
       (dotimes (i 6)
         (let ((got (get-next-number (subseq transform-string next-index))))
           (setf (aref mat i) (first got))
           (incf next-index (second got))
           (incf next-index (string-skip '(#\Space #\, #\))
                                         (subseq transform-string next-index)))))
       (TODO "convert to, and apply, transformation in GL")))
    (t (error "unsupported transformation"))))

(defun use-s-v-g-reference (node dynamic)
  (let ((shape (find-s-v-g-symbol ((@ node get-attribute) 'href))))
    (if-let ((transform ((@ node get-attribute) 'transform)))
            (apply-transformation-s-v-g shape dynamic transform)
            shape)))

;;; note to self: set-lookat might be the method to create billboards.

(defun parse-s-v-g (node dynamic)
  (ecase (@ node local-name)
    ('svg      (parse-s-v-g-document-node node dynamic))
    ('title    (setf (@ dynamic title) ((@ node node-value))) nil) 
    ('desc     (setf (@ dynamic desc) ((@ node node-value))) nil)
    ('defs     (parse-s-v-g-children node dynamic))
    ('symbol   (parse-s-v-g-symbol node dynamic))
    ('g        (parse-s-v-g-group node dynamic))
    ('use      (use-s-v-g-reference node dynamic))
    ('animate  (TODO 'animate))))

(defun import-s-v-g (resource x y z &optional (scalar 1.0))
  (cond 
    ((= 'string (typeof resource))
     (destructuring-bind (url asset) (split-chunk-from-u-r-l url)
       (load-s-v-g url (lambda (loaded)
                         (import-s-v-g loaded x y z scalar)) asset)))
    (t (position-billboard 
        (scale-object
         (parse-s-v-g resource (create))
         scalar)
        x y z))))

(defun find-s-v-g-asset (xml asset-id)
  (TODO "write me"))

(defun load-s-v-g (url on-load &optional (asset ""))
  (if-let ((cached (getprop *svg-cache* (concat url "#" asset))))
    cached
    (unless (@ window *x-m-l-http-request)
      (error 'xml-http-request-needed))
    (let ((request (new *x-m-l-http-request)))
      ((@ request open) "GET" url t)
      (setf (@ request onreadystatechange)
            (lambda (event)
              (cond 
                ((= +response-ready+ (@ request ready-state))
                 (let ((status (@ request status)))
                   (cond
                     ((and (<= +http-ok+ status)
                           (<= status (+ +http-ok+ 99)))
                      (if (= "" asset)
                          (on-load (@ request response))
                          (on-load (find-s-v-g-asset (@ request response) asset))))
                     (t (error 'xml-http-request-failed status url request)))))
                ;; otherwise, ignore
                (t nil))))
      ((@ request send)))))

