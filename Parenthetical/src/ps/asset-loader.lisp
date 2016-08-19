;;; -*- lisp -*-

(defvar *asset-prefix* "./")

((@ console log) (concat "Asset Prefix: " *asset-prefix*))

(setf (@ document loaders) (create))

(defun update-loading ()
  (let ((loaded 0)
        (count 0)
        (loader-bar (by-id "loader-bar")))
    (for-in (key (@ document loaders))
            (incf loaded (getprop (@ document loaders) key))
            (incf count))
    (if (= 0 count)
        (progn (setf (@ loader-bar style display) :none)
               ((@ console log) "Done loading everything"))
        (progn
          (setf (@ loader-bar style display) :block)
          (let ((percent ((@ *math round) (* 100 (/ loaded count)))))
            ((@ loader-bar set-attribute) 'loader-value percent)
            ((@ console log) (concat "Loading " count " assets, overall status "
                                     percent "%")))))))

(defun start-loading (key)
  (setf (getprop (@ document 'loaders) key) 0)
  ((@ console log) (concat "Started loading asset: " key))
  (update-loading))

(defun done-loading (key)
  (delete (getprop (@ document loaders) key))
  ((@ console log) (concat "Done loading asset: " key))
  (update-loading))



