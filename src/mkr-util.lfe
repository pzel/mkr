(defmodule mkr-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'mkr))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(mkr ,(get-version)))))
