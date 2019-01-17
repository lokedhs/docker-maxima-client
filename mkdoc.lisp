(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "infoparser"))

(defun generate-docs ()
  (handler-case
      (infoparser:generate-doc-directory)
    (error (condition)
      (format t "Error when generating documentation: ~a" condition)
      (uiop:quit 1))))

(generate-docs)
