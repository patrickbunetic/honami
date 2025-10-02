(defpackage :honami.utils
  (:use :cl  :cl-ppcre)
  (:export :sanitize-css))
(in-package :honami.utils)

(defun sanitize-css (css)
  "Remove potentially dangerous CSS properties and values."
  (let ((allowed-properties '("color" "background" "background-color" "border" "border-radius" "margin"
			      "padding" "font" "font-size" "font-family" "width" "height" "display"
			      "position" "top" "left" "right" "bottom" "float" "clear" "text-align"
			      "line-height" "box-shadow" "opacity" "cursor")))
    (with-output-to-string (s)
      (loop for line in (split-sequence #\Newline css)
            do (ppcre:do-matches (start end "(\\w+[-\\w]*)\\s*:\\s*([^;]+)" line)
                 (let ((property (string-trim " " (subseq line start (position #\: line :start start))))
                       (value (string-trim " " (subseq line (1+ (position #\: line :start start)) end))))
                   (when (and (member property allowed-properties :test #'string=)
                              (not (ppcre:scan "(?i:url\\(|behavior|expression)" value)))
                     (format s "~a: ~a;~%" property value))))))))
