(in-package :qqbot.head)

(defun random-int-r (max)
  (let ((generator (random-state:make-generator :mersenne-twister-32 (ccl:current-time-in-nanoseconds))))
    (random-state:random-int generator 0 max)))

(defun assoc-value (plist key)
  (cdr (assoc key plist :test #'string=)))

(defun split-s (str &optional (delimiter " "))
  (if (= 0 (length str)) nil
      (cl-strings:split str delimiter)))

(defun string-merge (str1 str2 delimiter)
  (if (or (equal str1 "") (equal str2 ""))
      (format nil "~A~A" str1 str2)
      (format nil "~A~A~A" str1 delimiter str2)))

(defun string-merges (lst &optional (deleimiter "") (result ""))
  (if lst
      (string-merges (cdr lst) deleimiter (string-merge result (car lst) deleimiter))
      result))

(defun bits-to-json (bits)
  (jonathan:parse (babel:octets-to-string bits) :as :alist))

(defun list-directory (dir)
  (directory (make-pathname :name :wild :type :wild :defaults dir)))

(defun run-shell (program)
  #+clozure (ccl:run-program "/bin/sh" (list "-c" (format nil "~A" program))))

(defun load-json-file (path)
  (with-open-file (in path :direction :input :if-does-not-exist :error)
    (multiple-value-bind (s) (make-string (file-length in))
      (read-sequence s in)
      (jonathan:parse s :as :alist))))

(defun save-json-file (path json)
  (with-open-file (out path
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (write-sequence json out)))

(defun lst-line-string (lst)
  (with-output-to-string (stream)
    (dolist (line lst)
      (write-line line stream))))

(defun get-source-dir ()
  (asdf:system-source-directory :qqbot))

(in-package :cl-user)

