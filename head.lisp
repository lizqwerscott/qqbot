(in-package :qqbot.head)

(defvar *patron* (make-instance 'patron:patron
                                :worker-capacity 3
                                :job-capacity 32
                                :worker-timeout-duration 600))

(setf yason:*parse-object-as* :alist)

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defun last1 (lst)
  (car (last lst)))

(defun random-int-r (max)
  (let ((generator (random-state:make-generator :mersenne-twister-32 (ccl:current-time-in-nanoseconds))))
    (random-state:random-int generator 0 max)))

(defun assoc-value (plist key)
  (cdr (assoc key plist :test #'string=)))

(defun assoc-v (plist key)
  (cdr (assoc key plist)))

(defun split-s (str &optional (deleimiter " "))
  (split deleimiter str))

(defun string-merge (str1 str2 delimiter)
  (if (or (equal str1 "") (equal str2 ""))
      (format nil "~A~A" str1 str2)
      (format nil "~A~A~A" str1 delimiter str2)))

(defun string-merges (lst &optional (deleimiter ""))
  (join deleimiter lst))

(defun bits-to-json (bits)
  (parse (babel:octets-to-string bits)))

(defun list-directory (dir)
  (directory (make-pathname :name :wild :type :wild :defaults dir)))

(defun get-directory (file)
  (make-pathname :directory (pathname-directory file)))

(defun directoryp (dir) 
  (equal dir (get-directory dir)))

(defun make-next-dir (dir-lst path)
  "get the path/dir/"
  (when (directoryp path)
    (merge-pathnames (make-pathname :directory (append (list :relative)
                                                       (if (stringp dir-lst)
                                                           (list dir-lst)
                                                           dir-lst)))
                   path)))

(defun run-shell (program)
  #+clozure (ccl:run-program "/bin/sh" (list "-c" (format nil "~A" program))))

(defun load-line-file (path)
  (let ((result))
    (with-open-file (in path :direction :input
                             :if-does-not-exist :error)
      (do ((line (read-line in nil 'eof)
                 (read-line in nil 'eof)))
          ((eql line 'eof) result)
        (setf result (append result (list line)))))))

(defun load-json-file (path)
  (with-open-file (in path :direction :input :if-does-not-exist :error)
    (multiple-value-bind (s) (make-string (file-length in))
      (read-sequence s in)
      (parse s))))

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

(defun generate-path (path)
  (merge-pathnames path (get-source-dir)))

(defun save-l-picture (bits path)
  (with-open-file (out path
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (when out
      (write-sequence bits out)))
  path)

(defun save-picture-url (url path &optional (name nil))
  (if name
      (run-shell (format nil "wget ~A -O ~A~A/~A" url (get-source-dir) path name))
      (run-shell (format nil "wget -P ~A~A/ ~A" (get-source-dir) path url))))

(defun to-json-a (alist)
  (to-json alist :from :alist))

(defun to-json-ss (alist)
  (let ((out (make-string-output-stream)))
    (encode-alist alist out)
    (get-output-stream-string out)))

(in-package :cl-user)

