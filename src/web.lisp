(defpackage :qqbot.web
  (:import-from :quri :make-uri)
  (:use :common-lisp :qqbot.head :babel :yason)
  (:export
   :generate-url
   :web-post-json
   :web-post
   :web-post-upload

   :web-get
   :web-get-url))
(in-package :qqbot.web)

(defun generate-url (host command &key (args nil) (ssl nil))
  (let ((url (format nil "~A/~A" host command))
        (str-args ""))
    (if (> (length args) 0)
        (setf url
              (format nil "~A?" url)))
    (if ssl
        (setf url
              (format nil "https://~A" url))
        (setf url
              (format nil "http://~A" url)))
    (dolist (i args)
      (setf str-args (string-merge str-args (string-merge (car i) (cdr i) "=") "&")))
    (format nil "~A~A" url str-args)))

(defun web-post-json (host command &key args (jsonp t) (isbyte t))
  (multiple-value-bind (body status respone-headers uri stream)
      (dex:post (generate-url host command)
                :content (if isbyte
                             (string-to-octets (to-json-a args))
                             (to-json-a args))
                :headers '(("content-type" . "application/json; charset=utf-8")))
    (declare (ignorable status uri stream))
    (if (and jsonp
             (str:starts-with-p "application/json"
                                (gethash "content-type"
                                         respone-headers)))
        (parse body)
        body)))

(defun web-post (url &key args (jsonp t))
  (multiple-value-bind (body status respone-headers uri stream)
      (dex:post url
                :content args)
    (declare (ignorable status uri stream))
    (if (and jsonp
             (str:starts-with-p "application/json"
                                (gethash "content-type"
                                         respone-headers)))
        (parse body)
        body)))

(defun web-post-upload (url file &key (jsonp nil))
  (let ((text (dex:post url
                        :content `(("image" . ,(pathname file))))))
    (if jsonp
        (parse text)
        text)))

(defun make-url (host command args)
  (make-uri :defaults (generate-url host command)
            :query args))

(defun web-get (host command &key args (jsonp nil))
  (let ((text (dex:get (make-url host command args))))
    (if jsonp
        (parse text)
        text)))

(defun web-get-url (url &key (jsonp nil))
  (let ((text (dex:get url)))
    (if jsonp
        (parse text)
        text)))

(in-package :cl-user)

