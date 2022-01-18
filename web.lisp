(in-package :qqbot.web)

(defun generate-url (host command &optional args)
  (let ((url (format nil "http://~A/~A?" host command))
        (str-args ""))
    (dolist (i args)
      (setf str-args (string-merge str-args (string-merge (car i) (cdr i) "=") "&")))
    (format nil "~A~A" url str-args)))

(defun web-post (host command &key args (jsonp t) (parse-method t))
  (multiple-value-bind (bytes code headers)
      (http-request (generate-url host command)
                    :method :post
                    :content (string-to-octets (to-json args :from :alist)))
    (declare (ignorable code))
    (let ((content-type (cdr (assoc :content-type headers))))
      (if (and jsonp
               (str:starts-with-p "application/json" content-type))
          (if parse-method
              (parse bytes :as :alist)
              (decode-json-from-string bytes))
          bytes))))

(defun web-post-upload (host command file &key (jsonp nil))
  (let ((text (http-request (generate-url host command)
                            :method :post
                            :content-type "multipart/form-data"
                            :parameters `(("image" . ,(pathname file)))
                            :form-data t)))
    (if jsonp
        (parse text :as :alist)
        text)))

(defun web-get (host command &key args (jsonp nil) (parse-method t))
  (let ((text (http-request (generate-url host command args)
                            :method :get)))
    (if jsonp
        (if parse-method
            (parse text :as :alist)
            (decode-json-from-string text))
        text)))


(in-package :cl-user)

