(in-package :qqbot.question)

(defun search-get (url)
  (decode-json-from-string (http-request url :method :get)))

(defun search-ti (keyword)
  (let ((json (search-get (format nil
                               "http://tool.chaoxing.zmorg.cn/api/search.php?q=~A"
                               (url-encode keyword)))))
    (format t "ti:result:~A" json)
    (when (= 1 (cdr (assoc :code json)))
      (cdr (assoc :msg json)))))

(add-command "查题"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list #'symbolp))
                       (let ((q (search-ti (car args))))
                         (send-text target (if q
                                               q
                                               "报错")))
                       (send-text target "查的题为空")))))

(in-package :cl-user)
