(in-package :qqbot.bt)

(defun getBt (str)
  (handler-case (http-request (format nil "https://www.busdmm.xyz/search/~A&type=&parent=ce"
                                             (purl:url-encode str))
                                     :method :get
                                     :user-agent :explorer)
    (error (e)
      (format t "error:~A~%" e)
      nil)))

(defun save (bytes)
  (when bytes
    (with-open-file (in (merge-pathnames "web/search.html" (get-source-dir)) :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
     (when in
       (write-sequence bytes in)))))

(defun find-movie-url (doc)
  (let ((movie-urls (lquery:$ doc ".movie-box" (attr :href))))
      movie-urls))

(defun searchBt (str)
  (let ((result (getBt str)))
    (when result
      (save result)
      (find-movie-url (lquery:$ (initialize (merge-pathnames "web/search.html" (get-source-dir))))))))

(add-command "磁力"
             #'(lambda (sender args)
                 (if (args-type args (list #'symbolp))
                     (let ((target (target-id sender)))
                       (send-message-text target "磁力搜索中...")
                       (let ((urls (searchBt (car args))))
                         (if urls
                             (if (> (length urls) 3)
                                 (progn
                                   (send-message-text target "搜索结果大于三条, 发给个人.")
                                   (dotimes (i (length urls))
                                     (sleep 0.5)
                                     (send-message-text (sender-id sender) (elt urls i))))
                                 (send-message-text target (lst-line-string urls)))
                             (send-message-text target "磁力搜索失败"))))
                     (send-message-text (target-id sender) "参数错误")))
             "后面跟一个参数, 为你要搜索的种子名称")

(in-package :cl-user)
