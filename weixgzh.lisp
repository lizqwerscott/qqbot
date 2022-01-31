(in-package :qqbot.weixgzh)

(defun load-cookies ()
  (with-open-file (in (generate-path "data/cookies.txt"))
    (when in
      (read-line in))))

(defvar *cookies* (load-cookies))

(defvar *user-agent* "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.81 Safari/537.36")

(defvar *token* 392955827)

;;save in data/gzhs.json
(defvar *gzhs* (make-hash-table :test #'equal))

(defun generate-data (fakeid)
  `(("token" . ,*token*)
    ("action" . "list_ex")
    ("begin" . 0)
    ("count" . 5)
    ("fakeid" . ,fakeid)
    ("type" . 9)
    ("query" . "")
    ("lang" . "zh_CN")
    ("ajax" . "1")))

(defun generate-search (query)
  `(("action" . "search_biz")
    ("begin" . 0)
    ("count" . 5)
    ("query" . ,query)
    ("token" . ,*token*)
    ("lang" . "zh_CN")
    ("f" . "json")
    ("ajax" . 1)))

(defun weix-get (command args)
  (parse (octets-to-string (http-request (generate-url "mp.weixin.qq.com"
                                                       (format nil "cgi-bin/~A" command)
                                                       args)
                                         :method :get
                                         :additional-headers `(("cookie:" . ,*cookies*))
                                         :user-agent *user-agent*))))

(defun handle-search (message)
  (if (= 0 (assoc-value (assoc-value message "base_resp") "ret"))
      (mapcar (lambda (item)
                (format t "nickName:~A~%" (assoc-value item "nickname"))
                (format t "fakeid:~A~%" (assoc-value item "fakeid"))
                (setf (gethash (assoc-value item "nickname") *gzhs*)
                      (assoc-value item "fakeid"))
                (list (assoc-value item "nickname")
                      (assoc-value item "fakeid")))
              (assoc-value message "list"))
      (format t "error:~A~%" (assoc-value (assoc-value message "base_resp") "err_msg"))))

(defun search-gzh (query)
  (handle-search (weix-get "searchbiz"
                           (generate-search (purl:url-encode query)))))

(defun handle-page (message)
  (if (= 0 (assoc-value (assoc-value message "base_resp") "ret"))
        (mapcar (lambda (article)
                  (format t "title:~A~%" (assoc-value article "title"))
                  (list (assoc-value article "title")
                        (assoc-value article "link")
                        (assoc-value article "update_time")))
                (assoc-value message "app_msg_list"))
        (format t "error:~A~%" (assoc-value (assoc-value message "base_resp") "err_msg"))))

(defun get-page (fakeid)
  (handle-page (weix-get "appmsg"
                         (generate-data fakeid))))

(defun load-last-time ()
  (with-open-file (in (generate-path "data/ciy.txt")
                      :direction :input
                      :if-does-not-exist :error)
    (read in)))

(defun save-last-time (time)
  (with-open-file (out (generate-path "data/ciy.txt")
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (print time out)))

(defun weix-ciy-check ()
  (format t "Get 次元备份库的文章~%")
  (let ((target 769649079)
        (last-time (load-last-time))
        (articles (get-page "MzkwMTI1MTg2Nw==")))
    (when (> (third (car articles)) last-time)
      (send-text target "次元备份库更新了")
      (dolist (article (remove-if-not #'(lambda (article)
                                         (> (third article) last-time))
                                      articles))
        (send-text-lst target
                       (list (format nil "Title:~A" (first article))
                                   (format nil "Link:~A" (second article)))))
      (save-last-time (third (car articles))))))

(add-task #'weix-ciy-check
          "weix"
          (list 10 1))

(start-task "weix")

(add-command "公众号搜索"
             #'(lambda (sender args)
                 (if (args-type args (list #'symbolp))
                     (let ((target (target-id sender))
                           (query (car args)))
                       (if (gethash query *gzhs*)
                           (send-text target (gethash query *gzhs*))
                           (let ((gzhs (search-gzh query)))
                             (if gzhs
                                 (send-text-lst target
                                                (mapcar (lambda (item)
                                                          (car item))
                                                        gzhs))
                                 (send-text target "没查询到公众号哟!")))))
                     (send-text (target-id sender) "请给一个公众号的名字哟!"))))

(add-command "公众号文章"
             #'(lambda (sender args)
                 (if (args-type args (list 'symbolp 'numberp))
                     (let ((gzh (gethash (car args) *gzhs*))
                           (target (target-id sender)))
                       (when (not gzh)
                         (search-gzh (car args))
                         (sleep 4))
                       (let ((articles (get-page gzh)))
                         (if (and (<= (parse-integer (second args)) 13) articles)
                             (dolist (item (subseq articles 0 (parse-integer (second args))))
                               (send-text-lst target (list (format nil "Title:~A" (first item))
                                                           (format nil "Link:~A" (second item))))
                               (sleep 1))
                             (send-text target "没有找到这个公众号或者文章显示个数过多."))))
                     (send-text (target-id sender) "用法:伊蕾娜 公众号文章 共青团中央 3, 最大的最近文章个数是13"))))

(in-package :cl-user)
