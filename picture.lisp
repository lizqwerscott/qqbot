(in-package :qqbot.picture)

(defun get-random-picture ()
  (let ((picture (web-get "api.vvhan.com" "api/acgimg"
                          :args '(("type" . "json")) :jsonp t)))
    (when (assoc-value picture "success")
      (assoc-value picture "imgurl"))))

(defun save-picture (url)
  (run-shell (format nil "wget -P ~Apicture/ ~A" (get-source-dir) url)))

;;tag is list
(defun get-pixiv-picture (tag)
  (handler-case
      (let ((picture (web-post "api.lolicon.app"
                               "setu/v2"
                               :args `(("r18" . 1)
                                       ("num" . 1)
                                       ("size" . "original")
                                       ("proxy" . nil)
                                       ("tag" . ,tag))
                               :jsonp t)))
    (format t "~A~%" picture)
    (if (string= "" (assoc-value picture "error"))
        (list (assoc-value (car (assoc-value picture "data")) "pid")
              (assoc-value (assoc-value (car (assoc-value picture "data")) "urls") "original"))
        (format t "error~%")))
    (error (c)
      (format t "error:~A~%" c)
      (values nil c))))

(defun get-pixiv-pictures (num tag)
  (let ((pictures nil))
    (dotimes (i num)
      (sleep 0.5)
      (let ((picture (get-pixiv-picture tag)))
        (if picture
            (setf pictures (append pictures (list picture))))))
    pictures))

(add-command "二次元图片"
             #'(lambda (sender args)
                 (let ((url (get-random-picture)))
                   (save-picture url)
                   (send-picture (target-id sender)
                                 url)))
             "获取随机一张二次元图片")

(add-command "色图"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list #'numberp #'symbolp))
                       (if (<= (parse-integer (car args)) 10)
                           (progn
                             (send-text target "p站搜索中。。。")
                             (let ((pictures (get-pixiv-pictures (parse-integer (car args)) (cdr args))))
                               (if pictures
                                   (dolist (picture pictures)
                                     (format t "~A~%" picture)
                                     (sleep 0.5)
                                     (send-message target
                                                   `(,(gmessage-text (format nil "pid:~A" (car picture)))
                                                     ,(gmessage-text (format nil "url:~A" (second picture)))
                                                     ,(gmessage-picture (second picture)))))
                                   (send-text target "震惊,居然一张都没有找到!!!!"))))
                           (send-text target "请求的太多了。。。请少点啦!!!!"))
                       (send-text target "参数错误"))))
             "从p站获取色图(大部分为萝莉),第一个参数为要几张色图, 后面参数为要找的图片类型")

(in-package :cl-user)
