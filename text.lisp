(in-package :qqbot.text)

(defun get-random-text (command)
  (web-get "api.vvhan.com" (format nil "api/~A" command)))

(defun bullet-fly-load ()
  (load-line-file (merge-pathnames "data/bullet.txt"
                                   (get-source-dir))))

(add-command "笑话"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                                    (get-random-text "xh")))
             "获取随机一条笑话")

(add-command "骚话"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                                    (get-random-text "sao")))
             "获取随机一条骚话")

(add-command "情话"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                                    (get-random-text "love")))
             "获取随机一条情话")

(add-command "让子弹飞一会"
             #'(lambda (sender args)
                 (dolist (line (bullet-fly-load))
                   (sleep 1)
                   (send-text (target-id sender) line))))

(in-package :cl-user)
