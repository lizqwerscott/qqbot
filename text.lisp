(in-package :qqbot.text)

(defun get-random-text (command)
  (web-get "api.vvhan.com" (format nil "api/~A" command)))

(add-command "笑话"
             #'(lambda (sender args)
                 (send-message-text (target-id sender)
                                    (get-random-text "xh")))
             "获取随机一条笑话")

(add-command "骚话"
             #'(lambda (sender args)
                 (send-message-text (target-id sender)
                                    (get-random-text "sao")))
             "获取随机一条骚话")

(add-command "情话"
             #'(lambda (sender args)
                 (send-message-text (target-id sender)
                                    (get-random-text "love")))
             "获取随机一条情话")

(in-package :cl-user)
