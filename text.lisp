(in-package :qqbot.text)

(defun get-random-text (command)
  (web-get "api.vvhan.com" (format nil "api/~A" command)))

(defun bullet-fly-load ()
  (let ((result nil))
    (with-open-file (in (merge-pathnames "data/bullet.txt"
                                         (get-source-dir)))
      (when in
        (do ((line (read-line in nil 'eof)
                   (read-line in nil 'eof)))
            ((eql line 'eof))
          (setf result (append result (list line))))))
    result))

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

(add-command "让子弹飞一会"
             #'(lambda (sender args)
                 (dolist (line (bullet-fly-load))
                   (sleep 1)
                   (send-message-text (target-id sender) line))))

(in-package :cl-user)
