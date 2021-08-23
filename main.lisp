(in-package :qqbot)

(add-command "添加管理"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (changep (sender-id sender))
                     (progn
                       (add-admin (car args))
                       (send-message-text target "管理员添加成功")
                       (save-admin))
                     (send-message-text target "只有主人和管理员才可以添加哟!")))))

(add-command "删除管理"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (changep (sender-id sender))
                       (progn
                         (if (is-admin (car args))
                             (progn
                               (remove-admin (car args))
                               (send-message-text target "管理员删除成功")
                               (save-admin))
                             (send-message-text target "没有这个管理员哟!")))
                       (send-message-text target "只有主人和管理员才可以删除哟!")))))

(add-command "管理员列表"
             #'(lambda (sender args)
                 (let ((target (target-id sender))
                       (lst (list-admin)))
                   (if (= 0 (length lst))
                       (send-message-text target "没有管理员")
                       (dotimes (i (length lst))
                         (send-message-text target (elt lst i)))))))

(add-command "问好"
	     #'(lambda (sender args)
		 (let ((chenhu (if (is-master (sender-id sender)) "主人"
                                   (if (is-admin (sender-id sender)) "管理员"
                                       "陌生人"))))
		   (send-message-text (target-id sender)
                                      (format nil "你好，~A，我是陈睿机器人。" chenhu)))))

(add-command "run"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (let ((code (string-merges args " ")))
                     (format t "handle-code-right:~A~%" code)
                     (send-message-text target (handle-code code))))))

(defun start ()
  (verify)
  (bind)
  (get-group-list)
  (bt:make-thread #'run))

(in-package :cl-user)
