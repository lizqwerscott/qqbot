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
                     (if (or (is-master (sender-id sender))
                             (is-admin (sender-id sender)))
                         (progn
                           (format t "handle-code-right:~A~%" code)
                           (send-message-text target (handle-code code)))
                         (send-message-text target "你没有权限!"))))))

(defun start ()
  (let ((runp t))
    (do ()
        ((not runp) 'done)
      (setf runp nil)
      (format t "stay connect~%")
      (handler-case
          (verify)
        (error (c)
          (format t "error:~A~%" c)
          (setf runp t)))
      (sleep 10)))
  (format t "connect finish~%")
  (bind)
  (get-group-list)
  (send-message-text (get-master) "机器人启动完成")
  (bt:make-thread #'run))

(in-package :cl-user)
