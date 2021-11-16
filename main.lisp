(in-package :qqbot)

(add-command "添加管理"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (changep (sender-id sender))
                     (progn
                       (add-admin (car args))
                       (send-text target "管理员添加成功")
                       (save-admin))
                     (send-text target "只有主人和管理员才可以添加哟!")))))

(add-command "删除管理"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (changep (sender-id sender))
                       (progn
                         (if (is-admin (car args))
                             (progn
                               (remove-admin (car args))
                               (send-text target "管理员删除成功")
                               (save-admin))
                             (send-text target "没有这个管理员哟!")))
                       (send-text target "只有主人和管理员才可以删除哟!")))))

(add-command "管理员列表"
             #'(lambda (sender args)
                 (let ((target (target-id sender))
                       (lst (list-admin)))
                   (if (= 0 (length lst))
                       (send-text target "没有管理员")
                       (dotimes (i (length lst))
                         (send-text target (elt lst i)))))))

(add-command "问好"
	     #'(lambda (sender args)
		 (let ((chenhu (if (is-master (sender-id sender)) "主人"
                                   (if (is-admin (sender-id sender)) "管理员"
                                       "陌生人"))))
		   (send-text (target-id sender)
                                      (format nil "你好，~A，我是伊蕾娜。" chenhu)))))

(add-command "run"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (let ((code (string-merges args " ")))
                     (if (or (is-master (sender-id sender))
                             (is-admin (sender-id sender)))
                         (progn
                           (format t "handle-code-right:~A~%" code)
                           (send-text target (handle-code code)))
                         (send-text target "你没有权限!"))))))

(add-task #'(lambda ()
              (send-text (get-master)
                         "主人晚安")
              (send-text 769649079
                         "大家晚安哟!!!"))
          "goodnight"
          (get-time-range 23 30))

(start-task "goodnight")

(add-start-task #'(lambda ()
                    (send-text (get-master) "伊蕾娜起床了!!!"))
                "hello")

(defun goodmorning ()
  (send-text 769649079
             "大家早安, 伊蕾娜爱大家哟!!!")
  (send-at-text 769649079
                1072881846
                "主人!!!")
  (send-text 769649079
             (get-random-text "love")))

(add-task #'goodmorning
          "goodmorning"
          (get-time-range 7 0 :step-hour 3))

(start-task "goodmorning")

(defun run ()
  (format t "Start patron...~%")
  (start-patron *patron*)
  (format t "Start qqbot event~%")
  (dolist (task (run-start-tasks))
    (submit-job *patron*
                (make-instance 'patron:job
                               :function #'(lambda ()
                                             (sleep (first task))
                                             (funcall (second task))))))
  (format t "finish qqbot start event~%")
  (with-event-loop ()
    (with-interval (1)
      (dolist (task (run-tasks))
        (when (and (not (task-runp task))
                   (apply #'time-in (task-time task)))
          (submit-job *patron*
                      (make-instance 'patron:job
                                     :function (task-func task)))
          (setf (task-runp task) t)))
      (let ((message (car (last (fetch-last-message)))))
        (if message
            (let ((type (assoc-value message "type")))
              (format t "message:~A~%" message)
              (format t "type:~A~%" type)
              (when (or (string= "FriendMessage" type) (string= "GroupMessage" type))
                ;;(format t "message:~A~%" message)
                (handle-message message)))))))
  (format t "Stop patron...~%")
  (stop-patron *patron* :wait t))


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
      (sleep 3)))
  (format t "connect finish~%")
  (bind)
  (get-group-list)
  (make-thread #'run))

(in-package :cl-user)
