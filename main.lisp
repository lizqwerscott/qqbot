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
                 (format t "args:~A~%" args)
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
                           (handler-case
                               (send-text target (handle-code code))
                             (error (c)
                               (send-text target c))))
                         (send-text target "你没有权限!"))))))

(add-command "重启"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list #'numberp))
                       (let ((num (parse-integer (car args))))
                         (if (and (< num 60)
                                  (>= num 1))
                             (progn
                               (sleep num))
                             (sleep 3))
                         (run-shell "reboot"))
                       (progn
                         (sleep 3)
                         (run-shell "reboot")))))
             "参数1 几秒后重启 默认3秒")

(add-task #'(lambda ()
              (send-text (get-master)
                         "主人晚安")
              (send-text 769649079
                         "大家晚安哟!!!")
              (send-text 769649079
                         (wanan)))
          "goodnight"
          (list 23 30))

(start-task "goodnight")

(add-start-task #'(lambda ()
                    (send-text (get-master) "伊蕾娜起床了!!!"))
                "hello")

(defun goodmorning ()
  (send-text 769649079
             "大家早安, 伊蕾娜爱大家哟!!!")
  (send-text 769649079
             (zaoan))
  (send-at-text 769649079
                1072881846
                "主人!!!")
  (send-text 769649079
             (get-random-text "love"))
  (history-today-s 769649079
                   :number 3
                   :page 1))

(add-task #'goodmorning
          "goodmorning"
          (list 7 0 :step-hour 3))

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
      (when (and (apply #'time-in (get-time-range 0 0))
                 (not (is-reset)))
        (format t "reset all task run~%")
        (reset-task-time))
      (dolist (task (run-tasks))
        (when (and (not (task-runp task))
                   (apply #'time-in
                          (apply #'get-time-range
                                 (task-time task))))
          (submit-job *patron*
                      (make-instance 'patron:job
                                     :function (task-func task)))
          (setf (task-runp task) t)))
      (let ((message (car (last (fetch-last-message)))))
        (if message
            (let ((type (assoc-value message "type")))
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
