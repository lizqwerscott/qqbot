(in-package :qqbot.minecraft)

(defvar *key* "bc361c81d384481790c4e7fd229e3022")
(defvar *address* "124.222.100.66:23333")

(defparameter *instances* nil)

(defun handle-data (data)
  (if (= 200 (assoc-value data "status"))
      (assoc-value data "data")
      (progn
        (format t "[Error]:~A~%" data)
        nil)))

(defun manager-status ()
  (handle-data
   (web-get *address*
            "api/overview"
            :args `(("apikey" . ,*key*))
            :jsonp t)))

(defun manager-status-simplified ()
  (handle-data
   (web-get *address*
            "api/service/remote_services_system"
            :args `(("apikey" . ,*key*))
            :jsonp t)))

(defun kb-to-G (bytes)
  (float (/ bytes (expt 1024 3))))

(defun handle-ms-s (data)
  (let ((system (assoc-value data "system"))
        (instance (assoc-value data "instance")))
    (list (format nil
                  "cpu使用:~,2f%"
                  (* 100 (assoc-value system "cpuUsage")))
          (let ((totalmem (assoc-value system "totalmem"))
                (freemem (assoc-value system "freemem")))
            (format nil
                    "内存使用:~,2f%(~,2fG/~,2fG)"
                    (* 100 (assoc-value system "memUsage"))
                    (kb-to-g (- totalmem freemem))
                    (kb-to-g totalmem)))
          (format nil
                  "实例运行数:~A/~A"
                  (assoc-value instance "running")
                  (assoc-value instance "total")))))

(defun service-info ()
  (handle-data
   (web-get *address*
            "api/service/remote_services"
            :args `(("apikey" . ,*key*))
            :jsonp t)))

(defun handle-service-info (data)
  (mapcar #'(lambda (service)
              (format t "[uuid]:~A~%" (assoc-value service "uuid"))
              (format t
                      "[address]:~A:~A~%"
                      (assoc-value service "ip")
                      (assoc-value service "port"))
              (mapcar #'(lambda (instance)
                          (list (assoc-value
                                 (assoc-value instance
                                              "config")
                                 "nickname")
                                (assoc-value instance "instanceUuid")
                                (assoc-value service "uuid")))
                      (assoc-value service "instances")))
          data))

(defun instance-info (uuid remote-uuid)
  (handle-data
   (web-get *address*
            "api/instance"
            :args `(("apikey" . ,*key*)
                    ("uuid" . ,uuid)
                    ("remote_uuid" . ,remote-uuid))
            :jsonp t)))

(defun all-instance-info ()
  (mapcar #'(lambda (instance)
              (format t "name:~A~%" (car instance))
              (apply #'instance-info
                     (cdr instance)))
          (car (handle-service-info (service-info)))))

(defun save-instance-data ()
  (setf *instances*
        (car (handle-service-info (service-info)))))

(save-instance-data)

(defun instance-command (uuid remote-uuid command)
  (handle-data
   (web-get *address*
            (format nil "api/protected_instance/~A" command)
            :args `(("apikey" . ,*key*)
                    ("uuid" . ,uuid)
                    ("remote_uuid" . ,remote-uuid))
            :jsonp t)))

(defun append1 (lst1 item)
  (append lst1 (list item)))

(defun instance-start (index)
  (let ((instance (elt *instances* index)))
    (apply #'instance-command
           (append1 (cdr instance)
                    "open"))))

(defun instance-stop (index)
  (let ((instance (elt *instances* index)))
    (apply #'instance-command
           (append1 (cdr instance)
                    "stop"))))

(defun instance-kill (index)
  (let ((instance (elt *instances* index)))
    (apply #'instance-command
           (append1 (cdr instance)
                    "kill"))))

(defun instance-restart (index)
  (let ((instance (elt *instances* index)))
    (apply #'instance-command
           (append1 (cdr instance)
                    "restart"))))

(defun i-info (&optional (index 0))
  (when-bind (info (apply #'instance-info
                          (cdr (elt *instances* index))))
    (list (assoc-value info "status")
          (assoc-value (assoc-value info
                                    "config")
                       "nickname")
          (assoc-value info
                       "info"))))
;;;0 stop
;;;1 started
;;;2 running
(defun instance-status (status info)
  (if (= status 0)
      0
      (if (= status 3)
          (if (and (not
                    (stringp (assoc-value info
                                          "maxPlayers")))
                   (= (assoc-value info
                                   "maxPlayers")
                      -1)
                   (= (assoc-value info
                                   "currentPlayers")
                      -1))
              1
              2)
          status)))

(defun instance-send-command (uuid remote-uuid command)
  (handle-data
   (web-get *address*
            "api/protected_instance/command"
            :args `(("apikey" . ,*key*)
                    ("uuid" . ,uuid)
                    ("remote_uuid" . ,remote-uuid)
                    ("command" . ,command))
            :jsonp t)))

(add-command "mc状态"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (dotimes (i (length *instances*))
                   (let ((data (i-info i))
                       (target (target-id sender)))
                   (send-text target
                              (format nil "(~A)服务器名:~A" i (elt data 1)))
                   (sleep 0.5)
                   (case (instance-status (first data)
                                          (last1 data))
                     ((0)
                      (send-text target
                                 "服务器处于关闭状态"))
                     ((1)
                      (send-text target
                                 "服务器处于启动状态"))
                     ((2)
                      (send-text-lst target
                                     (list "服务器处于运行状态"
                                           (format nil
                                                   "players:~A/~A"
                                                   (assoc-value (last1 data)
                                                                "currentPlayers")
                                                   (assoc-value (last1 data)
                                                                "maxPlayers")))))
                     (t
                      (send-text target
                                 (format nil
                                         "服务器状态异常:~A~%"
                                         (first data)))))))))

(defun get-index (args)
  (let ((index (car args)))
    (if index
        (parse-integer index)
        0)))

(add-command "mc开启"
             #'(lambda (sender args)
                 (let ((index (get-index args))
                       (target (target-id sender)))
                   (let ((result (instance-start index)))
                     (if result
                         (send-text target
                                    "开启成功")
                         (send-text target
                                    "失败"))))))

(add-command "mc关闭"
             #'(lambda (sender args)
                 (let ((index (get-index args))
                       (target (target-id sender)))
                   (let ((result (instance-stop index)))
                     (if result
                         (send-text target
                                    "关闭成功")
                         (send-text target
                                    "失败"))))))

(add-command "mc重启"
             #'(lambda (sender args)
                 (let ((index (get-index args))
                       (target (target-id sender)))
                   (let ((result (instance-restart index)))
                     (if result
                         (send-text target
                                    "重启成功")
                         (send-text target
                                    "重启失败"))))))

(add-command "服务器数据"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (send-text-lst (target-id sender)
                                (handle-ms-s (car (manager-status-simplified))))))

(add-command "mc指令"
             #'(lambda (sender args)
                 (if (args-type args (list #'symbolp))
                     (let ((command (car args))
                           (instance (elt *instances* 0)))
                       (let ((result (apply #'instance-send-command
                                            (append1 (cdr instance)
                                                     command))))
                         (if result
                             (send-text (target-id sender)
                                        "发送成功，具体指令效果请登陆服务器上查看")
                             (send-text (target-id sender)
                                        "发送失败, 是否服务器关闭的了？"))))
                     (send-text (target-id sender)
                                "参数错误， 参数为发送到服务器的命令"))))

(in-package :cl-user)
