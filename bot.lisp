(in-package :qqbot.bot)

(defvar *session* nil)
(defparameter *message-size* 0)
(defparameter *group-list* (make-array 3 :fill-pointer 0 :adjustable 1))

(defparameter *master* 1963771277)
(defparameter *admin* (make-array 1 :fill-pointer 0 :adjustable 1))

(defparameter *command-map* (make-hash-table :test #'equal))
(defparameter *command-mode-map* (make-hash-table :test #'equal))
(defparameter *command-mode-active-map* (make-hash-table :test #'equal))

(defparameter *is-repeat* t)
(defparameter *repeat-command* nil)

(defstruct people
  id
  name)

(defstruct qq-group
  id
  name)

(defun load-repeat ()
  (setf *repeat-command* (load-json-file (merge-pathnames "data/repeat.json" (get-source-dir)))))

(load-repeat)

(defun save-repeat ()
  (save-json-file (merge-pathnames "data/repeat.json" (get-source-dir))
                  (jonathan:to-json *repeat-command* :from :alist)))

(defun add-repeat (repeat)
  (setf *repeat-command* (append *repeat-command* (list repeat)))
  (save-repeat))

(defun remove-repeat (repeat-f)
  (setf *repeat-command* (remove repeat-f *repeat-command*
                                 :key #'(lambda (x)
                                          (car x))
                                 :test #'string=))
  (save-repeat))

(defun list-repeat ())

(defun active-repeat ()
  (setf *is-repeat* t))

(defun deactive-repeat ()
  (setf *is-repeat* nil))

(defparameter *help-map* (make-hash-table :test #'equal))

(defun get-master ()
  *master*)

(defun is-master (qq)
  (= *master* qq))

(defun add-admin (qq)
  (vector-push-extend qq *admin*))

(defun remove-admin (qq)
  (setf *admin* (delete qq *admin*)))

(defun is-admin (qq)
  (find qq *admin*))

(defun list-admin ()
  *admin*)

(defun sequence-to-json ()
  (let ((lst nil))
    (dotimes (i (length *admin*))
      (setf lst (append lst (list (elt *admin* i)))))
    (jonathan:to-json lst :from :alist)))

(defun save-admin ()
  (save-json-file (merge-pathnames "data/admin.json" (get-source-dir)) (sequence-to-json)))

(defun load-admin ()
  (let ((admin (load-json-file (merge-pathnames "data/admin.json" (get-source-dir)))))
    (mapcar #'(lambda (x)
                (setf *admin* (make-array 3 :fill-pointer 0 :adjustable 1))
                (vector-push-extend x *admin*))
            admin)))

(load-admin)

(defun changep (qq)
  (or (is-admin qq) (is-master qq)))

(defun parse-data (message)
  (if message
      (if (= 0 (assoc-value message "code")) message
          (format t "error: ~A~%" (assoc-value message "msg")))
      (format t "command error~%")))

(defun send-command-post (command args)
  (parse-data (web-post "192.168.3.3:8080" command :args args :jsonp t)))

(defun send-command-get (command args)
  (parse-data (web-get "192.168.3.3:8080" command :args args :jsonp t)))


(defun verify (&optional (key "12138"))
  (let ((message (send-command-post "verify"
                                    `(("verifyKey" . ,key)))))
    (setf *session* (assoc-value message "session"))
    *session*))

(defun bind (&optional (qq-id 3027736450))
  (send-command-post "bind"
                     `(("sessionKey" . ,*session*)
                       ("qq" . ,qq-id))))

(defun release (&optional (qq-id 3027736450))
  (send-command-get "release"
                    `(("sessionKey" . ,*session*)
                      ("qq" . ,qq-id))))

(defun get-message-size ()
  (let ((message (send-command-get "countMessage"
                                   `(("sessionKey" . ,*session*)))))
    (if message
	(setf *message-size* (assoc-value message "data")))))
;;return message list
(defun fetch-last-message ()
  (let ((message (send-command-get "fetchLatestMessage"
                                   `(("sessionKey" . ,*session*)
                                     ("count" . 1)))))
    (if message
	(assoc-value message "data"))))

(defun get-group-list ()
  (let ((message (send-command-get "groupList"
                                   `(("sessionKey" . ,*session*)))))
    (when message
      (setf *group-list* (make-array 3 :fill-pointer 0 :adjustable t))
      (dolist (i (assoc-value message "data"))
        (vector-push-extend (make-qq-group :id (assoc-value i "id")
                                           :name (assoc-value i "name"))
                            *group-list*)))))

(defun gmessage-text (text)
  `(("type" . "Plain")
    ("text" . ,text)))

(defun gmessage-picture (url)
  `(("type" . "Image")
    ("url" . ,url)))

(defun gmessage-music-share (title summary jumpurl pictureurl musicurl brief)
  `(("type" . "MusicShare")
    ("kind" . "NeteaseCloudMusic")
    ("title" . ,title)
    ("summary" . ,summary)
    ("jumpUrl" . ,jumpurl)
    ("pictureUrl" . ,pictureurl)
    ("musicUrl" . ,musicurl)
    ("brief" . ,brief)))

(defun gmessage-json (json)
  `(("type" . "Json")
    ("json" . ,json)))

(defun send-message (target message-chain)
  (let ((command (if (find target *group-list*) "sendGroupMessage" "sendFriendMessage")))
    (send-command-post command
                       `(("sessionKey" . ,*session*)
                         ("target" . ,target)
                         ("messageChain" . ,message-chain)))))

(defun send-text (target str)
  (send-message target `(,(gmessage-text str))))

(defun send-text-lst (target texts)
  (send-message target `(,(gmessage-text (lst-line-string texts)))))

(defun send-picture (target url)
  (send-message target `(,(gmessage-picture url))))

(defun send-music-share (target title summary jumpUrl pictureUrl musicUrl &optional (brief ""))
  (send-message target `(,(gmessage-music-share title summary jumpUrl pictureUrl musicUrl brief))))

(defun send-json (target json)
  (send-message target `(,(gmessage-json json))))

(defun sender-groupp (sender)
  (assoc-value sender "group"))


(defun sender-id (sender)
  (assoc-value sender "id"))

(defun sender-name (sender)
  (if (sender-groupp sender)
      (assoc-value sender "memberName")
      (assoc-value sender "nickName")))

(defun group-id (sender)
  (assoc-value (sender-groupp sender) "id"))

(defun group-name (sender)
  (assoc-value (sender-groupp sender) "name"))

(defun target-id (sender)
  (if (sender-groupp sender)
      (group-id sender)
      (sender-id sender)))

(defun get-text-message-chain (message-chain)
  (let ((text nil))
    (dolist (i message-chain)
          (if (string= "Plain" (assoc-value i "type"))
              (let ((temp (assoc-value i "text")))
                (format t "~A~%" temp)
                (if (stringp temp)
                    (setf text temp)))))
    text))

(defun mute-group-member (group member time)
  (send-command-post "mute"
                     `(("sessionKey" . ,*session*)
                       ("target" . ,group)
                       ("memberId" . ,member)
                       ("time" . ,time))))

(defun add-command (str func &optional describe)
  (setf (gethash str *command-map*) func)
  (setf (gethash str *help-map*) describe))

(defun remove-command (str)
  (setf (gethash str *command-map*) nil)
  (setf (gethash str *help-map*) nil))

(defun add-mode-command (str func-lst)
  (setf (gethash str *command-mode-map*) func-lst)
  (setf (gethash str *command-mode-active-map*) (make-array 3 :fill-pointer 0 :adjustable t)))

(defun remove-mode-command (str)
  (setf (gethash str *command-mode-map*) nil)
  (setf (gethash str *command-mode-active-map*) nil))

(defun active-mode (str group)
  (let ((mode (gethash str *command-mode-active-map*)))
    (when mode
      (vector-push-extend group (gethash str *command-mode-active-map*)))))

(defun deactive-mode (str group)
  (let ((mode (gethash str *command-mode-active-map*)))
    (when mode
      (setf (gethash str *command-mode-active-map*) (delete group mode)))))

(defun str-type (str)
  (let ((object (read-from-string str)))
    (if (numberp object)
  )))

(defun args-type (args &optional (types nil))
  (if (and (= 0 (length args)) (= 0 (length types))) t
      (when (and (not (= 0 (length types)))
                 (>= (length args) (length types)))
        (let ((result t))
          (dotimes (i (length args))
            (let ((object (read-from-string (elt args i))))
              (if (>= i (length types))
                  (setf result (funcall (car (last types)) object))
                  (setf result (funcall (elt types i) object)))))
      result))))

;;handle signal message
(defun handle-message (message)
  (let ((type (assoc-value message "type"))
        (text (get-text-message-chain (assoc-value message "messageChain")))
        (target (target-id (assoc-value message "sender"))))
    (if (stringp text)
        (progn
          (setf text (split-s text))
          (when *is-repeat*
            (let ((repeat (assoc-value *repeat-command* (first text))))
              (when repeat
                (send-text target repeat))))
          (maphash #'(lambda (k v)
                       (when (find target (gethash k *command-mode-active-map*))
                         (if (string= "help" (first text))
                             (send-text-lst target
                                                    (mapcar #'(lambda (x) (car x)) v))
                             (let ((command (assoc-value v (first text))))
                               (when command
                                 (funcall command
                                          (assoc-value message "sender")
                                          (cdr text)))))))
                   *command-mode-map*)
          (if (string= "陈睿" (first text))
              (if (gethash (second text) *command-map*)
                  (funcall (gethash (second text) *command-map*)
                           (assoc-value message "sender")
                           (cdr (cdr text)))
                  (send-text target "没有这个命令哟!"))))
        (format t "text is null~%"))))

(defun get-all-command ()
  (let ((result nil))
    (maphash #'(lambda (k v)
                 (when (not (string= k "帮助"))
                   (setf result (append result (list k)))))
             *command-map*)
    result))

(add-command "help"
             #'(lambda (sender args)
                 (if (or (args-type args) (args-type args (list #'symbolp)))
                     (let ((target (target-id sender))
                           (commands (get-all-command))
                           (command (first args)))
                       (if command
                           (if (find command commands :test #'string=)
                               (let ((help (gethash command *help-map*)))
                                 (if help
                                     (send-text target help)
                                     (send-text target "这个命令没有帮助,这么简单的命令还需要看帮助,你真是太弱了")))
                               (send-text target "没有这个命令, 你可以用 陈睿 help 来获取所以命令"))
                           (progn
                             (send-text target "命令的使用方法 陈睿 命令 参数1 参数2,所有命令后面的参数以空格分割, help 后面加命令的名字获取每条命令详细帮助")
                             (send-text target
                                                (lst-line-string commands)))))
                     (send-text (target-id sender) "参数不对"))))

(add-command "添加回复"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list #'symbolp #'symbolp))
                       (progn
                         (add-repeat `(,(first args) . ,(second args)))
                         (send-text target "成功"))
                       (send-text target "参数错误"))))
             "第一个参数为你说的,第二个参数为机器人回复的")

(add-command "删除回复"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list #'symbolp))
                       (progn
                         (remove-repeat (first args))
                         (send-text target "成功"))
                       (send-text target "参数错误"))))
             "第一个参数为你说的")

(add-command "列出回复"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (let ((repeats (mapcar #'(lambda (x)
                                              (format nil "~A:~A" (car x) (cdr x)))
                                          *repeat-command*)))
                     (send-text-lst target repeats)))))

(add-command "列出群聊"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (get-group-list)
                   (let ((result nil))
                     (dotimes (i (length *group-list*))
                       (setf result (append result
                                            (list (format nil "~A:~A ~A" i
                                                          (qq-group-id (elt *group-list* i))
                                                          (qq-group-name (elt *group-list* i)))))))
                     (send-text-lst target result)))))

(defun run ()
  (do ()
      (nil 'done)
    (let ((message (car (last (fetch-last-message)))))
      (if message
	  (let ((type (assoc-value message "type")))
            (format t "message:~A~%" message)
	    (format t "type:~A~%" type)
	    (when (or (string= "FriendMessage" type) (string= "GroupMessage" type))
              (format t "message:~A~%" message)
	      (handle-message message)))))))

(in-package :cl-user)
