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

(defparameter *addres* "192.168.3.3:8080")

(defparameter *recive-picture* nil)

(defun recive-picture ()
  (setf *recive-picture* t))

(defun recive-picture-off ()
  (setf *recive-picture* nil))

(defun set-remote ()
  (setf *addres* "cn-zz-bgp-1.natfrp.cloud:11079"))

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
  (find (format nil "~A" qq) *admin* :test #'string=))

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
          (format t "error:id:~A {msg: ~A}~%" (assoc-value message "code") (assoc-value message "msg")))
      (format t "command error~%")))

(defun send-command-post (command args)
  (parse-data (web-post *addres* command :args args :jsonp t)))

(defun send-command-get (command args)
  (parse-data (web-get *addres* command :args args :jsonp t)))


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
  (when-bind (message (send-command-get "countMessage"
                                        `(("sessionKey" . ,*session*))))
    (setf *message-size* (assoc-value message "data"))))
;;return message list
(defun fetch-last-message ()
  (when-bind (message (send-command-get "fetchLatestMessage"
                                        `(("sessionKey" . ,*session*)
                                          ("count" . 1))))
    (assoc-value message "data")))

(defun get-group-list ()
  (when-bind (message (send-command-get "groupList"
                                        `(("sessionKey" . ,*session*))))
    (setf *group-list* (make-array 3 :fill-pointer 0 :adjustable t))
    (dolist (i (assoc-value message "data"))
      (vector-push-extend (make-qq-group :id (assoc-value i "id")
                                         :name (assoc-value i "name"))
                          *group-list*))))

(defun get-group-member (target)
  (send-command-get "memberList"
                    `(("sessionKey" . ,*session*)
                      ("target" . ,target))))

(defun gmessage-text (text)
  `(("type" . "Plain")
    ("text" . ,text)))

(defun gmessage-picture (url)
  `(("type" . "Image")
    ("url" . ,url)))

(defun gmessage-local-picture (path)
  `(("type" . "Image")
    ("path" . ,path)))

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

(defun gmessage-at (target)
  `(("type" . "At")
    ("target" . ,target)))

(defun send-message (target message-chain)
  (let ((command (if (find target *group-list* :key #'qq-group-id) "sendGroupMessage" "sendFriendMessage")))
    (send-command-post command
                       `(("sessionKey" . ,*session*)
                         ("target" . ,target)
                         ("messageChain" . ,message-chain)))))

(defun send-nudge (target group)
  (send-command-post "sendNudge"
                     `(("sessionKey" . ,*session*)
                       ("target" . ,target)
                       ("subject" . ,group)
                       ("kind" . "group"))))

(defun send-text (target str)
  (send-message target `(,(gmessage-text str))))

(defun send-text-lst (target texts)
  (send-message target `(,(gmessage-text (lst-line-string texts)))))

(defun send-picture (target url)
  (send-message target `(,(gmessage-picture url))))

(defun send-local-picture (target path)
  (send-message target `(,(gmessage-local-picture path))))

(defun send-picture-and-text (target path text)
  (send-message target `(,(gmessage-local-picture path)
                         ,(gmessage-text text))))

(defun send-picture-and-text-lst (target path texts)
  (send-message target `(,(gmessage-local-picture path)
                         ,(gmessage-text (lst-line-string texts)))))

(defun send-music-share (target title summary jumpUrl pictureUrl musicUrl &optional (brief ""))
  (send-message target `(,(gmessage-music-share title summary jumpUrl pictureUrl musicUrl brief))))

(defun send-json (target json)
  (send-message target `(,(gmessage-json json))))

(defun send-at (target person)
  (send-message target `(,(gmessage-at person))))

(defun send-at-text (target person text)
  (send-message target `(,(gmessage-at person)
                         ,(gmessage-text text))))

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
  (when-bind (mode (gethash str *command-mode-active-map*))
    (vector-push-extend group (gethash str *command-mode-active-map*))))

(defun deactive-mode (str group)
  (when-bind (mode (gethash str *command-mode-active-map*))
    (setf (gethash str *command-mode-active-map*) (delete group mode))))

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

(defun handle-mode-message (text target sender)
  (when *is-repeat*
    (when-bind (repeat (assoc-value *repeat-command* (first text)))
      (send-text target repeat))
    (maphash #'(lambda (k v)
                 (when (find target (gethash k *command-mode-active-map*))
                   (if (string= "help" (first text))
                       (send-text-lst target
                                      (mapcar #'(lambda (x) (car x)) v))
                       (when-bind (command (assoc-value v (first text)))
                         (funcall command
                                    sender
                                    (cdr text))))))
             *command-mode-map*)))

(defun handle-command (text target sender)
  (if (gethash (first text) *command-map*)
      (submit-job *patron*
                  (make-instance 'patron:job
                                 :function (lambda ()
                                             (funcall (gethash (first text) *command-map*)
                                                      sender
                                                      (cdr text)))))
      (send-text target "没有这个命令哟!")))

;;handle signal message
(defun handle-message (message)
  (let ((type (assoc-value message "type"))
        (message-chain (cdr (assoc-value message "messageChain")))
        (text (get-text-message-chain (assoc-value message "messageChain")))
        (sender (assoc-value message "sender"))
        (target (target-id (assoc-value message "sender"))))
    (let ((first-str (first message-chain)))
      (when (and *recive-picture*
                 (string= "Image" (assoc-value first-str "type")))
        (format t "is Image~%")
        (let ((url (assoc-value first-str "url"))
              (image-id (assoc-value first-str "imageId")))
          (format t "save it:~A~%" image-id)
          (let ((path (format nil "tmp/~A/" (target-id sender)))
                (dir (pathname (format nil "~Atmp/~A/" (get-source-dir) (target-id sender)))))
            (when (not (probe-file dir))
              (ensure-directories-exist dir))
            (save-picture-url url path image-id))))
      (if (string= "At" (assoc-value first-str "type"))
          (when (= 3027736450 (assoc-value first-str "target"))
            (when (and (second message-chain)
                       (string= "Plain" (assoc-value (second message-chain) "type")))
              (format t "handle command:~S~%" (assoc-value (second message-chain) "text"))
              (handle-command (split-s (clean (Assoc-value (second message-chain) "text")))
                              target
                              sender)))
          (when (string= "Plain" (assoc-value first-str "type"))
            (let ((message-text (split-s (clean (assoc-value first-str "text")))))
              (handle-mode-message message-text
                                   target
                                   sender)
              (when (or (string= "@伊蕾娜" (first message-text))
                        (string= "伊蕾娜" (first message-text)))
                (format t "handle command:~S~%" message-text)
                (when (cdr message-text)
                  (handle-command (cdr message-text)
                                  target
                                  sender)))))))))

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
                               (send-text target "没有这个命令, 你可以用 伊蕾娜 help 来获取所以命令"))
                           (progn
                             (send-text target "命令的使用方法 伊蕾娜 命令 参数1 参数2,所有命令后面的参数以空格分割, help 后面加命令的名字获取每条命令详细帮助")
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
             "第一个参数为你说的,第二个参数为伊蕾娜回复的")

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

(add-command "戳一戳"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list #'numberp #'numberp #'numberp))
                       (let ((group (elt *group-list* (parse-integer (second args)))))
                         (if group
                             (dotimes (i (parse-integer (third args)))
                               (sleep 1)
                               (send-nudge (parse-integer (first args))
                                           (qq-group-id group)))
                             (send-text target "群号请写列出群聊的编号")))
                       (send-text target "参数错误, 第一个参数为 发的qq号, 第二个为在那个群, 第三个为几次 (最多3次)示列:伊蕾娜 1963771277 0 5")))))

(in-package :cl-user)
