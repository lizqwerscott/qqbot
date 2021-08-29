(in-package :qqbot.game)

(defclass r-roulette ()
  ((group
    :accessor r-group
    :initarg :group
    :initform 0)
   (status
    :accessor r-status
    :initform :match) ;match start
   (placen
    :accessor r-placen
    :initarg :placen
    :initform 6)
   (place-on-bullet
    :accessor r-pb
    :initform nil)
   (bulletn
    :accessor r-bullet
    :initarg :bulletn
    :initform 1)
   (run-place
    :accessor r-rplace
    :initform 0)
   (run
    :accessor r-run
    :initform 0)
   (player
    :accessor r-player
    :initform (make-array 2 :fill-pointer 0 :adjustable t))))

(defparameter *group-m* (make-hash-table :test #'equal))

(defmethod initialize-instance :after ((play r-roulette) &key)
  (let ((result))
    (dotimes (i (r-bullet play))
      (setf result (append result (list (random-int-r (r-placen play))))))
    (setf (r-pb play) result)))

(defmethod get-run-player ((play r-roulette))
  (elt (r-player play) (r-run play)))

(defmethod next-player ((play r-roulette))
  (setf (r-run play)
        (if (= (- (length (r-player play)) 1) (r-run play)) 0
            (+ 1 (r-run play)))))

(defmethod next-place ((play r-roulette)) (setf (r-rplace play) (if (= (r-rplace play) (r-placen play)) 0
            (+ (r-rplace play) 1))))

(defmethod add-player ((play r-roulette) joiner)
  (if (not (find joiner (r-player play)))
      (vector-push-extend joiner (r-player play))))

(defun create (group starter placen bulletn)
  (let ((play (make-instance 'r-roulette :group group :placen placen :bulletn bulletn)))
    (add-player play starter)
    (setf (gethash group *group-m*) play)))

(defun join (group joiner)
  (let ((play (gethash group *group-m*)))
    (when play
      (add-player play joiner))))

(defmethod shoot ((play r-roulette))
  (let ((result (find (r-rplace play) (r-pb play))))
    (remove result (r-pb play))
    (if result
        (progn
          (decf (r-bullet play))
          (setf (r-player play) (delete (elt (r-player play) (r-run play)) (r-player play)))))
    result))

(defun list-player (group)
  (let ((len (length (r-player (gethash group *group-m*)))))
    (format t "len:~A~%" len)
    (dotimes (i len)
      (format t "~A~%" (elt (r-player (gethash group *group-m*)) i)))))

(defmethod win-player ((play r-roulette))
  (let ((win))
    (dotimes (i (length (r-player play)))
      (setf win (append win (list (elt (r-player play) i)))))
    win))

(defun finish (group)
  (deactive-mode "俄罗斯轮盘" group)
  (setf (gethash group *group-m*) nil))

(defun q-join (sender args)
  (let ((target (target-id sender)))
    (let ((play (gethash (group-id sender) *group-m*)))
      (when play
        (if (equal :match (r-status play))
            (progn
              (join (group-id sender) (sender-id sender))
              (send-message-text target "您已经加入")
              (list-player (group-id sender)))
            (send-message-text target "对局已经开始"))))))

(defun q-start (sender args)
  (let ((target (target-id sender)))
    (let ((play (gethash (group-id sender) *group-m*)))
      (if play
          (progn
            (setf (r-status play) :start)
            (send-message-text target "对局开始.")
            (send-message-text target "请创建者使用shoot开始游戏"))
          (send-message-text target "没有对局")))))

(defun q-shoot (sender args)
  (let ((target (target-id sender))
        (play (gethash (group-id sender) *group-m*)))
    (if (and play (equal :start (r-status play)))
        (if (= (sender-id sender) (elt (r-player play) (r-run play)))
            (progn
              (send-message-text target "蹦...")
              (if (shoot play)
                  (progn
                    (send-message-text target "你死了")
                    (mute-group-member (group-id sender)
                                       (sender-id sender) 60))
                  (send-message-text target "你活下来了"))
              (send-message-text target (format nil "现在还有~A颗子弹" (r-bullet play)))
              (send-message-text target (format nil "现在还有~A" (r-rplace play)))
              (if (or (= 1 (length (r-player play))) (= 0 (r-bullet play)))
                  (progn
                    (dolist (i (win-player play))
                      (send-message-text target (format nil "获胜者:~A" i)))
                    (send-message-text target "游戏结束")
                    (finish (group-id sender)))
                  (progn
                    (next-player play)
                    (next-place play)
                    (send-message-text target
                                       (format nil "现在请~A" (get-run-player play))))))
            (send-message-text target "现在是其他人开枪的时候."))
        (send-message-text target "没有对局或者对局处于匹配中"))))

(defun q-exit (sender args)
  (let ((target (target-id sender))
        (play (gethash (group-id sender) *group-m*)))
    (if play
        (if (find (sender-id sender) (r-player play))
            (progn
              (finish (group-id sender))
              (send-message-text target "对局已退出"))
            (send-message-text target "你没有权限"))
        (send-message-text target "本群没有对局"))))

(let ((func-lst))
  (let ((add-lst #'(lambda (str func)
                     (setf func-lst (append func-lst (list `(,str . ,func)))))))
    (funcall add-lst "join" #'q-join)
    (funcall add-lst "start" #'q-start)
    (funcall add-lst "shoot" #'q-shoot)
    (funcall add-lst "exit" #'q-exit))
  (add-mode-command "俄罗斯轮盘" func-lst))

(add-command "俄罗斯轮盘"
             #'(lambda (sender args)
                 (if (args-type args (list #'numberp #'numberp))
                     (let ((target (target-id sender)))
                       (when (group-id sender)
                         (if (not (gethash (group-id sender) *group-m*))
                             (progn
                               (format t "~A,~A~%" (first args) (second args))
                               (create (group-id sender) (sender-id sender) (parse-integer (first args)) (parse-integer (second args)))
                               (active-mode "俄罗斯轮盘" (group-id sender))
                               (send-message-text target "对局已经创建, 请其他人使用join加入对局"))
                             (send-message-text target "本群正在进行对局或匹配中"))))
                     (send-message-text (target-id sender) "参数错误")))
             "开启一局俄罗斯轮盘, 规则为先进行匹配,然后输入 start开始游戏, 创建游戏的人用shoot先开枪,后面第一个参数为枪有多少个子弹位置, 第二个参数为有多少颗子弹")

(in-package :cl-user)
