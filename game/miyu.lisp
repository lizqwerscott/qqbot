(in-package :qqbot.game)

(defparameter *miyu* nil)
(defparameter *now-miyu* nil)

(defstruct player
  people
  score)

(defstruct play
  miyu
  members)

(defparameter *now-play* (make-hash-table :test #'equal))

(defun add-member (play id name)
  (vector-push-extend (make-player :people (make-people :id id :name name)
                                   :score 1)
                      (play-members play)))

(defun find-member (play id)
  (let ((in nil)
        (members (play-members play)))
    (find id members :key #'(lambda (x)
                              (people-id (player-people x)))
          :test #'=)))

(defun load-miyu ()
  (let ((str (load-line-file (merge-pathnames "data/谜语.txt"
                                              (get-source-dir)))))
    (mapcar #'(lambda (line)
                (split-s line ":"))
            str)))

(setf *miyu* (load-miyu))

(defun random-miyu ()
  (elt *miyu* (random-int-r (length *miyu*))))

(defun q-exit (sender args)
  (send-message-text (target-id sender) "不会了吧")
  (setf *now-miyu* nil)
  (setf (gethash (group-id sender) *now-play*) nil)
  (deactive-mode "谜语" (group-id sender)))

(defun q-cai (sender args)
  (let ((target (target-id sender)))
    (if (args-type args (list #'symbolp))
        (let ((play (gethash (group-id sender) *now-play*)))
          (if (string= (second (play-miyu play)) (car args))
            (progn
              (send-message-text target "答对了")
              (let ((member (find-member play (sender-id sender))))
                (format t "sender-name:~A~%" (sender-name sender))
                (format t "sender:~A~%" sender)
                (if member
                    (incf (player-score member))
                    (add-member play
                                (sender-id sender)
                                (sender-name sender))))
              (send-message-text target "下一题")
              (setf (play-miyu (gethash (group-id sender) *now-play*)) (random-miyu))
              (send-message-text target (first (play-miyu play))))
            (send-message-text target "答错了,再想一想吧")))
        (send-message-text target "参数错误"))))

(defun q-score (sender args)
  (let ((members (sort (play-members (gethash (group-id sender) *now-play*)) #'> :key #'(lambda (x)
                                                                                          (player-score x))))
        (result nil))
    (format t "member:~A~%" members)
    (dotimes (i (length members))
      (setf result (append result (list (format nil "~A.~A:~A" i (people-name (player-people (elt members i)))
                                                (player-score (elt members i)))))))
    (format t "result:~A~%" result)
    (send-message-text-lst (target-id sender) result)))

(let ((func-lst))
  (let ((add-lst #'(lambda (str func)
                     (setf func-lst (append func-lst (list `(,str . ,func)))))))
    (funcall add-lst "猜" #'q-cai)
    (funcall add-lst "分数" #'q-score)
    (funcall add-lst "exit" #'q-exit))
  (add-mode-command "谜语" func-lst))

(add-command "猜谜"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (when (sender-groupp sender)
                     (active-mode "谜语" (group-id sender))
                     (send-message-text target "第一题")
                     (setf (gethash (group-id sender) *now-play*)
                           (make-play :miyu (random-miyu)
                                      :members (make-array 4 :fill-pointer 0 :adjustable t)))
                     (send-message-text target (first (play-miyu (gethash (group-id sender) *now-play*))))))))

(in-package :cl-user)
