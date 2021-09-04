(in-package :qqbot.game)

(defparameter *miyu* nil)

(defstruct player
  people
  score)

(defstruct play
  miyu
  members)

(defparameter *now-play* (make-hash-table :test #'equal))

(defun add-member (play id name)
  (vector-push-extend (make-player :people (make-people :id id :name name)
                                   :score 0)
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
  (elt *miyu* (random-int-r (- (length *miyu*) 1))))

(defun get-score (id)
  (let ((members (sort (play-members (gethash id *now-play*))
                       #'>
                       :key #'(lambda (x)
                                (player-score x))))
        (result nil))
    (dotimes (i (length members))
      (setf result (append result (list (format nil "~A.~A:~A" i (people-name (player-people (elt members i)))
                                                (player-score (elt members i)))))))
    result))

(defun now-miyu (group)
  (first (play-miyu (gethash group *now-play*))))

(defun now-daan (group)
  (second (play-miyu (gethash group *now-play*))))

(defun next-miyu (group)
  (first (setf (play-miyu (gethash group *now-play*)) (random-miyu))))

(defun q-exit (sender args)
  (send-text-lst (target-id sender)
                         (append (list "分数:") (get-score (group-id sender))))
  (send-text (target-id sender) "不会了吧")
  (setf *now-miyu* nil)
  (setf (gethash (group-id sender) *now-play*) nil)
  (deactive-mode "谜语" (group-id sender)))

(defun q-timu (sender args)
  (let ((target (target-id sender)))
    (send-text target (now-miyu (group-id sender)))))

(defun q-cai (sender args)
  (let ((target (target-id sender)))
    (if (args-type args (list #'symbolp))
        (let ((play (gethash (group-id sender) *now-play*)))
          (when (not (find-member play (sender-id sender)))
            (add-member play
                        (sender-id sender)
                        (sender-name sender)))
          (if (string= (second (play-miyu play)) (car args))
                (progn
                  (send-text target "答对了")
                  (incf (player-score (find-member play (sender-id sender))))
                  (send-text target "下一题")
                  (send-text target (next-miyu (group-id sender))))
            (send-text target "答错了,再想一想吧")))
      (send-text target "参数错误"))))

(defun q-daan (sender args)
  (let ((target (target-id sender)))
    (let ((play (gethash (group-id sender) *now-play*)))
      (send-text target "扣除一分")
      (send-text target (format nil "谜底:~A" (now-daan (group-id sender))))
      (when (not (find-member play (sender-id sender)))
        (add-member play
                    (sender-id sender)
                    (sender-name sender)))
      (decf (player-score (find-member play (sender-id sender))))
      (send-text target "真是屑,连这么简单的题都不会>_<")
      (send-text target "下一题")
      (send-text target (next-miyu (group-id sender))))))

(defun q-score (sender args)
    (let ((result (get-score (group-id sender))))
      (if result
          (send-text-lst (target-id sender) result)
          (send-text (target-id sender) "没人猜过, 所以没有分数"))))

(let ((func-lst))
  (let ((add-lst #'(lambda (str func)
                     (setf func-lst (append func-lst (list `(,str . ,func)))))))
    (funcall add-lst "猜" #'q-cai)
    (funcall add-lst "答案" #'q-daan)
    (funcall add-lst "题目" #'q-timu)
    (funcall add-lst "分数" #'q-score)
    (funcall add-lst "exit" #'q-exit))
  (add-mode-command "谜语" func-lst))

(add-command "猜谜"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (when (sender-groupp sender)
                     (active-mode "谜语" (group-id sender))
                     (send-text target "第一题")
                     (setf (gethash (group-id sender) *now-play*)
                           (make-play :miyu (random-miyu)
                                      :members (make-array 4 :fill-pointer 0 :adjustable t)))
                     (send-text target (first (play-miyu (gethash (group-id sender) *now-play*))))))))

(in-package :cl-user)
