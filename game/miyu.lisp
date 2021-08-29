(in-package :qqbot.game)

(defparameter *miyu* nil)
(defparameter *now-miyu* nil)

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
  (deactive-mode "谜语" (group-id sender)))

(defun q-cai (sender args)
  (let ((target (target-id sender)))
    (if (args-type args (list #'symbolp))
        (if (string= (second *now-miyu*) (car args))
            (progn
              (send-message-text target "答对了")
              (send-message-text target "下一题")
              (setf *now-miyu* (random-miyu))
              (send-message-text target (first *now-miyu*)))
            (send-message-text target "答错了,再想一想吧"))
        (send-message-text target "参数错误"))))

(let ((func-lst))
  (let ((add-lst #'(lambda (str func)
                     (setf func-lst (append func-lst (list `(,str . ,func)))))))
    (funcall add-lst "猜" #'q-cai)
    (funcall add-lst "exit" #'q-exit))
  (add-mode-command "谜语" func-lst))

(add-command "猜谜"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (active-mode "谜语" (group-id sender))
                   (send-message-text target "第一题")
                   (setf *now-miyu* (random-miyu))
                   (send-message-text target (first *now-miyu*)))))

(in-package :cl-user)
