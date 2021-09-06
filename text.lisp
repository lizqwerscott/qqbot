(in-package :qqbot.text)

(defparameter *mi-yan* nil)

(defun load-miyan ()
  (load-json-file (merge-pathnames "data/名言.json"
                                   (get-source-dir))))

(setf *mi-yan* (load-miyan))

(defun random-lst (lst)
  (elt lst (random-int-r (- (length lst) 1))))

(defun miyanlist ()
  (mapcar #'(lambda (x)
              (car x))
          *mi-yan*))

(defun get-random-text (command)
  (web-get "api.vvhan.com" (format nil "api/~A" command)))

(defun get-random-sici (fenlei)
  (web-get "v1.jinrishici.com" fenlei :jsonp t))

(defun bullet-fly-load ()
  (load-line-file (merge-pathnames "data/bullet.txt"
                                   (get-source-dir))))

(add-command "笑话"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                                    (get-random-text "xh")))
             "获取随机一条笑话")

(add-command "骚话"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                                    (get-random-text "sao")))
             "获取随机一条骚话")

(add-command "情话"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                                    (get-random-text "love")))
             "获取随机一条情话")

(add-command "让子弹飞一会"
             #'(lambda (sender args)
                 (dolist (line (bullet-fly-load))
                   (sleep 1)
                   (send-text (target-id sender) line))))

(add-command "名言"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (or (args-type args (list #'numberp))
                           (args-type args))
                       (dotimes (i (if (car args) (parse-integer (car args)) 1))
                         (sleep 1)
                         (let ((miyans (random-lst *mi-yan*)))
                           (let ((miyan (random-lst (cdr miyans))))
                             (if (listp miyan)
                                 (progn
                                   (send-text target (car miyans))
                                   (send-text-lst target miyan))
                                 (send-text target (format nil "~A --~A" miyan (car miyans)))))))
                       (send-text target "参数错误, 例子:陈睿 名言 2 或者 陈睿 名言")))))

(add-command "名言人物"
             #'(lambda (sender args)
                 (send-text-lst (target-id sender) (miyanlist))))

(add-command "人物名言"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list #'symbolp))
                       (let ((key (assoc-value *mi-yan* (car args))))
                         (if key
                             (dolist (i key)
                               (sleep 1)
                               (if (listp i)
                                   (send-text-lst target i)
                                   (send-text target i)))
                             (send-text target "没有找到这个人呢!")))
                       (send-text target "参数错误, 例子:陈睿 人物名言 缘之空")))))

(add-command "来句诗"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (let ((siju (get-random-sici "all")))
                     (format t "类型:~A~%" (assoc-value siju "category"))
                     (send-text target (assoc-value siju "content"))
                     (send-text-lst target (list (format nil "类型:~A" (assoc-value siju "category"))
                                                 (format nil "来自:~A" (assoc-value siju "origin"))
                                                 (format nil "作者:~A" (assoc-value siju "author"))))))))

(in-package :cl-user)
