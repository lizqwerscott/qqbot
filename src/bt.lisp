(defpackage :qqbot.bt
  (:use :common-lisp :qqbot.bot :qqbot.head :cl-transmission)
  (:export))
(in-package :qqbot.bt)

(defvar *conn* (make-instance 'transmission-connection
                              :host "192.168.3.3"
                              :credentials '("transmission" "transmission")))

(defun add-bt (url)
  (transmission-add *conn* :filename url))

(defun classify (lst &optional (get-data nil) (pause nil) (finish nil) (another nil))
  (if (not lst) (list get-data pause finish)
      (let ((status (gethash :status (car lst))))
        (classify (cdr lst)
                  (append get-data (when (= status 4)
                                     (list (car lst))))
                  (append pause (when (= status 0)
                                  (list (car lst))))
                  (append finish (when (= status 6)
                                   (list (car lst))))
                  (append another (when (and (not (= status 4))
                                             (not (= status 0))
                                             (not (= status 6)))
                                    (list (car lst))))))))

(defun handle-bt (lst)
  (mapcar #'(lambda (item)
              (list (format nil "~A" (gethash :name item))
                    (format nil "速度:~AMB/s" (float (/ (gethash :rate-download item) (* 1024 1024))))
                    (format nil "进度:~A" (let ((total (gethash :total-size item))
                                                (left (gethash :left-until-done item)))
                                            (if (= total 0)
                                                "0%"
                                                (format nil "~A%" (float (* 100 (/ (- total left) total)))))))))
          lst))

;;return a list name have download and download rate
;;the bt status (:get-data 4 :pause 0 :finish 6)
(defun get-bt ()
  (mapcar #'(lambda (x)
              (handle-bt x))
          (classify (transmission-get *conn*
                                      #(:name :rate-download :total-size :left-until-done :status)
                                      :strict t))))

(add-command "上传磁力"
             #'(lambda (sender args)
                 (format t "bt:~A~%" args)
                 (let ((url (car args))
                       (target (target-id sender)))
                   (if args
                       (progn
                         (add-bt url)
                         (send-text target "上传完成"))
                     (send-text target "需要磁力链接")))))

(add-command "磁力列表"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (or (args-type args)
                           (args-type args (list #'symbolp)))
                       (let ((result (get-bt)))
                         (if (and args (car args))
                             (let ((args1 (car args)))
                               (dolist (item (cond ((string= args1 "未完成")
                                                     (first result))
                                                    ((string= args1 "暂停")
                                                     (second result))
                                                    ((string= args1 "完成")
                                                     (third result))
                                                    ((string= args1 "其他")
                                                     (fourth result))))
                                 (send-text-lst target item)))
                             (progn
                               (send-text-lst target
                                            (list (format nil "未完成的:~A" (length (first result)))
                                                  (format nil "没速度或者暂停的:~A" (length (second result)))
                                                  (format nil "完成的:~A" (length (third result)))
                                                  (format nil "其他:~A" (length (fourth result)))))
                               (send-text target "磁力列表命令可选择参数,未完成,暂停,完成,其他"))))
                     (send-text target "参数错误")))))

(in-package :cl-user)
