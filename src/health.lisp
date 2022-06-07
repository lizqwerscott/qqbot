(defpackage :qqbot.health
  (:use :cl :qqbot.head :qqbot.web :qqbot.bot :qqbot.task)
  (:export
   :load-health
   :health-up-all))
(in-package :qqbot.health)

(defvar *health-info* nil)

(defun load-health (&optional (path "./info.txt"))
  (setf *health-info*
        (mapcar #'(lambda (people)
                    (split-s people ","))
                (load-line-file path))))

(defun get-last-day (user-id uv-code)
  (web-get "mhealthyup.yingxinbao.net"
           "welcome/publish/mobile/healthy/config/modules/details"
           :args `(("uvCode" . ,uv-code)
                   ("mcode" . "dayUp")
                   ("userId" . ,user-id))
           :jsonp t))

(defun set-temparture (json &optional (temparture 36.2))
  (setf (cdr (car (last (car (cdr (car json))))))
        temparture)
  json)

(defun publish (user-id uv-code)
  (web-post-json "mhealthyup.yingxinbao.net"
                 "welcome/publish/mobile/healthy/config/modules/save"
                 :args (set-temparture (get-last-day user-id uv-code) 36.3)
                 :jsonp t))

(defun health-up-all ()
  (mapcar #'(lambda (people)
              (format t "正在为~A上传健康打卡信息~%" (second people))
              (send-text (first people)
                         "正在为您上传健康打卡信息")
              (apply #'publish (cdr (cdr people)))
              (let ((result (apply #'publish (cdr (cdr people)))))
                (format t "~A~%"
                        (assoc-value result
                                     "msg"))
                (send-text (first people)
                           (assoc-value result
                                        "msg"))))
         *health-info*))

(load-health (merge-pathnames "data/health.txt" (get-data-dir)))

(add-task #'health-up-all
          "healthup"
          (list 8 0 :step-hour 2))

(start-task "healthup")
