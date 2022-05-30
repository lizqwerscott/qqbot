(in-package :qqbot.moyu)

(defun get-json ()
  (web-get "api.j4u.ink"
           "proxy/remote/moyu.json"
           :jsonp t))

(defun handle-moyu (json)
  (if (= 200 (assoc-value json "code"))
      (assoc-value (assoc-value json "data") "moyu_url")
      (error (assoc-value json "message"))))

(defun moyu (target)
  (send-picture target
                (handle-moyu (get-json))))

(add-command "摸鱼"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (format t "摸鱼balbal~%")
                 (moyu (target-id sender))))

(in-package :cl-user)
