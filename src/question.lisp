(defpackage :qqbot.question
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web :yason)
  (:export))
(in-package :qqbot.question)

(defun search-ti (keyword)
  (let ((json (web-get "tool.chaoxing.zmorg.cn/api/search.php"
                       "api/search.php"
                       :args `(("q" . ,keyword))
                       :jsonp t)))
    (when (= 1 (assoc-value json "code"))
      (assoc-value json "msg"))))

(add-command "查题"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list #'symbolp))
                       (let ((q (search-ti (car args))))
                         (send-text target (if q
                                               q
                                               "报错")))
                       (send-text target "查的题为空")))))

(in-package :cl-user)
