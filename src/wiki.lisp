(defpackage :qqbot.wiki
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web :yason))
(in-package :qqbot.wiki)

(defun get-info (title)
  (web-get "zh.moegirl.org.cn"
           "api.php"
           :args `(("action" . "query")
                   ("titles" . ,title)
                   ("prop" . "info")
                   ("format" . "json")
                   ("inprop" . "url|talkid"))
           :jsonp t))

(defun handle-data (json)
  (let ((data (assoc-value (assoc-value json "query") "pages")))
    (mapcar #'(lambda (x)
                (if (not (string= "-1" (car x)))
                    (list
                     (assoc-value (cdr x)
                                  "pageid")
                     (assoc-value (cdr x)
                                  "title")
                     (assoc-value (cdr x)
                                  "fullurl"))))
            data)))

(defun query (title)
  (car (handle-data (get-info title))))

(add-command "wiki"
             #'(lambda (sender args)
                 (let ((target (target-id sender))
                       (result (query (string-merges args " "))))
                   (if result
                       (send-text-lst target
                                      (cdr result))
                       (send-text target "没有查询到呦！")))))

(in-package :cl-user)
