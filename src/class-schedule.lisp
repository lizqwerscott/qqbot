(defpackage :qqbot.class-schedule
  (:import-from :yason :parse)
  (:use :cl :qqbot.head :qqbot.bot :qqbot.task :local-time :qqbot.web)
  (:export
   :get-class-schedule
   :send-today-class-schedule))
(in-package :qqbot.class-schedule)

(defparameter +person-class+ nil)
(defparameter +remote-address+ "124.222.100.66:8089")

(defun load-class-person (path)
  (load-json-file path))

(setf +person-class+
      (load-class-person (merge-pathnames "data/class.json"
                                           (get-data-dir))))

(defun search-person-name (person-qq &optional (person (assoc-value +person-class+
                                                                    "person")))
  (when person
    (if (= person-qq
           (cdr
            (car person)))
        (car (car person))
        (search-person-name person-qq
                            (cdr person)))))

(defun get-today-class-schedule (name)
  (web-post-json +remote-address+
                            "todayclass"
                            :args `(("person" . ,name)
                                    ("jsonp" . t))))

(defun get-tomorrow-class-schedule (name)
  (web-post-json +remote-address+
                 "tomorrowclass"
                 :args `(("person" . ,name)
                         ("jsonp" . t))))

(defun handle-class-schedule (res)
  (let ((data (parse res)))
        (when (= 200
                 (assoc-value data "msg"))
          (assoc-value data "result"))))

(defun send-today-class-schedule ()
  (mapcar #'(lambda (class)
              (let ((class-schedule (load-class-schedule
                                     (car class)))
                    (class-person (assoc-value (cdr class)
                                               "person")))
                (let ((week-s (get-week-class-schedule class-schedule)))
                  (dolist (i class-person)
                    (if week-s
                        (send-text-lst i
                                       week-s)
                        (send-text i
                                   "今天没课!"))))))
          (assoc-value +person-class+
                       "class")))

(add-command "课表"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (let ((person-name (search-person-name
                                     (sender-id sender))))
                   (if person-name
                       (let ((class (handle-class-schedule
                                     (get-today-class-schedule
                                      person-name))))
                         (if class
                             (send-text-lst (target-id sender)
                                            class)
                             (send-text (target-id sender)
                                        "今天没课!")))
                       (send-text (target-id sender)
                                  "不知道你是那个班的!"))))
             "获取今日课表")

(add-command "明天课表"
             #'(lambda (sender args)
                 (declaim (ignore args))
                 (let ((person-name (search-person-name
                                     (sender-id sender))))
                   (if person-name
                       (let ((class (handle-class-schedule
                                     (get-tomorrow-class-schedule
                                      person-name))))
                         (if class
                             (send-text-lst (target-id sender)
                                            class)
                             (send-text (target-id sender)
                                        "今天没课!")))
                       (send-text (target-id sender)
                                  "不知道你是那个班的!")))))
