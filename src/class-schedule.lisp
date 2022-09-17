(defpackage :qqbot.class-schedule
  (:use :cl :qqbot.head :qqbot.bot :qqbot.task :local-time)
  (:export
   :get-class-schedule
   :send-today-class-schedule))
(in-package :qqbot.class-schedule)

(defparameter +person-class+ nil)

(defun load-class-person (path)
  (load-json-file path))

(setf +person-class+
      (load-class-person (merge-pathnames "data/class.json"
                                           (get-data-dir))))

(defun search-person-class (person-qq)
  (car
   (find person-qq
         (assoc-value +person-class+ "class")
         :key #'(lambda (x)
                  (car (cdr (car (cdr x)))))
         :test #'=)))

(defun load-class-schedule (class)
  (let ((path (merge-pathnames (format nil "classSchedule/~A.json" class)
                               (get-data-dir))))
    (when (probe-file path)
      (load-json-file path))))

(defun probe-week-s ()
  "signal")

(defun get-week-class-schedule (class-schedule &optional (week (timestamp-day-of-week (today))))
  (when (and (<= week 5)
             (> week 0))
    (mapcar #'(lambda (class i)
                (let ((data (assoc-value class
                                         (probe-week-s))))
                  (format nil "~A ~A"
                          i
                          (if (listp data)
                              (let ((first-week (car
                                                 (assoc-value data
                                                              "weeks"))))
                                (format nil "~A ~A ~A ~A"
                                        (assoc-value data "name")
                                        (assoc-value first-week "room")
                                        (assoc-value first-week "teacher")
                                        (assoc-value first-week "week")))
                              "没课!"))))
            (elt class-schedule (- week 1))
            (list "第一节" "第二节" "第三节" "第四节"))))

(defun get-class-schedule (person &optional (week (timestamp-day-of-week (today))))
  (get-week-class-schedule (load-class-schedule
                            (search-person-class person))
                           week))

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
                 (let ((class (search-person-class
                               (sender-id sender))))
                   (if class
                       (let ((schedule (load-class-schedule class)))
                         (if schedule
                             (let ((week-s (get-week-class-schedule schedule)))
                               (if week-s
                                   (send-text-lst (target-id sender)
                                                  week-s)
                                   (send-text (target-id sender)
                                              "今天没课!")))
                             (send-text (target-id sender)
                                        "没有你们班的课表!")))
                       (send-text (target-id sender)
                                  "不知道你是那个班的!"))))
             "获取今日课表")
