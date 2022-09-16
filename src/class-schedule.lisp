(defpackage :qqbot.class-schedule
  (:use :cl :qqbot.head :qqbot.bot :qqbot.task :local-time)
  (:export
   ))
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

(defun get-today-class-schedule (class-schedule)
  (let ((week (timestamp-day-of-week (today))))
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

(add-command "课表"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (send-text-lst (target-id sender)
                                (get-today-class-schedule
                                 (load-class-schedule
                                  (search-person-class
                                   (sender-id sender))))))
             "获取今日课表")
