(in-package :qqbot.task)

(defstruct task
  name
  time
  func)

(defparameter *tasks* (make-hash-table :test #'equal))
(defparameter *run-tasks* nil)
(defparameter *start-tasks* nil)

(defun get-time (hour minute &optional (second 0))
  (let ((today-t (now)))
    (make-timestamp )
    (encode-timestamp 0 second minute hour
                      (timestamp-day today-t)
                      (timestamp-month today-t)
                      (timestamp-year today-t))))

(defun time-mintue= (time)
  (let ((time-now (timestamp-to-universal (now))))
    (and (>= time-now
             (timestamp-to-universal time))
         (<= time-now
             (timestamp-to-universal (get-time (timestamp-hour time)
                                               (+ 1 (timestamp-minute time))))))))

(defun time= (time)
  (let ((time-now (timestamp-to-universal (now))))
    (= time-now
       (timestamp-to-universal time))))

(defun add-run-task (task)
  (setf *run-tasks*
        (append *run-tasks* (list task))))

(defun remove-run-task (task)
  (setf *run-tasks*
        (remove-if #'(lambda (x)
                       (string= (task-name x)
                                (task-name task)))
                   *run-tasks*)))

(defun add-task (func name time)
  (when (stringp name)
    (when (not (gethash name *tasks*))
      (setf (gethash name *tasks*)
            (make-task :name name :time time :func func)))))

(defun remove-task (name)
  (when (stringp name)
    (when (gethash name *tasks*)
      (remove-run-task (gethash name *tasks*))
      (setf (gethash name *tasks*) nil))))

(defun add-start-task (func name &optional (time 1))
  (setf *start-tasks*
            (append *start-tasks*
                    (list (add-task func name time)))))

(defun remove-start-task (name)
  (remove-task name)
  (setf *start-tasks*
        (remove-if #'(lambda (x)
                       (string= (task-name x)
                                name))
                   *start-tasks*)))

(defun start-task (name)
  (when (stringp name)
    (when (gethash name *tasks*)
      (when (not (find-if #'(lambda (x)
                              (string= name (task-name x)))
                          *run-tasks*))
        (add-run-task (gethash name *tasks*))))))

(defun stop-task (name)
  (when (stringp name)
    (when (gethash name *tasks*)
      (when (find-if #'(lambda (x)
                         (string= name (task-name x)))
                     *run-tasks*)
        (remove-run-task (gethash name *tasks*))))))

(defun run-tasks ()
  (mapcar #'(lambda (task)
              (list (task-time task)
                    (task-func task)))
          *run-tasks*))

(defun run-start-tasks ()
  (mapcar #'(lambda (task)
              (list (task-time task)
                    (task-func task)))
          *start-tasks*))

(add-command "列出任务"
             #'(lambda (sender args)
                 (maphash #'(lambda (k v)
                              (sleep 1)
                              (send-text (target-id sender)
                                         k))
                          *tasks*)))

(in-package :cl-user)
