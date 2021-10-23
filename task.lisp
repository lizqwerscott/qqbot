(in-package :qqbot.task)

(defstruct task
  name
  time
  func)

(defparameter *tasks* (make-hash-table :test #'equal))
(defparameter *run-tasks* nil)

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

(defun find-thread (name)
  (find name (all-threads) :test #'string= :key #'thread-name))

(defun add-task (func name time)
  (when (string name)
    (when (not (gethash name *tasks*))
      (setf (gethash name *tasks*)
            (make-task :name name :time time :func func)))))

(defun remove-task (name)
  (when (stringp name)
    (when (gethash name *tasks*)
      (remove-run-task (gethash name *tasks*))
      (setf (gethash name *tasks*) nil))))

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

(in-package :cl-user)
