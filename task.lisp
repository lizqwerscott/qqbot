(in-package :qqbot.task)

(defstruct task
  name
  runp
  func)

(defparameter *tasks* (make-hash-table :test #'equal))

(defun find-thread (name)
  (find name (all-threads) :test #'string= :key #'thread-name))

(defun add-task (func name)
  (when (string name)
    (when (not (gethash name *tasks*))
      (setf (gethash name *tasks*) (make-task :name name :runp nil :func func)))))

(defun remove-task (name)
  (when (stringp name)
    (when (gethash name *tasks*)
      (setf (gethash name *tasks*) nil))))

(defun start-task (name)
  (when (stringp name)
    (when (gethash name *tasks*)
      (make-thread #'(lambda ()
                       (let ((one (gethash name *tasks*)))
                         (when one
                           (setf (task-runp one) t)
                           (format t "start~%")
                           (do ()
                               ((not (task-runp one)) 'finish)
                             (funcall (task-func one) one))
                           (format t "stop~%"))))
                   :name name))))

(defun stop-task (name)
  (when (stringp name)
    (when (gethash name *tasks*)
      (setf (task-runp (gethash name *tasks*)) nil))))

(in-package :cl-user)
