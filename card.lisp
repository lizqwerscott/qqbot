(in-package :qqbot.card)

(defparameter *cards* (make-hash-table :test #'equal))

(defun load-cards ()
  (dolist (card (list-directory (merge-pathnames "cards/" (get-source-dir))))
    (let ((name (pathname-name card)))
      (setf (gethash name *cards*) (load-json-file card)))))

(defun list-cards ()
  (let ((result))
    (maphash #'(lambda (k v)
                 (setf result (append result (list k))))
             *cards*)
    result))

(defun list-card-select (card)
  (let ((result))
    (dolist (select (cdr (gethash card *cards*)))
      (setf result (append result (list (car select)))))
    result))

(defun random-get (lst)
  (if (= 1 (length lst))
      (car lst)
      (let ((x (random-int-r (- (length lst) 1))))
        (elt lst x))))

(defun random-s (n max)
  (let ((result 0))
    (dotimes (i n)
      (let ((temp (random-int-r max)))
        (incf result temp)))
    result))

(defparameter *parse-z-regx* "(\\[).*?(\\])")
(defparameter *parse-d-regx* "{.*?}")

(defun code-regx (str regx)
  (scan-to-strings regx str))

(defun remove-wrap (str)
  (subseq str 1 (- (length str) 1)))

(defun string-symbol (str)
  (read-from-string (format nil ":~A" str)))

(defun handle-signal (data)
  `(,(car data) . ,(random-get (cdr data))))

(defun handle-code (code)
  (format nil "~A" (eval (read-from-string code))))

(defun replace-all (str regx func)
  (let ((result str))
    (do ((code (code-regx result regx) (code-regx result regx)))
        ((not code) result)
      (setf result (regex-replace regx result (funcall func (remove-wrap code)))))))

(defun draw-card-step (str data)
  (let ((result str))
    (setf result (replace-all result *parse-d-regx*
                              #'(lambda (in)
                                  (random-get (assoc-value data in)))))
    (setf result (replace-all result *parse-z-regx*
                              #'(lambda (in)
                                  (handle-code in))))
    (if (not (string= result str))
        (progn
          (draw-card-step result data))
        result)))

(defun draw-card (card)
  (let ((data (gethash card *cards*)))
    (draw-card-step (random-get (assoc-value data card)) data)))

(load-cards)

(add-command "抽卡"
             #'(lambda (sender args)
                 (if (args-type args (list #'symbolp))
                     (let ((target (target-id sender))
                           (card (car args)))
                       (format t "card:~A~%" card)
                       (if (find card (list-cards) :test #'string=)
                           (send-text target (draw-card card))
                           (progn
                             (send-text target "你要的卡堆不存在....请重新选择吧")
                             (send-text target
                                                (lst-line-string (list-cards))))))
                     (send-text (target-id sender) "参数错误")))
             (let ((help nil))
               (setf help (append help (list "第一个参数为要抽的卡堆." "接下来是卡堆列表")))
               (setf help (append help (list-cards)))
               (lst-line-string help)))

(in-package :cl-user)
