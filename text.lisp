(in-package :qqbot.text)

(defparameter *mi-yan* nil)

(defun load-miyan ()
  (load-json-file (merge-pathnames "data/名言.json"
                                   (get-source-dir))))

(setf *mi-yan* (load-miyan))

(defun random-lst (lst)
  (elt lst (random-int-r (- (length lst) 1))))

(defun miyanlist ()
  (mapcar #'(lambda (x)
              (car x))
          *mi-yan*))

(defun get-random-text (command)
  (web-get "api.vvhan.com" (format nil "api/~A" command)))

(defun get-random-sici (fenlei)
  (web-get "v1.jinrishici.com" fenlei :jsonp t))

(defun bullet-fly-load ()
  (load-line-file (merge-pathnames "data/bullet.txt"
                                   (get-source-dir))))

(defun two-bread ()
  (load-line-file (merge-pathnames "data/twobread.txt"
                                   (get-source-dir))))

(defun zilie ()
  (load-line-file (generate-path "data/zilian.txt")))

(defvar *tianx-key* "f07a432f84956febe20375736114244e")

(defun get-random-news (num)
  (web-get "api.tianapi.com"
           "world/index"
           :args `(("key" . ,*tianx-key*)
                   ("num" . ,num)
                   ("rand". 1))
           :jsonp t
           :parse-method nil))

(defun get-news (query num)
  (web-get "api.tianapi.com"
           "world/index"
           :args `(("key" . ,*tianx-key*)
                   ("num" . ,num)
                   ("word". ,query))
           :jsonp t
           :parse-method nil))

(defun handle-news (news)
  (if (= 200 (assoc-v news :code))
      (assoc-v news :newslist)
      (format t "error:~A~%" (assoc-v news :msg))))

(defun get-zaoan ()
  (web-get "api.tianapi.com"
           "zaoan/index"
           :args `(("key" . ,*tianx-key*))
           :jsonp t
           :parse-method nil))

(defun get-zuan (&optional (level "min"))
  "have two level:min and max"
  (cl-json:decode-json-from-string (web-get "api.zuanbot.com"
                                            "nmsl"
                                            :args `(("level" . ,level)))))

(defun get-chp ()
  (cl-json:decode-json-from-string (web-get "api.zuanbot.com"
                                            "chp")))

(defun get-du ()
  (cl-json:decode-json-from-string (web-get "api.zuanbot.com"
                                            "du")))

(defun handle-zbc (json)
  (when-bind (data (cdr (assoc :data json)))
    (cdr (assoc :text data))))

(defun soul-load ()
  (load-line-file (merge-pathnames "data/soulD.txt"
                                   (get-source-dir))))

(defun random-soul (souls)
  (elt souls
       (random-int-r (- (length souls) 1))))

(defun get-duanzi ()
  (cl-json:decode-json-from-string (web-post-json "www.yduanzi.com"
                                                  "duanzi/getduanzi")))

(defun handle-duanzi (duanzi)
  (when (assoc :success duanzi)
    (split-s (cdr (assoc :duanzi duanzi)) "<br>")))

(add-command "笑话"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                                    (get-random-text "xh")))
             "获取随机一条笑话")

(add-command "骚话"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                                    (get-random-text "sao")))
             "获取随机一条骚话")

(add-command "情话"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                                    (get-random-text "love")))
             "获取随机一条情话")

(add-command "让子弹飞一会"
             #'(lambda (sender args)
                 (dolist (line (bullet-fly-load))
                   (sleep 1)
                   (send-text (target-id sender) line))))

(add-command "我们联合"
             #'(lambda (sender args)
                 (dolist (line (two-bread))
                   (sleep 1)
                   (send-text (target-id sender) line))))

(add-command "名言"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (or (args-type args (list #'numberp))
                           (args-type args))
                       (dotimes (i (if (car args) (parse-integer (car args)) 1))
                         (sleep 1)
                         (let ((miyans (random-lst *mi-yan*)))
                           (let ((miyan (random-lst (cdr miyans))))
                             (if (listp miyan)
                                 (progn
                                   (send-text target (car miyans))
                                   (send-text-lst target miyan))
                                 (send-text target (format nil "~A --~A" miyan (car miyans)))))))
                       (send-text target "参数错误, 例子:伊蕾娜 名言 2 或者 伊蕾娜 名言")))))

(add-command "名言人物"
             #'(lambda (sender args)
                 (send-text-lst (target-id sender) (miyanlist))))

(add-command "人物名言"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list #'symbolp))
                       (let ((key (assoc-value *mi-yan* (car args))))
                         (if key
                             (dolist (i key)
                               (sleep 1)
                               (if (listp i)
                                   (send-text-lst target i)
                                   (send-text target i)))
                             (send-text target "没有找到这个人呢!")))
                       (send-text target "参数错误, 例子:陈睿 人物名言 缘之空")))))

(add-command "来句诗"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (let ((siju (get-random-sici "all")))
                     (format t "类型:~A~%" (assoc-value siju "category"))
                     (send-text target (assoc-value siju "content"))
                     (send-text-lst target (list (format nil "类型:~A" (assoc-value siju "category"))
                                                 (format nil "来自:~A" (assoc-value siju "origin"))
                                                 (format nil "作者:~A" (assoc-value siju "author"))))))))

(add-command "自恋"
             #'(lambda (sender args)
                 (dolist (line (zilie))
                   (let ((s-l (split-s line "|")))
                     (send-text (target-id sender)
                                (first s-l))
                     (sleep 1)
                     (send-text (target-id sender)
                                (second s-l)))
                   (sleep 2))))

(add-command "新闻"
             #'(lambda (sender args)
                 (if (or (args-type args (list 'numberp))
                         (args-type args (list 'numberp 'symbolp)))
                     (let ((target (target-id sender))
                           (num (parse-integer (first args)))
                           (query (second args)))
                       (let ((news (handle-news (if query
                                                    (get-news (purl:url-encode query) num)
                                                    (get-random-news num)))))
                         (send-text-lst target
                                        (mapcar #'(lambda (new)
                                                    (format nil
                                                            "~A ~A"
                                                            (assoc-value new "title")
                                                            (assoc-value new "ctime")))
                                                news))))
                     (send-text (target-id sender) "参数错误,例子:伊蕾娜 新闻 3 或者 伊蕾娜 新闻 3 疫苗"))))

(defun text-to-self (sender args mode)
  (handler-case (let ((zuan (format nil " ~A" (handle-zbc (case mode
                                                            (0 (get-zuan))
                                                            (1 (get-chp))
                                                            (2 (get-du))))))
                      (target (target-id sender)))
                  (if (group-id sender)
                      (send-at-text target
                                    (sender-id sender)
                                    zuan)
                      (send-text target
                                 zuan)))
    (error (e)
      (send-text (target-id sender) (format nil "~A" e)))))

(add-command "骂我"
             #'(lambda (sender args)
                 (text-to-self sender args 0)))

(add-command "夸我"
             #'(lambda (sender args)
                 (text-to-self sender args 1)))

(add-command "鸡汤"
             #'(lambda (sender args)
                 (text-to-self sender args 2)))

(add-command "舔狗日记"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                            (random-soul (soul-load)))))

(add-command "网易云段子"
             #'(lambda (sender args)
                 (send-text-lst (target-id sender)
                                (handle-duanzi (get-duanzi)))))

(defun repeat-sb (sender args)
    (handler-case (let ((zuan (format nil " ~A" (handle-zbc (get-zuan "max"))))
                        (target (target-id sender)))
                    (if (group-id sender)
                        (send-at-text target
                                      (sender-id sender)
                                      zuan)
                        (send-text target
                                   zuan)))
    (error (e)
      (send-text (target-id sender) (format nil "~A" e)))))

(add-command "sb"
             #'repeat-sb)

(add-command "傻逼"
             #'repeat-sb)

(defun time-format (timestamp)
  (format-timestring nil
                     timestamp
                     :format '((:year 4)
                               (:month 2)
                               (:day 2))))

(defun mean-world (&optional (time "20210826"))
  (let ((result (web-get "api.rosysun.cn" "60s"
                         :args `(("date" . ,time))
                         :jsonp t
                         :parse-method nil)))
    (assoc-v result :msg)))

(add-command "看世界"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (send-text (target-id sender)
                            (mean-world))))

(defun re-ban (item)
  (web-get "api.rosysun.cn" item :jsonp t :parse-method nil))

(add-command "知乎热榜"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (let ((data (assoc-v (re-ban "zhihu") :data))
                       (target (target-id sender)))
                   (if data
                       (dolist (i (subseq data 0 5))
                         (send-text-lst target
                                        (list (format nil
                                                      "标题:~A"
                                                      (assoc-v i :title))
                                              (format nil
                                                      "链接:~A"
                                                      (assoc-v i :url))))
                         (sleep 1))
                       (send-text target "无法获取")))))

(defun nbbhhsh-guess (text)
  (let ((result (web-post "lab.magiconch.com"
                    "api/nbnhhsh/guess"
                    :args `(("text" . ,text))
                    :jsonp t
                    :isbyte nil)))
    (format t "~A~%" result)
    (assoc-value (car result) "trans")))

(add-command "缩写"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list 'symbolp))
                       (let ((lst (nbbhhsh-guess (car args))))
                         (if lst
                             (send-text-lst target lst)
                             (send-text target "没有获取到")))
                       (send-text target "需要一个参数")))))

(in-package :cl-user)
