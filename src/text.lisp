(defpackage :qqbot.text
  (:use :common-lisp :qqbot.bot :qqbot.web :qqbot.head :local-time)
  (:export
   :get-random-text
   :zaoan
   :wanan
   :history-today
   :history-today-s))
(in-package :qqbot.text)

(defparameter *mi-yan* nil)

(defun load-miyan ()
  (load-json-file (merge-pathnames "datas/data/名言.json"
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
  (load-line-file (merge-pathnames "datas/data/bullet.txt"
                                   (get-source-dir))))

(defun two-bread ()
  (load-line-file (merge-pathnames "datas/data/twobread.txt"
                                   (get-source-dir))))

(defun zilie ()
  (load-line-file (generate-path "datas/data/zilian.txt")))

(defvar *tian-address* "api.tianapi.com")
(defvar *tianx-key* "f07a432f84956febe20375736114244e")

(defun handle-tianx (data)
  (if (= 200 (assoc-value data "code"))
      (assoc-value data "newslist")
      (let ((errormsg (format nil
                             "[error][tianx]:~A"
                             (assoc-value data "msg"))))
        (format t "~A~%" errormsg)
        (error errormsg))))

(defun get-random-news (num)
  (web-get *tian-address*
           "world/index"
           :args `(("key" . ,*tianx-key*)
                   ("num" . ,num)
                   ("rand". 1))
           :jsonp t))

(defun get-news (query num)
  (web-get *tian-address*
           "world/index"
           :args `(("key" . ,*tianx-key*)
                   ("num" . ,num)
                   ("word". ,query))
           :jsonp t))

(defun handle-news (news)
  (handle-tianx news))

(defun get-zaoan ()
  (web-get *tian-address*
           "zaoan/index"
           :args `(("key" . ,*tianx-key*))
           :jsonp t))

(defun get-wanan ()
  (web-get *tian-address*
           "wanan/index"
           :args `(("key" . ,*tianx-key*))
           :jsonp t))

(defun handle-an (zaoan)
  (assoc-value (car (handle-tianx zaoan))
               "content"))

(defun zaoan ()
  (handle-an (get-zaoan)))

(defun wanan ()
  (handle-an (get-wanan)))

(defun get-today ()
  (format-timestring nil
                     (now-today)
                     :format '((:month 2)
                               (:day 2))))

(defun get-history-today (date)
  (web-get *tian-address*
           "lishi/index"
           :args `(("key" . ,*tianx-key*)
                   ("date" . ,date))
           :jsonp t))

(defun handle-history-today (history)
  (mapcar #'(lambda (item)
              (list (assoc-value item "title")
                    (assoc-value item "lsdate")))
          (handle-tianx history)))

(defun history-today (&optional (date (get-today)))
  (handle-history-today (get-history-today date)))

(defun append-n (lst &optional (result nil))
  (if lst
      (append-n (cdr lst)
                (append result (car lst)))
      result))

(defun n-append (lst &optional (n 2) (result nil))
  (if lst
      (if (<= (length lst) n)
          (n-append nil
                    n
                    (append result
                            (list (append-n lst))))
          (n-append (subseq lst n)
                    n
                    (append result
                            (list (append-n (subseq lst 0 n))))))
      result))

(defun history-today-s (sender &key (number 5) (page 0))
  (let ((data (history-today)))
    (when data
      (let ((pages (n-append data number)))
        (if (= page 0)
          (dolist (i pages)
            (send-text-lst sender
                           i)
            (sleep 1))
          (dotimes (i (min (length pages)
                           page))
            (send-text-lst sender
                           (elt pages i))
            (sleep 1)))))))

(add-command "历史上的今天"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (history-today-s (target-id sender))))

(defun get-djt ()
  (web-get *tian-address*
           "dujitang/index"
           :args `(("key" . ,*tianx-key*))
           :jsonp t))

(defun handle-djt (data)
  (assoc-value (car (handle-tianx data))
               "content"))

(add-command "毒鸡汤"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (send-text (target-id sender)
                            (handle-djt (get-djt)))))

(defun soul-load ()
  (load-line-file (merge-pathnames "datas/data/soulD.txt"
                                   (get-source-dir))))

(defun random-soul (souls)
  (elt souls
       (random-int-r (- (length souls) 1))))

(defun get-duanzi ()
  (web-get "www.yduanzi.com"
           "duanzi/getduanzi"
           :jsonp t))

(defun handle-duanzi (duanzi)
  (when (assoc-value duanzi "success")
    (split-s (assoc-value duanzi "duanzi") "<br>")))

(add-command "笑话"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                                    (get-random-text "joke")))
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
                       (send-text target (format nil "参数错误, 例子:~A 名言 2 或者 ~A 名言" (get-bot-name)))))))

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
                                                    (get-news (quri:url-encode query) num)
                                                    (get-random-news num)))))
                         (send-text-lst target
                                        (mapcar #'(lambda (new)
                                                    (format nil
                                                            "~A ~A"
                                                            (assoc-value new "title")
                                                            (assoc-value new "ctime")))
                                                news))))
                     (send-text (target-id sender)
                                (format nil "参数错误,例子:~A 新闻 3 或者 ~A 新闻 3 疫苗" (get-bot-name))))))

(add-command "舔狗日记"
             #'(lambda (sender args)
                 (send-text (target-id sender)
                            (random-soul (soul-load)))))

(add-command "网易云段子"
             #'(lambda (sender args)
                 (send-text-lst (target-id sender)
                                (handle-duanzi (get-duanzi)))))

(defun time-format (timestamp)
  (format-timestring nil
                     timestamp
                     :format '((:year 4)
                               (:month 2)
                               (:day 2))))

(defun mean-world (&optional (time "20210826"))
  (let ((result (web-get "api.rosysun.cn" "60s"
                         :args `(("date" . ,time))
                         :jsonp t)))
    (assoc-value result "msg")))

(add-command "看世界"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (send-text (target-id sender)
                            (mean-world))))

(defun re-ban (item)
  (web-get "api.rosysun.cn" item :jsonp t))

(add-command "知乎热榜"
             #'(lambda (sender args)
                 (declare (ignore args))
                 (let ((data (assoc-value (re-ban "zhihu") "data"))
                       (target (target-id sender)))
                   (if data
                       (dolist (i (subseq data 0 5))
                         (send-text-lst target
                                        (list (format nil
                                                      "标题:~A"
                                                      (assoc-value i "title"))
                                              (format nil
                                                      "链接:~A"
                                                      (assoc-value i "url"))))
                         (sleep 1))
                       (send-text target "无法获取")))))

(defun nbbhhsh-guess (text)
  (let ((result (web-post "https://lab.magiconch.com/api/nbnhhsh/guess"
                          :args `(("text" . ,text))
                          :jsonp t)))
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

(defun homo (number)
  (web-get "124.222.100.66:1145"
           "homo"
           :args `(("number" . ,number))))

(add-command "恶臭论证"
             #'(lambda (sender args)
                 (let ((target (target-id sender))
                       (query (car args)))
                   (if query
                       (cond ((string= "⑨" query)
                              (send-text-lst target
                                             (list "11-4-5+1-4"
                                                   "我感觉你真是像⑨一样聪明(≧▽≦)")))
                             ((or (string= "114514" query)
                                  (string= "1919810" query))
                              (send-text target
                                         "这么臭的数字还有论证的必要吗？ baka ≧▽≦"))
                             (t
                              (let ((number (read-from-string query)))
                                (if (numberp number)
                                    (send-text target
                                               (homo number))
                                    (send-text target
                                               "不要让我论证这么奇怪的东西呀！")))))
                       (send-text target
                                  "请输入要论证的数字baka")))))

(defun lajiSort (lj)
  (let ((result (web-get "api.vvhan.com"
                         "api/la.ji"
                         :args `(("lj" . ,lj))
                         :jsonp t)))
    (when (and result
               (assoc-value result "success"))
      result)))

(add-command "垃圾分类"
             #'(lambda (sender args)
                 (let ((target (target-id sender)))
                   (if (args-type args (list 'symbolp))
                       (let ((res (lajisort (first args))))
                         (if res
                             (send-text target
                                        (format nil "~A是~A."
                                                (assoc-value res "name")
                                                (assoc-value res "sort")))
                             (send-text target
                                        "发送错误啦!")))
                       (send-text target "请输入要分类的垃圾!!")))))

(in-package :cl-user)
