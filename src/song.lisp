(defpackage :qqbot.song
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web))
(in-package :qqbot.song)

(defun parse-data (data &optional (data-name "result"))
  (let ((code (assoc-value data "code")))
    (if (= code 200)
        (assoc-value data data-name)
        (format t "error-song:~A~%" data))))

(defun search-song (songname &optional (limit 3))
  (parse-data (web-get "192.168.3.3:3000" "cloudsearch"
                       :args `(("keywords" . ,songname)
                               ("type" . 1)
                               ("limit" . ,limit))
                       :jsonp t)))


(defun get-info-song (result)
  (let ((len (assoc-value result "songCount"))
        (songs (assoc-value result "songs")))
    (format t "len:~A~%" len)
    (mapcar #'(lambda (song)
                (list (assoc "name" song :test #'string=)
                      (assoc "id" song :test #'string=)
                      (assoc "picUrl" (assoc-value song "al") :test #'string=)
                      `("person" . ,(assoc-value (car (assoc-value song "ar")) "name"))))
              songs)))

(defun get-mp3 (id)
  (parse-data (web-get "192.168.3.3:3000" "song/url" :args `(("id" . ,id)) :jsonp t) "data"))

(defun get-url (mp3)
  (list (assoc-value mp3 "url")))

(add-command "点歌"
             #'(lambda (sender args)
                 (cond ((args-type args (list #'numberp #'symbolp))
                        (let ((target (target-id sender))
                              (result (search-song (string-merges (cdr args)
                                                                  " ")
                                                   (first args))))
                          (if (and result
                                   (> (assoc-value result "songCount") 0))
                              (let ((songs (get-info-song result)))
                                (format t "aa~%")
                                (dolist (song songs)
                                  (format t "song:~A~%" song)
                                  (let ((mp3 (get-mp3 (assoc-value song "id"))))
                                    (when mp3
                                      (let ((url (car (get-url (car mp3)))))
                                        (send-music-share target
                                                          (assoc-value song "name")
                                                          (assoc-value song "person")
                                                          (format nil "https://music.163.com/#/song?id=~A" (assoc-value song "id"))
                                                          (assoc-value song "picUrl")
                                                          url))))))
                              (send-text target "没有搜索到这个歌曲呢!"))))
                       ((args-type args (list #'symbolp))
                        (let ((target (target-id sender))
                              (result (search-song (string-merges args
                                                                  " "))))
                          (if (and result
                                   (> (assoc-value result "songCount") 0))
                              (let ((songs (get-info-song result)))
                                (format t "aa~%")
                                (dolist (song songs)
                                  (format t "song:~A~%" song)
                                  (let ((mp3 (get-mp3 (assoc-value song "id"))))
                                    (when mp3
                                      (let ((url (car (get-url (car mp3)))))
                                        (send-music-share target
                                                          (assoc-value song "name")
                                                          (assoc-value song "person")
                                                          (format nil "https://music.163.com/#/song?id=~A" (assoc-value song "id"))
                                                          (assoc-value song "picUrl")
                                                          url))))))
                              (send-text target "没有搜索到这个歌曲呢!"))))
                       (t
                        (send-text (target-id sender) "参数错误"))))
             "网易云搜索歌曲 第一个参数为返回的歌曲数量, 剩下的参数是歌曲关键字")

(in-package :cl-user)
