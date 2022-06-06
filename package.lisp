(in-package :cl-user)

(defpackage :qqbot.head
  (:import-from :jonathan :to-json)
  (:import-from :uiop :run-program)
  (:use :common-lisp :random-state :yason :babel :str :local-time)
  (:export
   :*patron*
   :run-shell
   :random-int-r
   :assoc-value
   :assoc-v

   :split-s
   :string-merge
   :string-merges
   :bits-to-json

   :get-directory
   :directoryp
   :list-directory
   :make-next-dir

   :load-line-file
   :load-json-file
   :save-json-file
   :lst-line-string
   :get-source-dir
   :get-data-dir
   :generate-path
   :save-l-picture
   :save-picture-url
   :when-bind
   :last1
   :to-json-a))

(defpackage :qqbot.web
  (:import-from :quri :make-uri)
  (:use :common-lisp :qqbot.head :babel :yason)
  (:export
   :generate-url
   :web-post-json
   :web-post
   :web-post-upload

   :web-get
   :web-get-url))

(defpackage :qqbot.bot
  (:use :common-lisp :qqbot.head :babel :qqbot.web :patron :yason :local-time :str)
  (:export
   :recive-picture
   :recive-picture-off

   :people
   :make-people
   :people-id
   :people-name
   :get-master
   :is-master
   :add-admin
   :remove-admin
   :is-admin
   :changep
   :list-admin
   :save-admin
   :load-admin

   :get-group-list
   :get-group-member

   :verify :bind :release
   :mute-group-member
   :gmessage-text
   :gmessage-picture
   :gmessage-at
   :send-message
   :send-text
   :send-text-lst
   :send-picture
   :send-local-picture
   :send-picture-and-text
   :send-picture-and-text-lst
   :send-music-share
   :send-json
   :send-at
   :send-at-text

   :sender-groupp
   :sender-id
   :sender-name
   :group-id
   :group-name
   :target-id
   :get-text-message-chain

   :add-command
   :remove-command

   :add-mode-command
   :remove-mode-command
   :active-mode
   :deactive-mode

   :fetch-last-message
   :handle-message
   :get-all-command
   :run
   :args-type))

(defpackage :qqbot.task
  (:use :common-lisp :qqbot.head :qqbot.bot :local-time :patron)
  (:export
   :task-name
   :task-time
   :task-func
   :task-runp

   :add-task
   :remove-task
   :start-task
   :stop-task
   :run-tasks

   :add-start-task
   :remove-start-task
   :run-start-tasks

   :get-time
   :get-time-range
   :time=
   :time-mintue=
   :time-in

   :is-reset
   :reset-task-time))

(defpackage :qqbot.text
  (:use :common-lisp :qqbot.bot :qqbot.web :qqbot.head :local-time)
  (:export
   :get-random-text
   :zaoan
   :wanan
   :history-today
   :history-today-s))

(defpackage :qqbot.question
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web :yason)
  (:export))

(defpackage :qqbot.picture
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web :patron :babel :yason)
  (:export
   :get-random-picture
   :save-picture
   :get-pixiv-pictures))

(defpackage :qqbot.bt
  (:use :common-lisp :qqbot.bot :qqbot.head :cl-transmission)
  (:export))

(defpackage :qqbot.card
  (:import-from :cl-ppcre :scan-to-strings :regex-replace)
  (:use :common-lisp :qqbot.bot :qqbot.head)
  (:export
   :handle-code
   :load-cards
   :list-cards
   :draw-card))

(defpackage :qqbot.game
  (:use :common-lisp :qqbot.bot :qqbot.head :str)
  (:export))

(defpackage :qqbot.song
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web))

(defpackage :qqbot.minecraft
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web))

(defpackage :qqbot.wiki
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web :yason))

(defpackage :qqbot.moyu
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web)
  (:export
   :moyu))

(defpackage :qqbot.health
  (:use :cl :qqbot.head :qqbot.web :qqbot.bot :qqbot.task)
  (:export
   :load-health
   :health-up-all))

(defpackage :qqbot
  (:import-from :bordeaux-threads :make-thread)
  (:use :common-lisp :qqbot.head :qqbot.bot :qqbot.web :qqbot.card :qqbot.task :qqbot.text :qqbot.moyu :local-time :cl-schedule :patron :cl-async)
  (:export :start))
