(in-package :cl-user)

(defpackage :qqbot.head
  (:use :common-lisp :random-state :jonathan :babel)
  (:export
   :run-shell
   :random-int-r
   :assoc-value
   :split-s
   :string-merge
   :string-merges
   :bits-to-json
   :list-directory
   :load-line-file
   :load-json-file
   :save-json-file
   :lst-line-string
   :get-source-dir))

(defpackage :qqbot.web
  (:use :common-lisp :drakma :qqbot.head :babel :jonathan)
  (:export
   :generate-url
   :web-post
   :web-get))

(defpackage :qqbot.bot
  (:use :common-lisp :drakma :qqbot.head :babel :qqbot.web)
  (:export
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

   :verify :bind :release
   :mute-group-member
   :gmessage-text
   :gmessage-picture
   :send-message
   :send-text
   :send-text-lst
   :send-picture
   :send-music-share
   :send-json
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
   :handle-message
   :get-all-command
   :run
   :args-type))

(defpackage :qqbot.text
  (:use :common-lisp :qqbot.bot :qqbot.web :qqbot.head)
  (:export
   :get-random-text))

(defpackage :qqbot.picture
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web)
  (:export
   :get-random-picture
   :save-picture
   :get-pixiv-pictures))

(defpackage :qqbot.bt
  (:use :common-lisp :qqbot.bot :drakma :qqbot.head)
  (:export
   :searchBt))

(defpackage :qqbot.card
  (:use :common-lisp :qqbot.bot :qqbot.head :cl-ppcre)
  (:export
   :handle-code
   :load-cards
   :list-cards
   :draw-card))

(defpackage :qqbot.game
  (:use :common-lisp :qqbot.bot :qqbot.head)
  (:export))

(defpackage :qqbot.song
  (:use :common-lisp :qqbot.bot :qqbot.head :qqbot.web))

(defpackage :qqbot
  (:use :common-lisp :qqbot.head :qqbot.bot :bordeaux-threads :qqbot.card)
  (:export :start))
