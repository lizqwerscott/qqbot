(defsystem :qqbot
  :depends-on (:cl-ppcre
               :drakma
               :cl-transmission
               :babel
               :jonathan
               :bordeaux-threads
               :local-time
               :purl
               :lquery
               :plump
               :cl-strings
               :cl-json
               :cl-schedule
               :cl-async
               :patron
               :random-state)
  :serial t
  :components ((:file "package")
               (:file "head")
               (:file "web")
               (:file "bot")
               (:file "task")
               (:file "text")
               (:file "question")
               (:file "picture")
               (:file "bt")
               (:file "card")
               (:module "game"
                :depends-on ("package" "head" "bot")
                :serial t
                :components ((:file "els")
                             (:file "miyu")))
               (:file "song")
               (:file "weixgzh")
               (:file "main")))
