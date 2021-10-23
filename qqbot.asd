(defsystem :qqbot
  :depends-on (:cl-ppcre
               :drakma
               :babel
               :jonathan
               :bordeaux-threads
               :local-time
               :purl
               :lquery
               :plump
               :cl-strings
               :cl-schedule
               :cl-ppcre
               :cl-async
               :patron
               :random-state)
  :serial t
  :components ((:file "package")
               (:file "head")
               (:file "web")
               (:file "task")
               (:file "bot")
               (:file "text")
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
