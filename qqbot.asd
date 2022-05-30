(defsystem :qqbot
  :depends-on (:cl-ppcre
               :dexador
               :drakma
               :cl-transmission
               :babel
               :bordeaux-threads
               :local-time
               :quri
               :purl
               :lquery
               :plump
               :str
               :cl-strings
               :yason
               :jonathan
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
               (:file "minecraft")
               (:file "wiki")
               (:file "moyu")
               (:file "main")))
