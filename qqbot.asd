(defsystem "qqbot"
  :version "0.1.0"
  :author "Lizqwer scott"
  :depends-on (:cl-ppcre
               :dexador
               :cl-transmission
               :babel
               :bordeaux-threads
               :local-time
               :quri
               :str
               :cl-strings
               :yason
               :jonathan
               :cl-schedule
               :cl-async
               :patron
               :random-state)
  :serial t
  :components ((:module "src"
                :components
                ((:file "head")
                 (:file "web")
                 (:file "bot")
                 (:file "task")
                 (:file "text")
                 (:file "question")
                 (:file "picture")
                 (:file "bt")
                 (:file "card")
                 (:module "game"
                  :depends-on ("head" "bot")
                  :serial t
                  :components ((:file "miyu")))
                 (:file "song")
                 (:file "minecraft")
                 (:file "wiki")
                 (:file "moyu")
                 (:file "health")
                 (:file "class-schedule")
                 (:file "main")))))
