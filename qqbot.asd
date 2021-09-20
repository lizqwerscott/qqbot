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
               :random-state)
  :serial t
  :components ((:file "package")
               (:file "head")
               (:file "web")
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
               (:file "task")
               (:file "main")))
