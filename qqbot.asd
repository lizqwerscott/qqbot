(defsystem :qqbot
  :depends-on (:cl-ppcre
               :drakma
               :babel
               :jonathan
               :bordeaux-threads
               :purl
               :lquery
               :plump
               :cl-strings
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
               (:file "game")
               (:file "song")
               (:file "main")))
