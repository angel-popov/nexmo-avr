;;;; nexmo.asd

(asdf:defsystem #:nexmo
  :description "Describe nexmo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:jsown
               #:hunchentoot
               #:babel
               #:lparallel
;               #:cljwt
               #:UIOP)
  :serial t
  :components ((:file "package")
               (:file "vars")
               (:file "nexmo")
               (:file "handlers")
               ))
