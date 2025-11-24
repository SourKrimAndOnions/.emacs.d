;;;; slynk-hot-reload.asd

(defsystem "slynk-hot-reload"
  :description "Hot reload backend for Sly - Server-Sent Events endpoint for browser refresh"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:hunchentoot
               :bordeaux-threads
               :flexi-streams)
  :components ((:file "hot-reload")))
