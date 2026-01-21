;;;; slynk-hot-reload.asd - ASDF system for Slynk hot-reload module

(defsystem "slynk-hot-reload"
  :description "Hot reload module for Slynk/Sly"
  :author "Your Name"
  :license "MIT"
  :depends-on (:hunchentoot
               :bordeaux-threads
               :flexi-streams)
  :serial t
  :components ((:file "slynk-hot-reload")))
