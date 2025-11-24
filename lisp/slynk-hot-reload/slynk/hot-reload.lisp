;;;; hot-reload.lisp

(defpackage #:slynk-hot-reload
  (:use #:cl)
  (:local-nicknames (#:ht #:hunchentoot)
                    (#:bt #:bordeaux-threads))
  (:export
   ;; Main API
   #:setup
   #:script
   #:reload

   ;; Configuration
   #:*dev-mode*
   #:*endpoint-path*))

(in-package #:slynk-hot-reload)

;;; Configuration
(defvar *dev-mode* t
  "When T, hot reload is enabled. Set to NIL in production.")

(defvar *endpoint-path* "/_dev/reload"
  "SSE endpoint path for hot reload.")

;;; Internal state
(defvar *reload-version* 0)
(defvar *reload-lock* (bt:make-lock "reload-lock"))
(defvar *reload-condvar* (bt:make-condition-variable :name "reload-cv"))

;;; Public API

(defun reload ()
  "Trigger a browser reload. Call this after code changes."
  (bt:with-lock-held (*reload-lock*)
    (incf *reload-version*)
    (bt:condition-notify *reload-condvar*))
  (format t "~&; Hot reload triggered (v~A)~%" *reload-version*)
  *reload-version*)

(defun setup (&key (acceptor nil))
  "Register hot reload SSE endpoint with Hunchentoot.
If ACCEPTOR is provided, endpoint will only work for that acceptor.
Otherwise, it's added to the global dispatch table."
  (declare (ignore acceptor)) ; For future per-acceptor support
  (pushnew (ht:create-prefix-dispatcher *endpoint-path* 'sse-handler)
           ht:*dispatch-table*
           :test #'equal)
  (format t "~&; Hot reload endpoint registered at ~A~%" *endpoint-path*))

(defun script ()
  "Returns JavaScript code for hot reload. Include in your HTML <head>."
  (if *dev-mode*
      (format nil "(function() {
         const es = new EventSource('~A');
         es.addEventListener('reload', () => location.reload());
         es.addEventListener('connected', (e) => console.log('Hot reload ready:', e.data));
         es.onerror = (e) => console.log('Hot reload connection error, reconnecting...', e);
       })();" *endpoint-path*)
      ""))

;;; Internal implementation

(defun sse-handler ()
  "SSE endpoint handler for hot reload events."
  (setf (ht:content-type*) "text/event-stream"
        (ht:header-out :cache-control) "no-cache"
        (ht:header-out :access-control-allow-origin) "*")
  (let* ((binary-stream (ht:send-headers))
         (stream (flexi-streams:make-flexi-stream binary-stream
                                                  :external-format :utf-8))
         (last-version *reload-version*))
    (format stream "event: connected~%data: v~A~%~%" last-version)
    (force-output stream)
    (handler-case
        (loop
          (bt:with-lock-held (*reload-lock*)
            (bt:condition-wait *reload-condvar* *reload-lock* :timeout 2))
          (when (/= last-version *reload-version*)
            (setf last-version *reload-version*)
            (format stream "event: reload~%data: v~A~%~%" last-version)
            (force-output stream))
          (format stream ":ping~%~%")
          (force-output stream))
      (error () nil))))
