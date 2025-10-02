(defpackage :honami.dashboard
  (:use :cl :hunchentoot :cl-who :sqlite :drakma :local-time :honami.auth :honami.database)
  (:shadow :cookie-value :parameter-error :*header-stream* :cookie-domain :cookie-name :cookie-path :cookie-expires :url-encode)
  (:export :render-page))
(in-package :honami.dashboard)

(defun render-page (title content)
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title (str title))
      (:link :rel "stylesheet" :href "/styles.css"))
     (:body
      ;; TODO: Sidebar navigation (placeholder for now)
      (:div :class "sidebar"
            (:a :href "/contacts" :class "nav-button" :title "Contacts" "C")
            (:a :href "/calendar" :class "nav-button" :title "Calendar" "E")
            (:a :href "/journals" :class "nav-button" :title "Journals" "J")
            (:a :href "/todos" :class "nav-button" :title "To-Do" "T")
            (:a :href "/settings" :class "nav-button" :title "Settings" "S"))
      ;; TODO: Main content area
      (:div :class "content"
            (str content))))))

(defun render-dashboard (user)
  (render-page "Dashboard"
               (with-html-output-to-string (*standard-output*)
                 (:h1 "Welcome, " (str user))
                 (:p "This is your dashboard. Customize it in settings!"))))

(define-easy-handler (dashboard :uri "/dashboard" :default-request-type :get) ()
  (setf (hunchentoot:content-type*) "text/html")
  (honami.database:with-db
    (let ((auth-token (cookie-in "auth_token")))
      (if (honami.auth:valid-token-p auth-token)
          (let* ((username (honami.auth:valid-token-p auth-token)))
            (render-dashboard username))
          (redirect "/login-page?redirect-uri=/dashboard")))))
