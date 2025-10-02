(defpackage :honami.login
  (:use :cl :hunchentoot :cl-who :sqlite :cl-json :honami.auth :honami.database))
(in-package :honami.login)

(defun render-login-page (redirect-uri username)
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title "Login or Register")
      (:style "body { font-family: 'Segoe UI', Arial, sans-serif; margin: 0; padding: 20px; background-color: #f5f5f5; color: #333; }
               .container { max-width: 800px; margin: 0 auto; padding: 20px; background-color: #fff; border-radius: 8px; box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1); }
               h1, h2, h3 { color: #2c3e50; }
               input, select, textarea { width: 100%; padding: 10px; margin: 10px 0; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box; }
               button { padding: 10px 20px; background-color: #3498db; color: #fff; border: none; border-radius: 4px; cursor: pointer; }
               button:hover { background-color: #2980b9; }
               .error { color: #e74c3c; }"
	      ))
     (:body
      (:div :class "container"
            (:h2 "Welcome")
            (:div :id "login-form-container"
                  (:form :id "login-form" :method "post" :action "/login"
                         (:input :type "hidden" :name "redirect-uri" :value (if redirect-uri redirect-uri "/dashboard"))
                         (:label :for "username" "Username:")
                         (:input :type "text" :id "username" :name "username")
                         (:label :for "password" "Password:")
                         (:input :type "password" :id "password" :name "password")
                         (:button :type "submit" "Log In"))
                  (:p (:a :href "/register-page" "Create Account")))
            (:p :id "feedback" :class "error" ""))))))

(defun render-login-error (message redirect-uri)
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "Login Failed"))
     (:body
      (:div :class "container"
            (:h2 "Login Failed")
            (:p (str message))
            (:a :href (format nil "/login-page?redirect-uri=~a" (or redirect-uri "/dashboard")) "Try Again"))))))

(defun render-register-page ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title "Register")
      (:style "body { font-family: 'Segoe UI', Arial, sans-serif; margin: 0; padding: 20px; background-color: #f5f5f5; color: #333; }
               .container { max-width: 800px; margin: 0 auto; padding: 20px; background-color: #fff; border-radius: 8px; box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1); }
               h1, h2, h3 { color: #2c3e50; }
               input, select, textarea { width: 100%; padding: 10px; margin: 10px 0; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box; }
               button { padding: 10px 20px; background-color: #3498db; color: #fff; border: none; border-radius: 4px; cursor: pointer; }
               button:hover { background-color: #2980b9; }
               .error { color: #e74c3c; }"))
     (:body
      (:div :class "container"
            (:h2 "Register")
            (:form :method "post" :action "/register"
                   (:label :for "username" "Username:")
                   (:input :type "text" :id "username" :name "username")
                   (:label :for "password" "Password:")
                   (:input :type "password" :id "password" :name "password")
                   (:button :type "submit" "Register"))
            (:p (:a :href "/login-page" "Back to Login"))
            (:p :id "feedback" :class "error" ""))))))

(defun render-register-error (message)
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "Registration Failed"))
     (:body
      (:div :class "container"
            (:h2 "Registration Failed")
            (:p (str message))
            (:a :href "/login-page" "Back to Login"))))))

(define-easy-handler (login-page :uri "/login-page") ((redirect-uri :request-type :get))
  (setf (hunchentoot:content-type*) "text/html")
  (honami.database:with-db
    (let ((username (honami.auth:valid-token-p (cookie-in "auth_token"))))
      (render-login-page redirect-uri username))))

(define-easy-handler (login :uri "/login" :default-request-type :post) (username password redirect-uri)
  (honami.database:with-db
    (let ((user-data (first (execute-to-list honami.database:*db* "SELECT password_hash, salt FROM users WHERE username = ?" username))))
      (if user-data
          (let ((stored-hash (first user-data))
                (salt (second user-data)))
            (if (string= stored-hash (hash-password password salt))
                (multiple-value-bind (token expiry redirect) (generate-token username redirect-uri)
                  (declare (ignore expiry))
                  (set-cookie "auth_token" :value token :path "/" :max-age 3600 :secure t)
                  (redirect (or redirect-uri "/dashboard")))
                (progn
                  (setf (hunchentoot:content-type*) "text/html")
                  (render-login-error "Invalid credentials" redirect-uri))))
          (progn
            (setf (hunchentoot:content-type*) "text/html")
            (render-login-error "User not found" redirect-uri))))))

(define-easy-handler (register-page :uri "/register-page" :default-request-type :get) ()
  (setf (hunchentoot:content-type*) "text/html")
  (render-register-page))

(define-easy-handler (register :uri "/register" :default-request-type :post) (username password)
  (honami.database:with-db
    (cond
      ((or (string= username "") (string= password ""))
       (setf (hunchentoot:content-type*) "text/html")
       (render-register-error "Username and password are required"))
      ((not (ppcre:scan "^[a-zA-Z0-9_-]{3,20}$" username))
       (setf (hunchentoot:content-type*) "text/html")
       (render-register-error "Username must be 3-20 alphanumeric characters, underscores, or hyphens"))
      ((execute-single honami.database:*db* "SELECT 1 FROM users WHERE username = ?" username)
       (setf (hunchentoot:content-type*) "text/html")
       (render-register-error "Username already taken"))
      (t
       (add-user username password)
       (redirect "/login-page")))))
