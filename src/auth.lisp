(defpackage :honami.auth
  (:use :cl :hunchentoot :cl-json :uuid :sqlite :ironclad :honami.database)
  (:export :start-server :stop-server :valid-token-p :verify-credentials :generate-token
           :check-permission :grant-permission :revoke-permission :add-user :hash-password
	   :generate-salt))
(in-package :honami.auth)

(defun generate-salt ()
  (byte-array-to-hex-string (random-data 16)))

(defun hash-password (password salt)
  (byte-array-to-hex-string
   (digest-sequence :sha256
                    (flexi-streams:string-to-octets (concatenate 'string password salt)))))

(defun add-user (username password)
  (with-db
    (let ((salt (generate-salt)))
      (execute-non-query *db*
        "INSERT OR IGNORE INTO users (username, password_hash, salt) VALUES (?, ?, ?)"
        username (hash-password password salt) salt)
      (not (null (execute-single *db* "SELECT id FROM users WHERE username = ?" username))))))

(defun verify-credentials (username password)
  (with-db
    (let ((user-data (execute-single *db* "SELECT password_hash, salt FROM users WHERE username = ?" username)))
      (when user-data
        (string= (first user-data) (hash-password password (second user-data)))))))

(defun generate-token (username redirect-uri)
  (with-db
    (let ((token (make-v4-uuid))
          (expiry (+ (get-universal-time) 3600)))
      (execute-non-query *db*
        "INSERT INTO tokens (token, username, expiry) VALUES (?, ?, ?)"
        (format nil "~a" token) username expiry)
      (values token expiry redirect-uri))))

(defun valid-token-p (token)
  (with-db
    (let ((result (first (execute-to-list *db* "SELECT username, expiry FROM tokens WHERE token = ?" token))))
      (when result
        (let ((username (first result)) (expiry (second result)))
          (and (< (get-universal-time) expiry) username))))))

(defun check-permission (username resource-type resource-uid action)
  (with-db
    (or (execute-single *db*
          "SELECT 1 FROM permissions WHERE resource_type = ? AND resource_uid = ? AND owner = ? AND access IN (?, 'write')"
          resource-type resource-uid username action)
        (execute-single *db*
          "SELECT 1 FROM permissions WHERE resource_type = ? AND resource_uid = ? AND (grantee = ? OR grantee = 'ALL') AND access IN (?, 'write')"
          resource-type resource-uid username action))))

(defun grant-permission (owner grantee resource-type resource-uid access)
  (with-db
    (execute-non-query *db*
      "INSERT OR REPLACE INTO permissions (resource_type, resource_uid, owner, grantee, access) VALUES (?, ?, ?, ?, ?)"
      resource-type resource-uid owner grantee access)))

(defun revoke-permission (owner grantee resource-type resource-uid)
  (with-db
    (execute-non-query *db*
      "DELETE FROM permissions WHERE resource_type = ? AND resource_uid = ? AND owner = ? AND grantee = ?"
      resource-type resource-uid owner grantee)))

(define-easy-handler (validate-token :uri "/validate-token" :default-request-type :get) (token)
  (setf (hunchentoot:content-type*) "application/json")
  (with-db
    (let ((username (valid-token-p token)))
      (if username
          (encode-json-to-string `((:status . "success") (:username . ,username)))
          (encode-json-to-string `((:status . "error") (:message . "Invalid or expired token")))))))

(define-easy-handler (root :uri "/" :default-request-type :get) ()
  (with-db
    (let ((auth-token (cookie-in "auth_token")))
      (if (valid-token-p auth-token)
          (redirect "/dashboard")
          (redirect "/login-page")))))

(defclass my-acceptor (hunchentoot:easy-acceptor) ())

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor my-acceptor) request)
  "Dispatch to defined handlers first; redirect unmatched URIs."
  (or (call-next-method)
      (with-db
        (let ((auth-token (cookie-in "auth_token")))
          (if (valid-token-p auth-token)
              (redirect "/dashboard")
              (redirect "/login"))))))

(defvar *server* (make-instance 'my-acceptor :port 8443))

(defun start-server ()
  (with-db
    (initialize-db)
    (hunchentoot:start *server*)
    (format t "Server started on http://localhost:8443~%")))

(defun stop-server ()
  (hunchentoot:stop *server*)
  (when *db*
    (disconnect *db*)
    (setf *db* nil)))
