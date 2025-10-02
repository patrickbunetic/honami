(defpackage :honami.database
  (:use :cl :sqlite)
  (:export :*db* :with-db :initialize-db))
(in-package :honami.database)

(defconstant +database-create-users-table+
  "CREATE TABLE IF NOT EXISTS users (
        id INTEGER PRIMARY KEY,
        username TEXT UNIQUE,
        password_hash TEXT,
        salt TEXT)")

(defconstant +database-create-tokens-table+
  "CREATE TABLE IF NOT EXISTS tokens (
        token TEXT PRIMARY KEY,
        username TEXT,
        expiry INTEGER,
        FOREIGN KEY (username) REFERENCES users(username))")

(defconstant +database-create-permissions-table+
  "CREATE TABLE IF NOT EXISTS permissions (
        id INTEGER PRIMARY KEY,
        resource_type TEXT,
        resource_uid TEXT,
        owner TEXT,
        grantee TEXT,
        access TEXT,
        UNIQUE (resource_type, resource_uid, grantee),
        FOREIGN KEY (owner) REFERENCES users(username))")

(defparameter *db-path* "honami.db") ; TODO: standardize location
(defvar *db* nil)

(defun ensure-db-connection ()
  "Ensure *db* is connected; reconnect if necessary."
  (when (or (null *db*)
            (handler-case
                (progn
                  (execute-single *db* "SELECT 1")
                  nil) 
              (error () t)))
    (handler-case
        (progn
          (when *db* (disconnect *db*))
          (setf *db* (connect *db-path*)))
      (error (e)
        (error "Failed to connect to database ~a: ~a" *db-path* e)))))

(defmacro with-db (&body body)
  "Wrap SQLite operations with connection check."
  `(progn
     (ensure-db-connection)
     ,@body))

(defun initialize-db ()
  (with-db
    (execute-non-query *db* +database-create-users-table+)
    (execute-non-query *db* +database-create-tokens-table+)
    (execute-non-query *db* +database-create-permissions-table+)))
