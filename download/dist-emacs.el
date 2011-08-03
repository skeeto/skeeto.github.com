;;; dist-emacs.el --- Elisp distributed computing

;; Copyright (C) 2010 Christopher Wellons <mosquitopsu@gmail.com>
;; Pick any BSD-style license.

;;; Commentary:

;; This is meant to provide a little framework for distributing
;; arbitrary code for clients to execute for the purpose of
;; distributed computing. Code is signed by the password
;; (dist-password) so that clients aren't wide open for abuse. So
;; choose a good password.

;; It's not very fancy right now, but more of a proof-of-concept. It
;; should be able to handle disconnected clients. It should also
;; provide some better tools for make effective use of the
;; clients. Threading would have been nice to have here.

;;; Code:

;; This config section should basically be identical between server
;; and clients. Be sure to change this password to something other
;; than the defaults (rule of thumb: changing defaults increases
;; security).

(defvar dist-password "RingsOfSaturn"
  "The password used to sign code.")

(defvar dist-server-address "localhost"
  "Address of server that client will connect to.")

(defvar dist-server-port 6500
  "Address of server that client will connect to.")

;; Encoding and decoding

(defun dist-sign-sexp (password sexp)
  "Return signature of the given form."
  (sha1 (format "%s%s" password sexp)))

(defun dist-encode (sexp)
  "Prepare a sexp to be sent."
  (prin1-to-string (cons (dist-sign-sexp dist-password sexp) sexp)))

(defun dist-decode (str)
  "Decode form, checking the signature in the process."
  (let* ((cons (read str))
	 (sig (car cons))
	 (sexp (cdr cons)))
    (if (equal sig (dist-sign-sexp dist-password sexp))
	sexp
      'dist-bad-sig)))

;; Client

(defun dist-connect ()
  "Connect to the server and begin receiving instruction."
  (interactive)
  (make-network-process
   :name     "dist-emacs-client"
   :host     dist-server-address
   :service  dist-server-port
   :server   nil
   :family   'ipv4
   :filter   'dist-client))

(defun dist-client (proc sexp)
  "Execute properly signed instructions from the server."
  (process-send-string proc (prin1-to-string (funcall (dist-decode sexp)))))

(defun dist-disconnect ()
  "Disconnect from the server."
  (interactive)
  (when (process-status "dist-emacs-client")
    (delete-process "dist-emacs-client")))

(defun dist-bad-sig ()
  "Function to run when the signature is bad."
  (message "dist-emacs: bad signature!"))

;; Server

(defvar dist-clients ()
  "List of clients.")

(defun dist-start ()
  "Start the dist-emacs server and begin accepting clients."
  (interactive)
  (make-network-process
   :name     "dist-emacs-server"
   :service  dist-server-port
   :server   t
   :family   'ipv4
   :sentinel 'dist-sentinel
   :filter   'dist-server))

(defun dist-sentinel (proc stat)
  "Update status of client."
  (if (eq (process-status proc) 'open)
      (add-to-list 'dist-clients proc)
    (dist-rem-from-list 'dist-clients proc)))

(defun dist-server (proc string)
  "Receive return from client."
  (message string))

(defun dist-stop ()
  "Stop the dist-emacs server."
  (interactive)
  (when (process-status "dist-emacs-server")
    (delete-process "dist-emacs-server"))  
  (setq dist-clients ()))

;; Framework

(defun dist-dist (funcs)
  "Distribute functions to clients."
  (dolist (pair (mapcar* 'cons dist-clients funcs))
    (process-send-string (car pair)
			 (dist-encode (cdr pair)))))

;; Utility

(defun dist-rem-from-list (list el)
  "Opposite of add-to-list."
  (set list (remq el (symbol-value list))))
