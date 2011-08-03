;;; ==========================================
;;; uuid.el --- Cross-platform UUID generation
;;; ==========================================
;;;
;;; :Author: Martin Blais <blais@furius.ca> (and others)
;;; :Date:   2005
;;; :Abstract:
;;;
;;;    UUID generation from within Emacs.
;;;
;;; Description
;;; ===========
;;;
;;; Provides a central file for definitions of UUID generation codes grabbed
;;; over USENET, mailing-lists and in other places.
;;;
;;; Note: some of the UUIDs geenerated by this code are weak.
;;;
;;; Download
;;; ========
;;;
;;; Click `Here <uuid.el>`_ for download.
;;;
;;; END
;;;
;;; Code:
;;;

;; From Martin Blais.
(defun uuid-priv-call-process-get-output (program &optional infile bufname &rest args)
  "Calls a process and returns a string with the output of that
process. Attempts to get rid of the buffer afterwards."
  (let* ((bufname (or bufname "*call-process-get-output*"))
         (buf (get-buffer-create bufname))
         output prepoint)
    (save-excursion
      (set-buffer buf)
      (setq prepoint (point))
      (apply
       'call-process (append (list program infile bufname) args))
      (setq output (buffer-substring prepoint (point-max)))
      (kill-buffer buf))
    output))

;; Commentaires de Stefan (FIXME TODO):
;;
;; (with-current-buffer BUF &rest BODY) est préférable
;; à save-excusrion+set-buffer.
;; 
;;    (apply 'call-process (append (list program infile bufname) args))
;; ==
;;    (apply 'call-process program infile bufname args)
;; 
;; Ceci dit, je crois que j'utiliserais qqch du genre:
;; 
;;  (with-output-to-string
;;    (apply 'call-process program infile standard-output args))
;; 
;; ou sinon, j'utiliserais with-temp-buffer.



;; From Martin Blais.
(defun uuid-uuidgen ()
  "Return a newly generated UUID. This uses the external program uuidgen."
  (downcase
   (substring
    (uuid-priv-call-process-get-output "uuidgen" nil "*uuidgen*") 0 -1)))

;; From Jesper Harder.
(defun uuid-simple ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s"
			(user-uid)
			(emacs-pid)
			(system-name)
			(user-full-name)
			user-mail-address
			(current-time)
			(random)
			(recent-keys)))))
    (format "%s-%s-%s-%s-%s"
	    (substring s 0 8)
	    (substring s 8 12)
	    (substring s 12 16)
	    (substring s 16 20)
	    (substring s 20 32)) ))

;; From Jesper Harder.
(defun uuid-random ()
  "Return a newly generated UUID. This Generates a version 4 UUID."
  (let ((bytes (uuid-random-bytes)))
    (setf (nth 7 bytes)
          (logior #B01000000 (logand #B01111111 (nth 7 bytes))))
    (setf (nth 8 bytes)
          (logior #B01000000 (logand #B01001111 (nth 8 bytes))))
    (apply 'format
     "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
     bytes)))

;; From Jesper Harder and then Kai Grossjohann
(defun uuid-random-bytes ()
  "Return a list of 16 random bytes."
  (if (file-readable-p "/dev/urandom")
      (let ((coding-system-for-read 'binary))
        (mapcar 'identity
                (substring
                 (string-as-unibyte
                  (shell-command-to-string
                   "dd count=16 bs=1 < /dev/urandom"))
                 0 16)))
    (mapcar 'random (make-list 16 255))))

;; Set (uuid) as an appropriate alias, depending on the platform.
(defalias 'uuid (if (executable-find "uuidgen") 'uuid-uuidgen 'uuid-random))

(provide 'uuid)

;;; uuid.el ends here
