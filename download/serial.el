;; ID: f0c736a9-afec-3e3f-455c-40997023e130
(defun compose (&rest funs)
  "Return function composed of FUNS."
  (lexical-let ((lex-funs funs))
    (lambda (&rest args)
      (reduce 'funcall (butlast lex-funs)
	      :from-end t
	      :initial-value (apply (car (last lex-funs)) args)))))

;; Configuration

(defvar serial-password "password"
  "Secret password used in serial generation and checks.")

(defvar serial-code-len 12
  "Length of the code component of the serial number.")

(defvar serial-check-len 12
  "Length of the check component of the serial number.")

(defalias 'serial-hash 'sha1
  "Hash function for generating serial codes.")

(defalias 'serial-mixer 'concat
  "Function for mixing the password and the code part of the serial number.")

;; Functions

(defun serial-gen-code-hex ()
  "Generate a the code component of a new serial number."
  (substring (serial-hash (mapconcat (compose 'prin1-to-string 'random)
				     (make-list serial-code-len 256) ""))
	     0 serial-code-len))

(defun serial-gen-hex ()
  "Generate a hex-encoded serial number."
  (let* ((code (serial-gen-code-hex))
	 (check (substring (serial-hash (serial-mixer serial-password code))
			   0 serial-check-len)))
    (concat code check)))

(defun serial-check-hex (serial)
  "Return t if the given hex-encoded serial is valid."
  (let ((code (substring serial 0 serial-code-len))
	(check (substring serial (- serial-check-len))))
    (string= check (substring (serial-hash (serial-mixer serial-password code))
			      0 serial-check-len))))

;; Base64 encoding wrappers

(defun serial-hex-to-bytes (hex)
  "Convert hexadecimal string to string of bytes."
  (if (= 0 (length hex))
      ""
    (concat (char-to-string (string-to-number (substring hex 0 2) 16))
	    (serial-hex-to-bytes (substring hex 2)))))

(defun serial-gen ()
  "Generate a base64 encoded serial number."
  (base64-encode-string (serial-hex-to-bytes (serial-gen-hex))))

(defun serial-check (serial)
  "Return t if base64 serial number is valid."
  (serial-check-hex (mapconcat (apply-partially 'format "%02x")
			       (base64-decode-string serial) "")))

(serial-check-hex (serial-gen-hex))
(serial-check (serial-gen))
(serial-check "U8qni2D5nYzQ3CEz")
