;; Copyright (c) 2007 Christopher Wellons <mosquitopsu@gmail.com>
;; 
;; Permission to use, copy, modify, and distribute this software for
;; any purpose with or without fee is hereby granted, provided that
;; the above copyright notice and this permission notice appear in all
;; copies.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prisoner's Dilemma
;;
;; The two primary functions are run-tournament and run-matrix. The
;; run-tournament function runs a simple tournament between a list of
;; different strategies. It does not do so very efficiently, running
;; two times as many tournaments as necessary. Example,
;;
;;  (run-tournament '(tit-for-tat always-defect rand-play) 1000)
;;
;; The run-matrix function will uniformly randomly place the given
;; strategies around a two dimensional matrix. Each strategy competes
;; against its 4-connected neighbors. Each cell will take on the
;; strategy of its strongest neighbor if a neighbor has a higher score
;; than itself. Example,
;;
;;  (run-matrix 25 250 20 '(tit-for-tat always-defect rand-play))
;;
;; The strategy functions (tit-for-tat, rand-play) are actually
;; strategy function generators. They return a function that plays the
;; strategy. This allows each strategy function to have its own
;; closure.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game engine

;; Run tournament between many players
(defun run-tournament (players n)
  (let ((scores nil)
	(score 0))
    (loop for a in players do
	 (setf score 0)
	 (loop for b in players do
	      (setf score (+ score (car (run-match 
					 (funcall a) 
					 (funcall b) n)))))
	 (push score scores))
    (setf scores (reverse scores))
    (print-results players scores)))

;; Run two specific strategies against each other
(defun run-match (player-a player-b n)
  (let ((choice-a nil)
	(choice-b nil)
	(scores '(0 0))
	(new-a nil))
    (loop for i from 1 to n do
	 (setf new-a    (funcall player-a choice-b))
	 (setf choice-b (funcall player-b choice-a))
	 (setf choice-a new-a)
	 (setf scores (add-lists scores (get-score choice-a choice-b))))
    scores))

(defun add-lists (x y)
  (if (or (null x) (null y)) nil
      (cons (+ (car x) (car y)) (add-lists (cdr x) (cdr y)))))

(defun get-score (a b)
  (if (and (eq a :coop) (eq b :coop)) '(3 3)
      (if (and (eq a :coop) (eq b :defect)) '(0 5)
	  (if (and (eq a :defect) (eq b :coop)) '(5 0)
	      '(1 1)))))

(defun print-results (players scores)
  (format t "~{~a:~20t~d~%~}" (zip players scores)))

(defun zip (x y)
  (if (or (null x) (null y)) nil
      (cons (car x) 
	    (cons (car y) 
		  (zip (cdr x) (cdr y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Competitive strategy matrix

;; Run random strategy matrix
;; Arguments: 
;;    matrix-size, game-iterations, matrix-iterations, player-list
(defun run-random-matrix (a-size n its players)
  (run-matrix (create-matrix a-size a-size players) n its))

;; Run specific matrix
(defun run-matrix (a n its)
  (let ((f nil))
    (setf f (print-matrix a f))
    (format t "~%")
    (loop for i from 2 to its do
	 (setf a (iterate-matrix a n))
	 (print-matrix a f)
	 (format t "~%"))
    (funcall f nil)))

(defun iterate-matrix (a n)
  (let ((w (array-dimension a 0))
	(h (array-dimension a 1))
	(scores nil))
    (setf scores (make-array (list w h) :initial-element 0))
    (loop for i from 0 to (- w 1) do
	 (loop for j from 0 to (- h 1) do
	      (let ((in (wrap-index (+ i 1) w))
		    (jn (wrap-index (+ j 1) h))
		    (s nil))
		(setf s (run-match (funcall (aref a i j)) 
				   (funcall (aref a in j)) n))
		(incf (aref scores i j) (car s))
		(incf (aref scores in j) (cadr s))
		(setf s (run-match (funcall (aref a i j)) 
				   (funcall (aref a i jn)) n))
		(incf (aref scores i j) (car s))
		(incf (aref scores i jn) (cadr s)))))
    (update-matrix a scores)))

(defun update-matrix (a scores)
  (let ((w (array-dimension a 0))
	(h (array-dimension a 1))
	(new-a nil))
    (setf new-a (make-array (list w h)))    
    (loop for i from 0 to (- w 1) do
	 (loop for j from 0 to (- h 1) do
	      (let ((cur-max (aref scores i j)))
		(setf (aref new-a i j) (aref a i j))
		(loop for offset in '((0 1) (1 0) (0 -1) (-1 0)) do
		     (let ((ci (wrap-index (+ (car offset) i) w))
			   (cj (wrap-index (+ (cadr offset) j) h)))
		       (if (> (aref scores ci cj) cur-max)
			   (progn
			     (setf (aref new-a i j) (aref a ci cj))
			     (setf cur-max (aref scores ci cj)))))))))
    new-a))

(defun wrap-index (i w)
  (if (>= i w) 0 
      (if (< i 0) (- w 1) i)))

(defun create-matrix (w h players)
  (let ((a (make-array (list w h)))
	(n (length players))
	(p (apply #'vector players)))
    (loop for i from 0 to (- w 1) do
	 (loop for j from 0 to (- h 1) do
	      (setf (aref a i j) (aref p (random n)))))
    a))

(defun print-matrix (a &optional print-func) 
  (let ((w (array-dimension a 0))
	(h (array-dimension a 1)))
    (if (null print-func) (setf print-func (print-element-func)))
    (loop for i from 0 to (- w 1) do
	 (loop for j from 0 to (- h 1) do
	      (funcall print-func (aref a i j)))
	 (format t "~%"))
    print-func))

(defun print-element-func ()
  (let ((h (make-hash-table))
	(c -1))
    (lambda (x)
      (if (null x)
	  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) h)
	  (let ((a (gethash x h)))
	    (format t "~d "
		    (if a a
			(setf (gethash x h) (setf c (+ c 1))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test functions

(defun create-three-box ()
  (let ((n 25)
	(a nil))
    (setf a (make-array (list n n)))
    (loop for i from 0 to 3 do
	 (loop for j from 0 to (- n 1) do
	      (setf (aref a i j) #'tit-for-tat)))
    (loop for i from 4 to 20 do
	 (setf (aref a i 0) #'tit-for-tat)
	 (setf (aref a i 1) #'tit-for-tat)
	 (setf (aref a i 2) #'tit-for-tat)
	 (setf (aref a i 3) #'tit-for-tat)
	 (loop for j from 4 to 20 do
	      (setf (aref a i j) #'always-coop))
	 (setf (aref a i 21) #'tit-for-tat)
	 (setf (aref a i 22) #'tit-for-tat)
	 (setf (aref a i 23) #'tit-for-tat)
	 (setf (aref a i 24) #'tit-for-tat))
    (loop for i from 20 to 24 do
	 (loop for j from 0 to (- n 1) do
	      (setf (aref a i j) #'tit-for-tat)))
    (setf (aref a 12 12) #'always-defect)
    a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game strategies

(defun tit-for-tat ()
  (lambda (x)
    (if (null x) :coop x)))

(defun tit-for-two-tats ()
  (let ((last :coop))
    (lambda (x)
      (if (or (eq last :coop) (eq x :coop))
	  (progn
	    (setf last x)
	    :coop)
	  (progn
	    (setf last x)
	    :defect)))))

;; Dummies

(defun rand-play ()
  (lambda (x)
    (declare (ignore x))
    (if (> (random 2) 0) :coop :defect)))

(defun switcher-coop ()
  (let ((last :coop))
    (lambda (x)
      (declare (ignore x))
      (if (eq last :coop) 
	  (setf last :defect) 
	  (setf last :coop)))))

(defun switcher-defect ()
  (let ((last :defect))
    (lambda (x)
      (declare (ignore x))
      (if (eq last :coop) 
	  (setf last :defect) 
	  (setf last :coop)))))

(defun always-coop ()
  (lambda (x) 
    (declare (ignore x))
    :coop))

(defun always-defect ()
  (lambda (x) 
    (declare (ignore x))
    :defect))
