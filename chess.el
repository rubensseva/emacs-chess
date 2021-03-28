;;; packge --- some summary
;;; Commentary:
;;; Code:

(defun create-board ()
  '(
    ("R" "K" "I" "Q" "K" "I" "K" "R")
    ("B" "B" "B" "B" "B" "B" "B" "B")
    ("*" "*" "*" "*" "*" "*" "*" "*")
    ("*" "*" "*" "*" "*" "*" "*" "*")
    ("*" "*" "*" "*" "*" "*" "*" "*")
    ("*" "*" "*" "*" "*" "*" "*" "*")
    ("b" "b" "b" "b" "b" "b" "b" "b")
    ("r" "k" "i" "q" "k" "i" "k" "r")
    ))


(defun print-board (board)
  "Print the board."
  (progn
    (erase-buffer)
    (insert "  a b c d e f g h\n")
    (let ((count 1))
	(dolist (x board)
	  (insert (format "%d " count))
	  (dolist (xx x)
	    (insert (format "%s " xx))
	  )
	  ;;(insert (format "%s" x))
	  (insert (format "%d" count))
	  (setq count (+ count 1))
	  (insert "\n")
	  )
	)
     )
    (insert "  a b c d e f g h\n")
  )

(defun get-index (chess-string-index)
  "Say hello.  CHESS-STRING-INDEX: the chess string in human readable format e.g. 2A."
  (let ((char (substring chess-string-index 0 1)) (num (substring chess-string-index 1 2)))
    (list (- (string-to-char (upcase char)) 65) (- (string-to-number num) 1))
  )
)

(defun get-piece (index board)
  "Get piece by INDEX and BOARD."
    (let ((piece-line (nth (nth 1 index) board)))
	(nth (nth 0 index) piece-line)
	)
    )

(defun set-nth (list index val)
    (let ((tmplist ()) (count 0) )
      (while list
        (if (eq count index)
	  (push val tmplist)
	  (push (car list) tmplist)
	)
	(setq list (cdr list))
	(setq count (+ count 1))
	)
       (reverse tmplist)
     )
 )


(defun is-above (cur-index mov-index)
  (and (eq (car cur-index) (car mov-index)) (eq (cadr cur-index) (+ (cadr mov-index) 1) ) )
)
(defun is-below (cur-index mov-index)
  (and (eq (car cur-index) (car mov-index)) (eq (cadr cur-index) (- (cadr mov-index) 1) ) )
)
(defun is-above-right (cur-index mov-index)
  (and (eq (car cur-index) (- (car mov-index) 1)) (eq (cadr cur-index) (+ (cadr mov-index) 1) ) )
)
(defun is-above-left (cur-index mov-index)
  (and (eq (car cur-index) (+ (car mov-index) 1)) (eq (cadr cur-index) (+ (cadr mov-index) 1) ) )
)

(defun is-legal-move-pawn (piece cur-index mov-index board player)
    (if (eq player 1)
	(let (
	    (above (list (car cur-index) (+ (cadr cur-index) 1)))
	    (above-right (list (+ (car cur-index) 1) (+ (cadr cur-index) 1)))
	    (above-left (list (- (car cur-index) 1) (+ (cadr cur-index) 1)))
	    )
	    (or
		(and (equal above mov-index) (string-equal (get-piece above board) "*"))
		(and (equal above-left mov-index) (string-equal (get-piece above-left board) "B"))
		(and (equal above-right mov-index) (string-equal (get-piece above-right board) "B"))
		))))

(defun is-legal-move (piece cur-index mov-index board)
  "Checks if a move is legal for PIECE INDEX and BOARD."
  (progn
    (cond
     ((string-equal piece "b") (is-legal-move-pawn piece cur-index mov-index board 1))
     ((string-equal piece "B") (message "handling B"))
     (t (message "couldnt find any for: %s" piece)))))


(defun chess ()
  "Play chess."
  (interactive)
  (let ((buffer-name "chess-buffer-foo") (board (create-board)))
    (switch-to-buffer buffer-name)
    (print-board board)
    (let ((curr_player 1) (piece-regex "^[a-h][1-9]$") (text-to-insert (read-string "piece to move?")))
	(message (format "you entered: %s" text-to-insert))
	(if (string-match piece-regex text-to-insert)
	    (progn
	      (message "match! sending in text %s" text-to-insert)
	      (let ((indexes (get-index text-to-insert)))
		(message "%s" indexes)
		(let ((piece (get-piece indexes board)))
		  (message piece)
		  )
		)
	      
	    )
	    (message (format "no match... please enter a valid num: %s" piece-regex))
	    )
    )
  )
)
