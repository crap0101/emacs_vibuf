;; Copyright (C) 2023 Marco Chieppa

;; vibuf.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; vibuf.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file LICENSE.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;
;; V A R S ;;
;;;;;;;;;;;;;

(defvar vibuf__buffer-list '()
  "Default buffer list for custom purposes.")

(defvar vibuf__excluded-names-default "^ .+"
  "Default regex for matching buffer names to be excluded from the list of visitable ones.")

(defvar vibuf__excluded-names "^\\(\\*\\| \\)"
  "A regex for matching buffer names to be excluded from the list of visitable ones.")
;; To not exclude *scratch* et similia buffers: "^ .+"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; S E T T I N G    F U N C E S ' S     V A R S ;;
;;;;;;;;;;;;;;,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vibuf-set__buffer-list-default ()
  "Sets the `vibuf__buffer-list' defvar."
  (setq vibuf__buffer-list (vibuf-get-file-buffers)))

(defun vibuf-set__excluded-names (regex-string)
  "Sets the `vibuf__excluded-names' defvar to `regex-string'."
  (setq vibuf__excluded-names regex-string))

(defun vibuf-set__excluded-names-default ()
  "Sets the `vibuf__excluded-names' defvar."
  (setq vibuf__excluded-names vibuf__excluded-names-default))

;;;;;;;;;;;;;;;;;;;
;; B U F F E R S ;;
;;;;;;;;;;;;;;;;;;;

(defun vibuf-get-buffers (&optional list-of-buffers exclusion-re-str)
  "Returns buffers from `list-of-buffers' (or, by default, from the list
   returned by `buffer-list') excluding some of them."
  (let ((excluded-re-names (if (equal nil exclusion-re-str)
			       vibuf__excluded-names exclusion-re-str)))
    (seq-remove (lambda (buf) (string-match excluded-re-names (buffer-name buf)))
		(if (equal list-of-buffers nil) (buffer-list) list-of-buffers))))

(defun vibuf-get-file-buffers (&optional list-of-buffers)
  "Returns buffers from `list-of-buffers' (or, by default, from the list
   returned by `buffer-list') which are actually visiting a file."
  (seq-remove (lambda (buf) (not (buffer-file-name buf)))
	      (if (equal nil list-of-buffers) (buffer-list) list-of-buffers)))

(defun vibuf-buffers-string (&optional list-of-buffers prefix-msg separator)
  "Returns a string (prefixed (with `prefix-string') of elements
   separated by `separator' from `list-of-buffers' (or, by default,
   from the list returned by `vibuf-get-buffers')."
  (concat (if (equal prefix-msg nil) "" prefix-msg)
	  (seq-mapcat (lambda (buf)
			(concat (buffer-name buf)
				(if (equal separator nil) "" separator)))
		      (if (equal nil list-of-buffers)
			  (vibuf-get-buffers) list-of-buffers)
		      'string)))

(defun vibuf-remove-current-buffer (&optional list-of-buffers)
  "Returns a list of buffers from `list-of-buffers' (or, by default, from
   the list returned by `vibuf-get-buffers') with the current buffer removed."
  (let* ((buffers (if (equal nil list-of-buffers)
		      (vibuf-get-buffers) list-of-buffers))
	 (curr (current-buffer)))
    (seq-remove (lambda (buf) (equal buf curr)) buffers)))

(defun vibuf-switch-to-buffer (buf)
  "Actually switch to `buf'."
  (switch-to-buffer buf))

(defun vibuf-create-buffer-hook-function ()
  "Hook to run when visiting a file."
  (let* ((buf (current-buffer))
	(vibuf__buffer-list-empty (equal 0 (seq-length vibuf__buffer-list)))
	(prev (car (vibuf-remove-current-buffer
		    (vibuf-get-file-buffers (buffer-list)))))
	(prev (if (equal nil prev) (car (buffer-list)) prev)))
    (progn
      (message "NEW BUFFER: %s [tot managed before = %d]"
	       buf (seq-length vibuf__buffer-list))
      (if vibuf__buffer-list-empty
	  (setq vibuf__buffer-list (list buf))
	(let* ((pos (+ 1 (seq-position vibuf__buffer-list prev)))
	       (head (seq-take vibuf__buffer-list pos))
	       (tail (seq-drop vibuf__buffer-list pos)))
	  (setq vibuf__buffer-list
		(seq-concatenate 'list head (list buf) tail))))
      (message "PREV BUFFER: %s [actual tot managed =%d]"
	       prev (seq-length vibuf__buffer-list))
      (message "(visit) switch to: %s" buf)
      (vibuf-switch-to-buffer buf))))

(defun vibuf-kill-buffer-hook-function ()
  "Hook to run when killing a buffer."
  (progn
    (setq vibuf__buffer-list (vibuf-remove-current-buffer vibuf__buffer-list))
    (message "KILLING BUFFER: %s [actual tot managed = %d] <%s>"
	     (current-buffer)
	     (seq-length vibuf__buffer-list)
	     (vibuf-buffers-string vibuf__buffer-list "remains:" "|"))
    (let* ((buf (car  vibuf__buffer-list))
	   (buf (if (equal nil buf)
		    (car (vibuf-remove-current-buffer (buffer-list))) buf)))
      (progn
	(message "(kill) switch to: %s" buf)
	(vibuf-switch-to-buffer buf)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B U F F E R    C Y C L I N G ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vibuf-next-buffer ()
  "Switch to the next buffer in the `vibuf__buffer-list' cycling on them."
  (let* ((curr (current-buffer))
	 (len (length vibuf__buffer-list))
	 (curr-idx (seq-position vibuf__buffer-list curr)))
    (if (or (equal 0 len) (equal nil curr-idx))
	(vibuf-switch-to-buffer (car (buffer-list)))    ;; XXX: change this to avoid getting the same buffer (i.e. no moves)
      (if (equal curr-idx (- len 1))
	  (vibuf-switch-to-buffer (car vibuf__buffer-list))
	(vibuf-switch-to-buffer (seq-elt vibuf__buffer-list (+ curr-idx 1)))))))

(defun vibuf-prev-buffer ()
  "Switch to the previous buffer in the `vibuf__buffer-list' cycling on them."
  (let* ((curr (current-buffer))
	 (len (length vibuf__buffer-list))
	 (curr-idx (seq-position vibuf__buffer-list curr)))
    (if (or (equal 0 len) (equal nil curr-idx))
	(vibuf-switch-to-buffer (car (buffer-list)))    ;; XXX: change this to avoid getting the same buffer (i.e. no moves)
      (if (equal curr-idx 0)
	  (vibuf-switch-to-buffer (car (reverse vibuf__buffer-list)))
	(vibuf-switch-to-buffer (seq-elt vibuf__buffer-list (- curr-idx 1)))))))


(provide 'vibuf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E X A M P L E    C O N F I N G ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To be put in the init file:
;(add-to-list 'load-path "/some/path")
;(require 'vibuf)
;(add-hook 'find-file-hook 'vibuf-create-buffer-hook-function)
;(add-hook 'kill-buffer-hook 'vibuf-kill-buffer-hook-function)
;(add-hook 'emacs-startup-hook 'vibuf-set__buffer-list-default)
; regex preference: (add-hook 'emacs-startup-hook 'vibuf-set__buffer-list-default)
;;
;; key-bindings:
;(global-set-key (kbd "C-S-<left>") (lambda () (interactive) (vibuf-prev-buffer)))
;(global-set-key (kbd "C-S-<right>") (lambda () (interactive) (vibuf-next-buffer)))

