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

(defvar vibuf__buffer-name-if-empty '()
  "Name of the default buffer to switch to when
   `vibuf__buffer-list' is empty... e.g. *scratch*.")

(defconst vibuf__excluded-names-default "^ .+"
  "Default regex for matching buffer names
    to be excluded from the list of visitable ones.")
  ;; NOTE: use this to not exclude *scratch* et similia buffers.

(defvar vibuf__excluded-names "^[ *]"
  "A regex for matching buffer names
   to be excluded from the list of visitable ones.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; S E T T I N G    F U N C E S ' S     V A R S ;;
;;;;;;;;;;;;;;,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vibuf-set__buffer-list-default ()
  "Sets the `vibuf__buffer-list' defvar."
  (setq vibuf__buffer-list (vibuf-get-file-buffers)))

(defun vibuf-set__buffer-name-if-empty (string-of-buffer-name)
  "Sets the `vibuf__buffer-name-if-empty' defvar."
  (setq vibuf__buffer-name-if-empty string-of-buffer-name))

(defun vibuf-set__excluded-names (regex-string)
  "Sets the `vibuf__excluded-names' defvar to `regex-string'."
  (setq vibuf__excluded-names regex-string))

(defun vibuf-set__excluded-names-to-default ()
  "Sets the `vibuf__excluded-names' defvar with `vibuf__excluded-names-default'."
  (setq vibuf__excluded-names vibuf__excluded-names-default))

;;;;;;;;;;;;;;;;;;;
;; B U F F E R S ;;
;;;;;;;;;;;;;;;;;;;

(defun vibuf-get-buffer-from-name (name &optional list-of-buffers)
  "Return the buffer named `name' from `list-of-buffers'
   (or, by default, from (buffer-list))."
  (let ((buffers (if (equal nil list-of-buffers) (buffer-list) list-of-buffers)))
    (seq-remove (lambda (buf) (not (string= name (buffer-name buf)))) buffers)))

(defun __vibuf-get-buffer-from-name (name &optional list-of-buffers)
  "Return the buffer named `name' from `list-of-buffers'
   (or, by default, from (buffer-list))."
  (let ((buffers (if (equal nil list-of-buffers) (buffer-list) list-of-buffers)))
    (seq-remove (lambda (buf) (not (string= name (buffer-name buf)))) buffers)))

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
	(prev0 (car (vibuf-remove-current-buffer
		    (vibuf-get-file-buffers (buffer-list)))))
	(prev1 (if (equal nil prev0) (car (buffer-list)) prev0))
	(prev (or prev0 prev1)))
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
  ;; Since a lot of other libraries functions creates (and kills)
  ;; temporary buffers, check first if current-buffer is a visiting file.
  (if (not (buffer-file-name (current-buffer)))
      '() ;; not a visited file, do nothing.
    (progn
      (setq vibuf__buffer-list (vibuf-remove-current-buffer vibuf__buffer-list))
      (setq __buffer-list (vibuf-remove-current-buffer (buffer-list)))
      (message "KILLING BUFFER: %s [actual tot managed = %d] <%s>"
	       (current-buffer)
	       (seq-length vibuf__buffer-list)
	       (vibuf-buffers-string vibuf__buffer-list "remains:" "|"))
      (let* ((buf0 (car vibuf__buffer-list))
	     (buf1 (if (equal nil buf0)
		       (or (car (vibuf-get-buffers __buffer-list vibuf__excluded-names))
			   ;; as last resource, go to the first
			   ;; live buffer found (excluding special buffers)
			   (car (vibuf-get-buffers __buffer-list "[ ]")))))
	     (buf (or buf0 buf1)))
	(progn
	  (message "(kill) switch to: %s" buf)
	  (vibuf-switch-to-buffer buf))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B U F F E R    C Y C L I N G ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vibuf-next-buffer ()
  "Switch to the next buffer in the `vibuf__buffer-list' cycling on them."
  (let* ((curr (current-buffer))
	 (len (length vibuf__buffer-list))
	 (curr-idx (seq-position vibuf__buffer-list curr)))
    (if (and (> len 0) (equal nil curr-idx))
	;; if in a non tracked buffer, switch to the last visited one
	(vibuf-switch-to-buffer (car (vibuf-get-file-buffers)))
      (if (equal 0 len)
	  (progn
	    (setq buf (or (car (vibuf-get-buffer-from-name vibuf__buffer-name-if-empty))
			  (car (vibuf-get-buffers (buffer-list) vibuf__excluded-names))
			  ;; as last resource, go to the first
			  ;; live buffer found (excluding special buffers)
			  (car (vibuf-get-buffers (buffer-list) "[ ]"))))
	    (message "vibuf-next-buffer: buffer to switch to is: %s [%s]" buf (type-of buf))
	    (vibuf-switch-to-buffer buf))
	(if (equal curr-idx (- len 1))
	    (vibuf-switch-to-buffer (car vibuf__buffer-list))
	  (vibuf-switch-to-buffer (seq-elt vibuf__buffer-list (+ curr-idx 1))))))))

(defun vibuf-prev-buffer ()
  "Switch to the previous buffer in the `vibuf__buffer-list' cycling on them."
  (let* ((curr (current-buffer))
	 (len (length vibuf__buffer-list))
	 (curr-idx (seq-position vibuf__buffer-list curr)))
    (if (and (> len 0) (equal nil curr-idx))
	;; if in a non tracked buffer, switch to the last visited one
	(vibuf-switch-to-buffer (car (vibuf-get-file-buffers)))
      (if (equal 0 len)
	  (progn
	    (setq buf (or (car (vibuf-get-buffer-from-name vibuf__buffer-name-if-empty))
			  (car (vibuf-get-buffers (buffer-list) vibuf__excluded-names))
			  ;; as last resource, go to the first
			  ;; live buffer found (excluding special buffers)
			  (car (vibuf-get-buffers (buffer-list) "[ ]"))))
	    (message "vibuf-prev-buffer: buffer to switch to is: %s [%s]" buf (type-of buf))
	    (vibuf-switch-to-buffer buf))
	(if (equal curr-idx 0)
	    (vibuf-switch-to-buffer (car (reverse vibuf__buffer-list)))
	  (vibuf-switch-to-buffer (seq-elt vibuf__buffer-list (- curr-idx 1))))))))


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
;
;; set a tefalut buffer to switch to when no visiting files:
;(vibuf-set__buffer-name-if-empty "*scratch*")
					;
;; regex exclude preference:
; (add-hook 'emacs-startup-hook (lambda () (vibuf-set__excluded-names "some-regex")))
;
;; key-bindings:
;(global-set-key (kbd "C-S-<left>") (lambda () (interactive) (vibuf-prev-buffer)))
;(global-set-key (kbd "C-S-<right>") (lambda () (interactive) (vibuf-next-buffer)))

