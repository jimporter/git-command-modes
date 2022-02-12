;;; git-command-modes.el --- Major modes for Git commands -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jim Porter

;; Author: Jim Porter
;; URL: https://github.com/jimporter/git-command-modes
;; Version: 0.1-dev
;; Keywords:
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major modes for Git commands.

(eval-when-compile (require 'subr-x))

(defvar git-common-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?\n ">")
    table))

;;;###autoload
(define-derived-mode git-commit-msg-mode text-mode "Git Commit"
  (setq-local font-lock-keywords '(git-common-syntax-table))
  (setq-local syntax-propertize-function
              (syntax-propertize-rules (".\\(#\\)" (1 ".")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG\\'" . git-commit-msg-mode))

(defun git-rebase-todo--match-keyword (name shorthand &optional face)
  "Return a font-lock matcher for the Git Rebase keyword NAME.
SHORTHAND is the single-character shorthand for the keyword.
FACE, if non-nil, is the face to use for the keyword; otherwise,
use `font-lock-keyword-face.'"
  `(,(rx-to-string `(seq line-start
                         (or ,name ,shorthand)
                         symbol-end))
    (0 (list 'face ,(or face 'font-lock-keyword-face)
             'git-rebase-todo-command ',(intern name)))))

(defconst git-rebase-todo--commit-rx
  '(seq symbol-start (>= 5 hex) symbol-end)
  "An `rx' expression recognizing a Git commit hash.")

(defconst git-rebase-todo--match-commit
  `(,(rx-to-string `(seq point (+ space)
                         (group ,git-rebase-todo--commit-rx)))
    nil nil (1 font-lock-variable-name-face))
  "A font-lock highlighter matching a Git commit hash following a space.")

(defconst git-rebase-todo--match-label
  `(,(rx-to-string '(seq point (+ space)
                         (group symbol-start
                                (+ alnum)
                                symbol-end)))
    nil nil (1 font-lock-builtin-face))
  "A font-lock highlighter matching a Git label following a space.")

(defvar git-rebase-todo-font-lock-keywords
  `(;; Commands taking a commit.
    ,@(let ((cmds '(("pick"   "p")
                    ("reword" "r")
                    ("edit"   "e")
                    ("squash" "s")
                    ("drop"   "p" font-lock-warning-face))))
        (mapcar (lambda (cmd)
                  `(,@(apply #'git-rebase-todo--match-keyword cmd)
                    ,git-rebase-todo--match-commit))
                cmds))
    ;; Commands taking a label.
    ,@(let ((cmds '(("label" "l")
                    ("reset" "t"))))
        (mapcar (lambda (cmd)
                  `(,@(apply #'git-rebase-todo--match-keyword cmd)
                    ,git-rebase-todo--match-label))
                cmds))
    ;; Miscellaneous commands.
    ,@(let ((cmds '(("exec" "x")
                    ("break" "b" font-lock-type-face))))
        (mapcar (lambda (cmd)
                  (apply #'git-rebase-todo--match-keyword cmd))
                cmds))
    ;; Fixup command.
    (,@(git-rebase-todo--match-keyword "fixup" "f")
     (,(rx-to-string '(seq point (+ space)
                         (group (or "-c" "-C"))))
      nil nil (0 '(face nil git-rebase-todo-command fixup))
      (1 font-lock-constant-face))
     ,git-rebase-todo--match-commit)
    ;; Merge command.
    (,@(git-rebase-todo--match-keyword "merge" "m")
     (,(rx-to-string `(seq point (+ space)
                           (group (or "-c" "-C"))
                           (+ space)
                           (group ,git-rebase-todo--commit-rx)))
      nil nil (1 font-lock-constant-face) (2 font-lock-variable-name-face))
     ,git-rebase-todo--match-label))
  "Keywords to highlight in Git Rebase mode.")

(defun git-rebase-todo-move-up (&optional lines)
  "Move the current rebase command up one line."
  (interactive "p")
  (git-rebase-todo-move-down (- lines)))

(defun git-rebase-todo-move-down (&optional lines)
  "Move the current rebase command down one line."
  (interactive "p")
  (let ((column (current-column)))
    (unless (eq (forward-line) 0)
      (newline))
    (unwind-protect
        (transpose-lines lines)
      (forward-line -1)
      (move-to-column column))))

(defun git-rebase-todo-commit-command-p (command &optional include-drop)
  "Return non-nil if COMMAND takes a commit.
If INCLUDE-DROP is non-nil, consider the `drop' command to be a
commit command."
  (or (memq command '(pick reword edit squash fixup))
      (and include-drop (eq command 'drop))))

(defun git-rebase-todo-fold-command-p (command)
  "Return non-nil if COMMAND folds a commit into a previous commit command."
  (memq command '(squash fixup)))

(defun git-rebase-todo-replace-command-at-point (command &optional extra)
  "If the current command at point is a commit command, replace it with COMMAND.
This allows replacing a command like \"pick\" with \"squash\",
for example.  EXTRA, if non-nil, is a string of extra data to
append to the command. This is useful if the command takes
optional flags."
  (unless (git-rebase-todo-commit-command-p command t)
    (error "Invalid command type %S" command))
  (save-excursion
    (beginning-of-line)
    (let ((old-command (get-text-property (point) 'git-rebase-todo-command)))
      (unless old-command
        (user-error "No command here"))
      (unless (git-rebase-todo-commit-command-p old-command t)
        (user-error "Not a commit"))
      (when (git-rebase-todo-fold-command-p command)
        (if-let ((prev-point (previous-single-property-change
                              (point) 'git-rebase-todo-command)))
            (unless (git-rebase-todo-commit-command-p
                     (get-text-property (1- prev-point)
                                        'git-rebase-todo-command))
              (user-error "Previous command is not a commit"))
          (user-error "No previous command")))
      ;; Replace the old command with the new one.
      (let ((command-end (next-single-property-change
                          (point) 'git-rebase-todo-command)))
        (delete-region (point) command-end)
        (insert (symbol-name command))
        (when extra (insert " " extra))))))

(defun git-rebase-todo-insert-command-at-point (command &optional extra after)
  "Insert COMMAND on the line before point.
EXTRA, if non-nil, is a string of extra arguments to include
after COMMAND.  AFTER, if non-nil, causes the command to be
inserted on the line after point."
  (save-excursion
    (when after
      (forward-line 1))
    (beginning-of-line)
    (insert (symbol-name command))
    (when extra (insert " " extra))
    (insert "\n")))

(defun git-rebase-todo-set-pick-at-point ()
  "Set the current command at point to \"pick\"."
  (interactive)
  (git-rebase-todo-replace-command-at-point 'pick))

(defun git-rebase-todo-set-reword-at-point ()
  "Set the current command at point to \"reword\"."
  (interactive)
  (git-rebase-todo-replace-command-at-point 'reword))

(defun git-rebase-todo-set-edit-at-point ()
  "Set the current command at point to \"edit\"."
  (interactive)
  (git-rebase-todo-replace-command-at-point 'edit))

(defun git-rebase-todo-set-squash-at-point ()
  "Set the current command at point to \"squash\"."
  (interactive)
  (git-rebase-todo-replace-command-at-point 'squash))

(defun git-rebase-todo-set-fixup-at-point (&optional flag)
  "Set the current command at point to \"fixup\".

With \\[universal-argument] prefix, set the \"-c\" flag (use the commit message of
the current commit for the folded commit, and allow editing the
message).  With two \\[universal-argument] prefixes, set the \"-C\" flag (use the
commit message of the current commit, and don't edit the message)."
  (interactive "P")
  (let ((arg (cond
              ((not flag) nil)
              ((= (prefix-numeric-value flag) 4) "-c")
              (t "-C"))))
    (git-rebase-todo-replace-command-at-point 'fixup arg)))

(defun git-rebase-todo-set-drop-at-point ()
  "Set the current command at point to \"drop\"."
  (interactive)
  (git-rebase-todo-replace-command-at-point 'drop))

(defun git-rebase-todo-insert-exec-at-point (command &optional after)
  "Insert an \"exec\" command on the line before point.
COMMAND is the command to execute.  AFTER, if non-nil, causes the
command to be inserted on the line after point."
  (interactive
   (list
    (read-shell-command "Command: ")
    (consp current-prefix-arg)))
  (git-rebase-todo-insert-command-at-point 'exec command after))

(defun git-rebase-todo-insert-break-at-point (&optional after)
  "Insert an \"exec\" command on the line before point.
COMMAND is the command to execute.  AFTER, if non-nil, causes the
command to be inserted on the line after point."
  (interactive "P")
  (git-rebase-todo-insert-command-at-point 'break nil after))

(defun git-rebase-todo-insert-label-at-point (name &optional after)
  "Insert a \"label\" command with label NAME on the line before point.
AFTER, if non-nil, causes the command to be inserted on the line
after point."
  ;; TODO: Support completion on labels.
  (interactive "MLabel: \nP")
  (git-rebase-todo-insert-command-at-point 'label name after))

(defun git-rebase-todo-insert-reset-at-point (name &optional after)
  "Insert a \"reset\" command with label NAME on the line before point.
AFTER, if non-nil, causes the command to be inserted on the line
after point."
  ;; TODO: Support completion on labels.
  (interactive "MLabel: \nP")
  (git-rebase-todo-insert-command-at-point 'reset name after))

;; TODO: Support inserting `merge' command.

(defvar git-rebase-todo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<up>") #'git-rebase-todo-move-up)
    (define-key map (kbd "ESC <up>") #'git-rebase-todo-move-up)
    (define-key map (kbd "M-<down>") #'git-rebase-todo-move-down)
    (define-key map (kbd "ESC <down>") #'git-rebase-todo-move-down)
    (define-key map (kbd "C-c C-p") #'git-rebase-todo-set-pick-at-point)
    (define-key map (kbd "C-c C-r") #'git-rebase-todo-set-reword-at-point)
    (define-key map (kbd "C-c C-e") #'git-rebase-todo-set-edit-at-point)
    (define-key map (kbd "C-c C-s") #'git-rebase-todo-set-squash-at-point)
    (define-key map (kbd "C-c C-f") #'git-rebase-todo-set-fixup-at-point)
    (define-key map (kbd "C-c C-d") #'git-rebase-todo-set-drop-at-point)
    (define-key map (kbd "C-c C-x") #'git-rebase-todo-insert-exec-at-point)
    (define-key map (kbd "C-c C-b") #'git-rebase-todo-insert-break-at-point)
    (define-key map (kbd "C-c C-l") #'git-rebase-todo-insert-label-at-point)
    (define-key map (kbd "C-c C-t") #'git-rebase-todo-insert-reset-at-point)
    map)
  "Keymap for Git Rebase buffers.")

(easy-menu-define git-rebase-todo-menu-map git-rebase-todo-mode-map
  "Menu for Git Rebase buffers."
  '("Git Rebase"
    ["Move Command Up" git-rebase-todo-move-up
     :help "Move the command at point up one line"]
    ["Move Command Down" git-rebase-todo-move-down
     :help "Move the command at point down one line"]))

;;;###autoload
(define-derived-mode git-rebase-todo-mode nil "Git Rebase"
  (setq-local font-lock-keywords '(git-common-syntax-table))
  (setq-local syntax-propertize-function
              (syntax-propertize-rules (".\\(#\\)" (1 "."))))
  (setq-local font-lock-defaults
              '(git-rebase-todo-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("/git-rebase-todo\\'" . git-rebase-todo-mode))

(provide 'git-command-modes)
;;; git-command-modes.el ends here
