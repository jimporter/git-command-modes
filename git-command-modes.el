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

(defgroup git-command-modes nil
  "Major modes for Git commands"
  :group 'vc)

(defface git-command-section-heading '((t :inherit font-lock-keyword-face))
  "Face for section headings.")

(defface git-commit '((t :inherit font-lock-variable-name-face))
  "Face for commit hashes.")

(defface git-file-deleted '((t :inherit font-lock-warning-face))
  "Face for deleted file indicator.")

(defface git-file-modified '((t :inherit font-lock-variable-name-face))
  "Face for modified file indicator.")

(defface git-file-new '((t :inherit font-lock-string-face))
  "Face for new file indicator.")

(defface git-file-renamed '((t :inherit font-lock-builtin-face))
  "Face for renamed file indicator.")

(defface git-rebase-command '((t :inherit font-lock-keyword-face))
  "Face for commands when rebasing.")

(defface git-rebase-drop-command '((t :inherit font-lock-warning-face))
  "Face for the \"drop\" command when rebasing.")

(defface git-rebase-break-command '((t :inherit font-lock-type-face))
  "Face for the \"break\" command when rebasing.")

(defface git-rebase-label '((t :inherit font-lock-builtin-face))
  "Face for labels when rebasing.")

(defface git-rebase-flag '((t :inherit font-lock-constant-face))
  "Face for command flags when rebasing.")

(defvar git-common-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?\n ">")
    table)
  "Syntax table for Git command files.")

(defun git-rebase-todo--match-keyword (prefix name shorthand &optional face)
  "Return a font-lock matcher for the Git Rebase keyword NAME.
SHORTHAND is the single-character shorthand for the keyword.
PREFIX is a list of `rx' forms that should precede the keyword.
FACE, if non-nil, is the face to use for the keyword; otherwise,
use `git-rebase-command'."
  `(,(rx-to-string `(seq ,@prefix
                         (group (or ,name ,shorthand))
                         symbol-end))
    (1 (list 'face ',(or face 'git-rebase-command)
             'git-rebase-todo-command ',(intern name))
       t)))

(defconst git-rebase-todo--commit-rx
  '(seq symbol-start (>= 5 hex) symbol-end)
  "An `rx' expression recognizing a Git commit hash.")

(defconst git-rebase-todo--match-commit
  `(,(rx-to-string `(seq point (+ space)
                         (group ,git-rebase-todo--commit-rx)))
    nil nil (1 'git-commit t))
  "A font-lock highlighter matching a Git commit hash following a space.")

(defconst git-rebase-todo--match-label
  `(,(rx-to-string '(seq point (+ space)
                         (group symbol-start
                                (+ alnum)
                                symbol-end)))
    nil nil (1 'git-rebase-label t))
  "A font-lock highlighter matching a Git label following a space.")

(defun git-rebase-todo--font-lock-keywords (prefix)
  "Return a list of font-lock keywords for Git Rebase Todo commands.
PREFIX is a list of `rx' forms that should precede each command."
  `(;; Commands taking a commit.
    ,@(let ((cmds '(("pick"   "p")
                    ("reword" "r")
                    ("edit"   "e")
                    ("squash" "s")
                    ("drop"   "p" 'git-rebase-drop-command))))
        (mapcar (lambda (cmd)
                  `(,@(apply #'git-rebase-todo--match-keyword prefix cmd)
                    ,git-rebase-todo--match-commit
                    (,(rx-to-string '(* nonl))
                     nil nil (0 'default t))))
                cmds))
    ;; Commands taking a label.
    ,@(let ((cmds '(("label" "l")
                    ("reset" "t"))))
        (mapcar (lambda (cmd)
                  `(,@(apply #'git-rebase-todo--match-keyword prefix cmd)
                    ,git-rebase-todo--match-label))
                cmds))
    ;; Miscellaneous commands.
    ,@(let ((cmds '(("exec" "x")
                    ("break" "b" 'git-rebase-break-command))))
        (mapcar (lambda (cmd)
                  (apply #'git-rebase-todo--match-keyword prefix cmd))
                cmds))
    ;; Fixup command.
    (,@(git-rebase-todo--match-keyword prefix "fixup" "f")
     (,(rx-to-string '(seq point (+ space)
                           (group (or "-c" "-C"))))
      nil nil (0 '(face nil git-rebase-todo-command fixup) t)
      (1 'git-rebase-flag t))
     ,git-rebase-todo--match-commit)
    ;; Merge command.
    (,@(git-rebase-todo--match-keyword prefix "merge" "m")
     (,(rx-to-string `(seq point (+ space)
                           (group (or "-c" "-C"))
                           (+ space)
                           (group ,git-rebase-todo--commit-rx)))
      nil nil (1 'git-rebase-flag t) (2 'git-commit t))
     ,git-rebase-todo--match-label)))

(defvar git-commit-msg-font-lock-keywords
  `((,(rx line-start "#" (+ space)
          (group (or (seq "Last commands done" (* nonl))
                     (seq "Next commands to do" (* nonl))
                     (seq "Changes" (* nonl))
                     "Untracked files")
                 ":"))
     1 'git-command-section-heading t)
    ,@(let ((changes '(("deleted"  . 'git-file-deleted)
                       ("modified" . 'git-file-modified)
                       ("new file" . 'git-file-new)
                       ("renamed"  . 'git-file-renamed))))
        (mapcar (lambda (change)
                  `(,(rx-to-string `(seq line-start "#" (+ space)
                                         (group ,(car change) ":")))
                    (1 ,(cdr change) t)
                    (".*" nil nil (0 'default t))))
                changes))
    ,@(git-rebase-todo--font-lock-keywords '("#" (+ space))))
  "Keywords to highlight in Git Commit mode.")

;;;###autoload
(define-derived-mode git-commit-msg-mode text-mode "Git Commit"
  (setq-local font-lock-keywords '(git-common-syntax-table))
  (setq-local syntax-propertize-function
              (syntax-propertize-rules (".\\(#\\)" (1 "."))))
  (setq-local font-lock-defaults
              '(git-commit-msg-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG\\'" . git-commit-msg-mode))

(defvar git-rebase-todo-font-lock-keywords
  (git-rebase-todo--font-lock-keywords '(line-start))
  "Keywords to highlight in Git Rebase mode.")

(defun git-rebase-todo-move-up (&optional lines)
  "Move the current rebase command up one line.
If LINES is non-nil, move up that many lines."
  (interactive "p")
  (git-rebase-todo-move-down (- lines)))

(defun git-rebase-todo-move-down (&optional lines)
  "Move the current rebase command down one line.
If LINES is non-nil, move down that many lines."
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

(defun git-rebase-todo-replace-command-in-region (command &optional beg end
                                                          extra)
  "If the commands in the region are commit commands, replace them with COMMAND.
This allows replacing a command like \"pick\" with \"squash\",
for example.

The region is defined by BEG and END.  If BEG is nil, use the
current point.  If END is nil, use the minimum of BEG + 1 and
`point-max'.

EXTRA, if non-nil, is a string of extra data to append to the
command.  This is useful if the command takes optional flags."
  (unless (git-rebase-todo-commit-command-p command t)
    (error "Invalid command type %S" command))
  (unless beg
    (setq beg (point)))
  (unless end
    (setq end (min (1+ beg) (point-max))))
  (let ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (< (point-marker) end-marker)
        (let ((old-command (get-text-property
                            (point) 'git-rebase-todo-command)))
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
            (when extra (insert " " extra))))
        (forward-line 1)))))

(defun git-rebase-todo-insert-command-at-point (command &optional point extra
                                                        after)
  "Insert COMMAND on the line before POINT.
If POINT is nil, use the current point.  EXTRA, if non-nil, is a
string of extra arguments to include after COMMAND.  AFTER, if
non-nil, causes the command to be inserted on the line after
point."
  (save-excursion
    (when point
      (goto-char point))
    (when after
      (forward-line 1))
    (beginning-of-line)
    (insert (symbol-name command))
    (when extra (insert " " extra))
    (insert "\n")))

(defun git-rebase-todo-region-or-point ()
  "Return the region if the region is active, otherwise return the point.
This always returns a list of the form (BEGIN END-OR-NIL)."
  (if (region-active-p)
      (list (region-beginning) (region-end))
    (list (point) nil)))

(defun git-rebase-todo-set-pick (&optional beg end)
  "Set the commands in the region from BEG to END to \"pick\"."
  (interactive (git-rebase-todo-region-or-point))
  (git-rebase-todo-replace-command-in-region 'pick))

(defun git-rebase-todo-set-reword (&optional beg end)
  "Set the commands in the region from BEG to END to \"reword\"."
  (interactive (git-rebase-todo-region-or-point))
  (git-rebase-todo-replace-command-in-region 'reword))

(defun git-rebase-todo-set-edit (&optional beg end)
  "Set the commands in the region from BEG to END to \"edit\"."
  (interactive (git-rebase-todo-region-or-point))
  (git-rebase-todo-replace-command-in-region 'edit))

(defun git-rebase-todo-set-squash (&optional beg end)
  "Set the commands in the region from BEG to END to \"squash\"."
  (interactive (git-rebase-todo-region-or-point))
  (git-rebase-todo-replace-command-in-region 'squash))

(defun git-rebase-todo-set-fixup (&optional beg end flag)
  "Set the commands in the region from BEG to END to \"fixup\".

With \\[universal-argument] prefix, set the \"-c\" flag (use the commit message of
the current commit for the folded commit, and allow editing the
message).  With two \\[universal-argument] prefixes, set the \"-C\" flag (use the
commit message of the current commit, and don't edit the message)."
  (interactive (append (git-rebase-todo-region-or-point)
                       (list current-prefix-arg)))
  (let ((arg (cond
              ((not flag) nil)
              ((= (prefix-numeric-value flag) 4) "-c")
              (t "-C"))))
    (git-rebase-todo-replace-command-in-region 'fixup beg end arg)))

(defun git-rebase-todo-set-drop (&optional beg end)
  "Set the commands in the region from BEG to END to \"drop\"."
  (interactive (git-rebase-todo-region-or-point))
  (git-rebase-todo-replace-command-in-region 'drop beg end))

(defun git-rebase-todo-insert-exec (command &optional point after)
  "Insert an \"exec\" command on the line before POINT.
COMMAND is the command to execute.  AFTER, if non-nil, causes the
command to be inserted on the line after point."
  (interactive (list (read-shell-command "Command: ")
                     (point)
                     current-prefix-arg))
  (git-rebase-todo-insert-command-at-point 'exec point command after))

(defun git-rebase-todo-insert-break (&optional point after)
  "Insert an \"exec\" command on the line before POINT.
COMMAND is the command to execute.  AFTER, if non-nil, causes the
command to be inserted on the line after point."
  (interactive "d\nP")
  (git-rebase-todo-insert-command-at-point 'break point nil after))

(defvar git-rebase-todo-label-history nil)

(defun git-rebase-todo--read-label ()
  "Read a Git Rebase label from the minibuffer."
  (read-string
   (concat "Label"
           (when git-rebase-todo-label-history
             (concat " (default " (car git-rebase-todo-label-history) ")"))
           ": ")
   nil 'git-rebase-todo-label-history (car git-rebase-todo-label-history)))

(defun git-rebase-todo-insert-label (name &optional point after)
  "Insert a \"label\" command with label NAME on the line before POINT.
AFTER, if non-nil, causes the command to be inserted on the line
after point."
  (interactive (list (git-rebase-todo--read-label)
                     (point)
                     current-prefix-arg))
  (git-rebase-todo-insert-command-at-point 'label point name after))

(defun git-rebase-todo-insert-reset (name &optional point after)
  "Insert a \"reset\" command with label NAME on the line before POINT.
AFTER, if non-nil, causes the command to be inserted on the line
after point."
  (interactive (list (git-rebase-todo--read-label)
                     (point)
                     current-prefix-arg))
  (git-rebase-todo-insert-command-at-point 'reset point name after))

(defun git-rebase-todo-insert-merge (name &optional point after)
  "Insert a \"merge\" command with label NAME on the line before POINT.
AFTER, if non-nil, causes the command to be inserted on the line
after point."
  (interactive (list (git-rebase-todo--read-label)
                     (point)
                     current-prefix-arg))
  ;; TODO: Support -C/-c.  Maybe it would be better to use the prefix arg for
  ;; this like fixup, and handle inserting after the current line via a new
  ;; key binding like C-c C-M?
  (git-rebase-todo-insert-command-at-point 'merge point name after))

(defvar git-rebase-todo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<up>") #'git-rebase-todo-move-up)
    (define-key map (kbd "ESC <up>") #'git-rebase-todo-move-up)
    (define-key map (kbd "M-<down>") #'git-rebase-todo-move-down)
    (define-key map (kbd "ESC <down>") #'git-rebase-todo-move-down)
    (define-key map (kbd "C-c C-p") #'git-rebase-todo-set-pick)
    (define-key map (kbd "C-c C-r") #'git-rebase-todo-set-reword)
    (define-key map (kbd "C-c C-e") #'git-rebase-todo-set-edit)
    (define-key map (kbd "C-c C-s") #'git-rebase-todo-set-squash)
    (define-key map (kbd "C-c C-f") #'git-rebase-todo-set-fixup)
    (define-key map (kbd "C-c C-d") #'git-rebase-todo-set-drop)
    (define-key map (kbd "C-c C-x") #'git-rebase-todo-insert-exec)
    (define-key map (kbd "C-c C-b") #'git-rebase-todo-insert-break)
    (define-key map (kbd "C-c C-l") #'git-rebase-todo-insert-label)
    (define-key map (kbd "C-c C-t") #'git-rebase-todo-insert-reset)
    (define-key map (kbd "C-c C-m") #'git-rebase-todo-insert-merge)
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
