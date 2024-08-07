;;; git-command-modes.el --- Major modes for Git commands -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Jim Porter

;; Author: Jim Porter
;; URL: https://github.com/jimporter/git-command-modes
;; Version: 0.1-git
;; Keywords:
;; Package-Requires: ((emacs "27.1") (compat "29.1.0.1"))

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

(require 'compat)

(defgroup git-command-modes nil
  "Major modes for Git commands"
  :group 'vc)

(defface git-command-section-heading '((t :inherit font-lock-keyword-face))
  "Face for section headings.")

(defface git-commit-hash '((t :inherit font-lock-variable-name-face))
  "Face for commit hashes.")

(defface git-commit-message nil
  "Face for commit messages.")

(defface git-file-deleted '((t :inherit font-lock-warning-face))
  "Face for deleted file indicator.")

(defface git-file-modified '((t :inherit font-lock-variable-name-face))
  "Face for modified file indicator.")

(defface git-file-new '((t :inherit font-lock-string-face))
  "Face for new file indicator.")

(defface git-file-renamed '((t :inherit font-lock-builtin-face))
  "Face for renamed file indicator.")

(defface git-rebase-keyword '((t :inherit font-lock-keyword-face))
  "Face for keywords when rebasing.")

(defface git-rebase-pick-keyword '((t :inherit git-rebase-keyword))
  "Face for the \"pick\" keyword when rebasing.")

(defface git-rebase-reword-keyword '((t :inherit git-rebase-keyword))
  "Face for the \"reword\" keyword when rebasing.")

(defface git-rebase-edit-keyword '((t :inherit git-rebase-keyword))
  "Face for the \"edit\" keyword when rebasing.")

(defface git-rebase-squash-keyword '((t :inherit git-rebase-keyword))
  "Face for the \"squash\" keyword when rebasing.")

(defface git-rebase-drop-keyword '((t :inherit font-lock-warning-face))
  "Face for the \"drop\" keyword when rebasing.")

(defface git-rebase-label-keyword '((t :inherit git-rebase-keyword))
  "Face for the \"label\" keyword when rebasing.")

(defface git-rebase-reset-keyword '((t :inherit git-rebase-keyword))
  "Face for the \"reset\" keyword when rebasing.")

(defface git-rebase-exec-keyword '((t :inherit git-rebase-keyword))
  "Face for the \"exec\" keyword when rebasing.")

(defface git-rebase-break-keyword '((t :inherit font-lock-type-face))
  "Face for the \"break\" keyword when rebasing.")

(defface git-rebase-fixup-keyword '((t :inherit git-rebase-keyword))
  "Face for the \"fixup\" keyword when rebasing.")

(defface git-rebase-merge-keyword '((t :inherit git-rebase-keyword))
  "Face for the \"merge\" keyword when rebasing.")

(defface git-rebase-label '((t :inherit font-lock-builtin-face))
  "Face for labels when rebasing.")

(defface git-rebase-flag '((t :inherit font-lock-constant-face))
  "Face for command flags when rebasing.")

(defface git-rebase-exec-command '((t :inherit font-lock-constant-face))
  "Face for the \"exec\" command to run when rebasing.")

(defvar git-common-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for Git command files.")

(defun git-rebase-todo--font-lock-keywords (prefix)
  "Return a list of font-lock keywords for Git Rebase Todo commands.
PREFIX is a regular expression that should precede each command."
  (rx-let ((symbol (expr)
             (seq symbol-start expr symbol-end))
           (rebase-command (cmd)
             (seq (or (literal (car cmd)) (literal (cadr cmd))) symbol-end))
           (commit-sha (symbol (>= 5 hex))))
    ;; The anchored highlighters here are a bit tricky: prior to checking the
    ;; anchors, we advance point past any whitespace to the very beginning of
    ;; any potential arguments. Then, we do the same when finishing an anchored
    ;; highlighter. This means that anchors checking for arguments like "-c"
    ;; only run once, as we'd want.
    (let* ((commit-anchor
            `(,(rx point (group commit-sha)
                   (? (+ space) (group (* nonl))))
              nil nil
              (1 'git-commit-hash t)
              (2 'git-commit-message t t)))
           (label-anchor
            `(,(rx point (group (* nonl)))
              nil nil (1 'git-rebase-label t)))
           (cmds `(;; Commands taking a commit.
                   ("pick"   "p" ,commit-anchor)
                   ("reword" "r" ,commit-anchor)
                   ("edit"   "e" ,commit-anchor)
                   ("squash" "s" ,commit-anchor)
                   ("drop"   "d" ,commit-anchor)
                   ;; Commands taking a label.
                   ("label"  "l" ,label-anchor)
                   ("reset"  "t" ,label-anchor)
                   ;; Other commands.
                   ("exec"   "x"
                    (,(rx point (group (* nonl)))
                     nil nil (1 'git-rebase-exec-command t)))
                   ("break"  "b")
                   ("fixup"  "f"
                    (,(rx point (group (symbol (or "-c" "-C"))))
                     nil (re-search-forward (rx point (* space)))
                     (1 'git-rebase-flag t))
                    ,commit-anchor)
                   ("merge"  "m"
                    (,(rx point (group (symbol (or "-c" "-C"))) (+ space)
                          (group commit-sha))
                     nil (re-search-forward (rx point (* space)))
                     (1 'git-rebase-flag t)
                     (2 'git-commit-hash t))
                    ,label-anchor))))
      (mapcar (lambda (cmd)
                `(,(rx (regexp prefix) (group (rebase-command cmd)) (* space))
                  (1 '( face ,(intern (format "git-rebase-%s-keyword"
                                              (car cmd)))
                        git-rebase-todo-command ,(intern (car cmd)))
                     t)
                  ,@(cddr cmd)))
              cmds))))

(defvar git-commit-msg-font-lock-keywords
  `((,(rx line-start "#" (+ space)
          (group (or (seq "Last commands done" (* nonl))
                     (seq "Next commands to do" (* nonl))
                     (seq "Changes" (* nonl))
                     (seq "Submodule changes" (* nonl))
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
    ,@(git-rebase-todo--font-lock-keywords (rx line-start "#" (+ space))))
  "Keywords to highlight in Git Commit mode.")

(defvar-keymap git-commit-msg-mode-map)

;;;###autoload
(define-derived-mode git-commit-msg-mode text-mode "Git Commit"
  :syntax-table git-common-syntax-table
  (setq-local font-lock-keywords '(git-common-syntax-table)
              font-lock-defaults '(git-commit-msg-font-lock-keywords)
              font-lock-extra-managed-props '(git-rebase-todo-command))
  (setq-local syntax-propertize-function
              (syntax-propertize-rules (".\\(#\\)" (1 ".")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG\\'" . git-commit-msg-mode))

;;;###autoload
(define-derived-mode git-tag-msg-mode text-mode "Git Tag"
  :syntax-table git-common-syntax-table
  (setq-local font-lock-keywords '(git-common-syntax-table))
  (setq-local syntax-propertize-function
              (syntax-propertize-rules (".\\(#\\)" (1 ".")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("/TAG_EDITMSG\\'" . git-tag-msg-mode))


(defvar git-rebase-todo-font-lock-keywords
  (git-rebase-todo--font-lock-keywords (rx line-start))
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
          (let ((command-end (or (next-single-property-change
                                  (point) 'git-rebase-todo-command)
                                 (point-max))))
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
  (git-rebase-todo-replace-command-in-region 'pick beg end))

(defun git-rebase-todo-set-reword (&optional beg end)
  "Set the commands in the region from BEG to END to \"reword\"."
  (interactive (git-rebase-todo-region-or-point))
  (git-rebase-todo-replace-command-in-region 'reword beg end))

(defun git-rebase-todo-set-edit (&optional beg end)
  "Set the commands in the region from BEG to END to \"edit\"."
  (interactive (git-rebase-todo-region-or-point))
  (git-rebase-todo-replace-command-in-region 'edit beg end))

(defun git-rebase-todo-set-squash (&optional beg end)
  "Set the commands in the region from BEG to END to \"squash\"."
  (interactive (git-rebase-todo-region-or-point))
  (git-rebase-todo-replace-command-in-region 'squash beg end))

(defun git-rebase-todo-set-fixup (&optional beg end flag)
  "Set the commands in the region from BEG to END to \"fixup\".

With \\[universal-argument] prefix, set the \"-c\" flag (use the commit \
message of the
current commit for the folded commit, and allow editing the
message).  With two \\[universal-argument] prefixes, set the \"-C\" flag (use \
the
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

(defvar-keymap git-rebase-todo-mode-map
  "M-<up>"     #'git-rebase-todo-move-up
  "ESC <up>"   #'git-rebase-todo-move-up
  "M-<down>"   #'git-rebase-todo-move-down
  "ESC <down>" #'git-rebase-todo-move-down
  "C-c C-p"    #'git-rebase-todo-set-pick
  "C-c C-r"    #'git-rebase-todo-set-reword
  "C-c C-e"    #'git-rebase-todo-set-edit
  "C-c C-s"    #'git-rebase-todo-set-squash
  "C-c C-f"    #'git-rebase-todo-set-fixup
  "C-c C-d"    #'git-rebase-todo-set-drop
  "C-c C-x"    #'git-rebase-todo-insert-exec
  "C-c C-b"    #'git-rebase-todo-insert-break
  "C-c C-l"    #'git-rebase-todo-insert-label
  "C-c C-t"    #'git-rebase-todo-insert-reset
  "C-c C-m"    #'git-rebase-todo-insert-merge)

(easy-menu-define git-rebase-todo-menu-map git-rebase-todo-mode-map
  "Menu for Git Rebase buffers."
  '("Git Rebase"
    ["Move Command Up" git-rebase-todo-move-up
     :help "Move the command at point up one line"]
    ["Move Command Down" git-rebase-todo-move-down
     :help "Move the command at point down one line"]))

;;;###autoload
(define-derived-mode git-rebase-todo-mode nil "Git Rebase"
  :syntax-table git-common-syntax-table
  (setq-local font-lock-keywords '(git-common-syntax-table)
              font-lock-defaults '(git-rebase-todo-font-lock-keywords)
              font-lock-extra-managed-props '(git-rebase-todo-command))
  (setq-local syntax-propertize-function
              (syntax-propertize-rules (".\\(#\\)" (1 ".")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("/git-rebase-todo\\'" . git-rebase-todo-mode))

(provide 'git-command-modes)
;;; git-command-modes.el ends here
