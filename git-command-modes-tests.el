;;; git-command-modes-tests.el --- Tests for git-command-modes -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Jim Porter

;; Author: Jim Porter
;; URL: https://github.com/jimporter/git-command-modes
;; Keywords: tests

;; This file is NOT part of GNU Emacs.

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

;; Tests for git-command-modes

;;; Code:

(require 'compat)
(require 'ert)
(require 'ert-x)
(require 'git-command-modes)

(defmacro git-command-font-lock-deftest (mode name text fontified)
  "Define a font-lock test for MODE named NAME.
TEXT is the text to insert into the buffer and FONTIFIED is the
expected result after fontification."
  (declare (indent 2))
  `(ert-deftest
       ,(intern (concat (string-replace "mode" "tests" (symbol-name mode))
                        "/" (symbol-name name)))
       ()
     (ert-with-test-buffer ()
       (insert ,text)
       (,mode)
       (font-lock-ensure)
       (should (equal-including-properties (buffer-string) ,fontified)))))

;;; Tests:

(git-command-font-lock-deftest git-rebase-todo-mode pick
  "pick 12345 message"
  #("pick 12345 message"
     0  4 (git-rebase-todo-command pick face git-rebase-pick-keyword)
     5 10 (face git-commit-hash)
    11 18 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode pick-short
  "p 12345 message"
  #("p 12345 message"
    0  1 (git-rebase-todo-command pick face git-rebase-pick-keyword)
    2  7 (face git-commit-hash)
    8 15 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode reword
  "reword 12345 message"
  #("reword 12345 message"
     0  6 (git-rebase-todo-command reword face git-rebase-reword-keyword)
     7 12 (face git-commit-hash)
    13 20 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode reword-short
  "r 12345 message"
  #("r 12345 message"
    0  1 (git-rebase-todo-command reword face git-rebase-reword-keyword)
    2  7 (face git-commit-hash)
    8 15 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode edit
  "edit 12345 message"
  #("edit 12345 message"
     0  4 (git-rebase-todo-command edit face git-rebase-edit-keyword)
     5 10 (face git-commit-hash)
    11 18 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode edit-short
  "e 12345 message"
  #("e 12345 message"
    0  1 (git-rebase-todo-command edit face git-rebase-edit-keyword)
    2  7 (face git-commit-hash)
    8 15 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode squash
  "squash 12345 message"
  #("squash 12345 message"
      0  6 (git-rebase-todo-command squash face git-rebase-squash-keyword)
      7 12 (face git-commit-hash)
     13 20 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode squash-short
  "s 12345 message"
  #("s 12345 message"
    0  1 (git-rebase-todo-command squash face git-rebase-squash-keyword)
    2  7 (face git-commit-hash)
    8 15 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode drop
  "drop 12345 message"
  #("drop 12345 message"
     0  4 (git-rebase-todo-command drop face git-rebase-drop-keyword)
     5 10 (face git-commit-hash)
    11 18 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode drop-short
  "d 12345 message"
  #("d 12345 message"
    0  1 (git-rebase-todo-command drop face git-rebase-drop-keyword)
    2  7 (face git-commit-hash)
    8 15 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode label
  "label my-label"
  #("label my-label"
    0  5 (git-rebase-todo-command label face git-rebase-label-keyword)
    6 14 (face git-rebase-label)))

(git-command-font-lock-deftest git-rebase-todo-mode label-short
  "l my-label"
  #("l my-label"
    0  1 (git-rebase-todo-command label face git-rebase-label-keyword)
    2 10 (face git-rebase-label)))

(git-command-font-lock-deftest git-rebase-todo-mode reset
  "reset my-label"
  #("reset my-label"
    0  5 (git-rebase-todo-command reset face git-rebase-reset-keyword)
    6 14 (face git-rebase-label)))

(git-command-font-lock-deftest git-rebase-todo-mode reset-short
  "t my-label"
  #("t my-label"
    0  1 (git-rebase-todo-command reset face git-rebase-reset-keyword)
    2 10 (face git-rebase-label)))

(git-command-font-lock-deftest git-rebase-todo-mode exec
  "exec some command"
  #("exec some command"
    0  4 (git-rebase-todo-command exec face git-rebase-exec-keyword)
    5 17 (face git-rebase-exec-command)))

(git-command-font-lock-deftest git-rebase-todo-mode exec-short
  "x some command"
  #("x some command"
    0  1 (git-rebase-todo-command exec face git-rebase-exec-keyword)
    2 14 (face git-rebase-exec-command)))

(git-command-font-lock-deftest git-rebase-todo-mode break
  "break"
  #("break"
    0 5 (git-rebase-todo-command break face git-rebase-break-keyword)))

(git-command-font-lock-deftest git-rebase-todo-mode break-short
  "b"
  #("b"
    0 1 (git-rebase-todo-command break face git-rebase-break-keyword)))

(git-command-font-lock-deftest git-rebase-todo-mode fixup
  "fixup 12345 message"
  #("fixup 12345 message"
     0  5 (git-rebase-todo-command fixup face git-rebase-fixup-keyword)
     6 11 (face git-commit-hash)
    12 19 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode fixup-short
  "f 12345 message"
  #("f 12345 message"
    0  1 (git-rebase-todo-command fixup face git-rebase-fixup-keyword)
    2  7 (face git-commit-hash)
    8 15 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode fixup/flag
  "fixup -c 12345 message"
  #("fixup -c 12345 message"
     0  5 (git-rebase-todo-command fixup face git-rebase-fixup-keyword)
     6  8 (face git-rebase-flag)
     9 14 (face git-commit-hash)
    15 22 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode fixup-short/flag
  "f -C 12345 message"
  #("f -C 12345 message"
     0  1 (git-rebase-todo-command fixup face git-rebase-fixup-keyword)
     2  4 (face git-rebase-flag)
     5 10 (face git-commit-hash)
    11 18 (face git-commit-message)))

(git-command-font-lock-deftest git-rebase-todo-mode merge
  "merge my-label"
  #("merge my-label"
    0  5 (git-rebase-todo-command merge face git-rebase-merge-keyword)
    6 14 (face git-rebase-label)))

(git-command-font-lock-deftest git-rebase-todo-mode merge-short
  "m my-label"
  #("m my-label"
    0  1 (git-rebase-todo-command merge face git-rebase-merge-keyword)
    2 10 (face git-rebase-label)))

(git-command-font-lock-deftest git-rebase-todo-mode merge/commit
  "merge -c 12345 my-label"
  #("merge -c 12345 my-label"
     0  5 (git-rebase-todo-command merge face git-rebase-merge-keyword)
     6  8 (face git-rebase-flag)
     9 14 (face git-commit-hash)
    15 23 (face git-rebase-label)))

(git-command-font-lock-deftest git-rebase-todo-mode merge-short/commit
  "m -C 12345 my-label"
  #("m -C 12345 my-label"
     0  1 (git-rebase-todo-command merge face git-rebase-merge-keyword)
     2  4 (face git-rebase-flag)
     5 10 (face git-commit-hash)
    11 19 (face git-rebase-label)))

;;; git-command-modes-tests.el ends here
