;;; fyure.el --- An interface to fix Japanese hyoki-yure

;; Copyright (C) 2012  Masafumi Oyamada

;; Author: Masafumi Oyamada <stillpedant@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Place `fyure.el' and `fyure.py' into a directory in `load-path',
;; and add the following code into your .emacs file.
;;
;; (autoload 'fyure:start-fixing "fyure" "An interface to fix Japanese hyoki-yure." t)
;;
;; Then, type "M-x fyure:start-fixing" in a document you want to fix.

;;; Code:

(eval-when-compile (require 'cl))

;; ------------------------------------------------------------ ;;
;; Variables
;; ------------------------------------------------------------ ;;

;; fyure.py

(defvar fyure:source-directory (if load-file-name
                                   (file-name-directory load-file-name)
                                 default-directory))

(defvar fyure:python-executable "python")

(defvar fyure:mecab-dictionary-path nil "Path to the mecab dict")

(defvar fyure:custom-checker-command nil "List that indictes custom checker command (program arg1 arg2 ...)")

(defmacro* fyure:build-checker-command (&optional (args (list fyure:mecab-dictionary-path)))
  `(or fyure:custom-checker-command
       (append (list fyure:python-executable
                     (concat fyure:source-directory "fyure.py"))
               ,@args)))

;; Custom

(defvar fyure:follow-mode t
  "When this value is t, enable `helm-follow-mode' by default.")

;; Keymap

(defconst fyure:ask-mode-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
RET or `q' to exit, Period to replace one match and exit,
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions"
  "Help message while in `query-replace'.")

(defvar fyure:ask-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'act)
    (define-key map "\d" 'skip)
    (define-key map [delete] 'skip)
    (define-key map [backspace] 'skip)
    (define-key map "y" 'act)
    (define-key map "n" 'skip)
    (define-key map "Y" 'act)
    (define-key map "N" 'skip)
    (define-key map "q" 'exit)
    (define-key map "\r" 'exit)
    (define-key map [return] 'exit)
    (define-key map "." 'act-and-exit)
    (define-key map "\C-l" 'recenter)
    (define-key map "!" 'automatic)
    (define-key map "\C-h" 'help)
    (define-key map [f1] 'help)
    (define-key map [help] 'help)
    (define-key map "?" 'help)
    (define-key map "\C-g" 'quit)
    (define-key map "\C-]" 'quit)
    (define-key map "\e" 'exit-prefix)
    (define-key map [escape] 'exit-prefix)
    map)
  "Keymap that defines the responses to questions in `fyure:query-replace-position'.
The \"bindings\" in this map are not commands; they are answers.
The valid answers include `act', `skip', `exit', `act-and-exit', `recenter',
`automatic', and `exit-prefix'.")

;; Face

(defvar fyure:highlight-face 'query-replace)

;; ------------------------------------------------------------ ;;
;; Helper
;; ------------------------------------------------------------ ;;

(defmacro fyure:with-duplicated-buffer (duplicated-buffer &rest body)
  ""
  (declare (indent 1) (debug t))
  `(let ((original-buffer (current-buffer)))
     (with-temp-buffer
       (let ((,duplicated-buffer (current-buffer)))
         (insert-buffer-substring original-buffer)
         (with-current-buffer original-buffer
           ,@body)))))

(defun fyure:helm-after-initialization ()
  (defvar helm-buffer)
  (defvar helm-follow-mode)
  (with-current-buffer helm-buffer
    (setq helm-follow-mode fyure:follow-mode)))

(defun fyure:anything-after-initialization ()
  (defvar anything-buffer)
  (defvar anything-follow-mode)
  (with-current-buffer anything-buffer
    (setq anything-follow-mode fyure:follow-mode)))

(defmacro fyure:helm (&rest args)
  `(cond ((and (featurep 'helm) (fboundp 'helm))
          (unwind-protect (progn
                            (add-hook 'helm-after-initialize-hook 'fyure:helm-after-initialization t)
                            (helm ,@args))
            (remove-hook 'helm-after-initialize-hook 'fyure:helm-after-initialization)))
         ((and (featurep 'anything) (fboundp 'anything))
          (unwind-protect (progn
                            (add-hook 'anything-after-initialize-hook 'fyure:anything-after-initialization t)
                            (anything ,@args))
            (remove-hook 'anything-after-initialize-hook 'fyure:anything-after-initialization)))
         (t
          (error "Neither `helm' nor `anything' found (required for fyure.el)"))))

;; ------------------------------------------------------------ ;;
;; Highlighting
;; ------------------------------------------------------------ ;;

(defsubst fyure:highlight-overlay (begin end &optional overlay-variable face)
  (if overlay-variable
      (move-overlay overlay-variable begin end) ; reuse
    ;; create
    (setq overlay-variable (make-overlay begin end))
    (overlay-put overlay-variable 'face (if face (quote face) fyure:highlight-face)))
  overlay-variable)

(defvar fyure:highlights nil)

(defun fyure:push-highlight (begin end &optional face)
  (push (fyure:highlight-overlay begin end)
        fyure:highlights))

(defun fyure:pop-highlight ()
  (pop fyure:highlights))

(defun fyure:clear-highlights ()
  (loop for highlight in fyure:highlights
        do (when (overlayp highlight) (delete-overlay highlight)))
  (setq fyure:highlights nil))

;; ------------------------------------------------------------ ;;
;; Query-replace like replacing mechanism
;; ------------------------------------------------------------ ;;

(defun fyure:do-replace (replacement from-string-begin from-string-end)
  (delete-region from-string-begin from-string-end)
  (goto-char from-string-begin)
  (insert replacement))

(defsubst fyure:byte-to-position (byte &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (byte-to-position (1+ byte))))

(defun fyure:query-replace-position (replacement from-positions)
  (fyure:with-duplicated-buffer untouched-buffer
    (let ((offset 0)
          (replace-overlay)
          (replacement-length (length replacement))
          (automatic-mode)
          (number-of-replace 0)
          (exit-flag))
      (unwind-protect
          (loop named asking-loop for (from-byte . to-byte) in from-positions
                unless exit-flag
                do (let* ((from-position (fyure:byte-to-position from-byte untouched-buffer))
                          (to-position (fyure:byte-to-position to-byte untouched-buffer))
                          (from-string-length (- to-position from-position))
                          (from-string-begin (+ from-position offset))
                          (from-string-end (+ to-position offset))
                          (command)
                          (end-ask-loop))
                     (while (not end-ask-loop)
                       (setq end-ask-loop t)
                       (if automatic-mode
                           (setq command 'act)
                         ;; Ask command
                         (setq replace-overlay
                               (fyure:highlight-overlay from-string-begin from-string-end replace-overlay))
                         (goto-char from-string-begin)
                         (message "fyure: Replace `%s' => `%s'? [`C-h`, `?', `[f1]` for help]"
                                  (buffer-substring from-string-begin
                                                    from-string-end)
                                  replacement)
                         (setq command (or (lookup-key fyure:ask-command-map
                                                       (vector (read-event)))
                                           'exit)))
                       ;; Command -> Act
                       (when (eq command 'automatic)
                         (setq automatic-mode t)
                         (setq command 'act))
                       ;; Command specific deeds
                       (case command
                         ;; Act
                         ((act act-and-exit)
                          (fyure:do-replace replacement from-string-begin from-string-end)
                          (setq offset (+ offset (- replacement-length from-string-length)))
                          (setq number-of-replace (1+ number-of-replace)))
                         ;; Recenter
                         ('recenter
                          (setq end-ask-loop nil)
                          (recenter nil))
                         ;; Help
                         ('help
                          (setq end-ask-loop nil)
                          (with-output-to-temp-buffer "*Help*"
                            (princ (substitute-command-keys fyure:ask-mode-help))
                            (with-current-buffer standard-output (help-mode)))))
                       ;; Exit?
                       (case command
                         ((exit act-and-exit)
                          (setq exit-flag t))))))
        ;; Finally, delete highlights
        (when replace-overlay
          (delete-overlay replace-overlay)))
      number-of-replace)))

;; ------------------------------------------------------------ ;;
;; fyure.py integration
;; ------------------------------------------------------------ ;;

;; Target-list handling

(defsubst fyure:get-hyoki-alist-for-target (target result-alist)
  (assoc-default target result-alist))

(defsubst fyure:get-replacee-positions-for-target (target result-alist &optional except-hyoki)
  (fyure:collect-replacee-positions (fyure:get-hyoki-alist-for-target target
                                                                      result-alist)
                                    except-hyoki))

(defun fyure:highlight-target (target result-alist)
  (fyure:clear-highlights)
  (let* ((replacee-positions (fyure:get-replacee-positions-for-target target result-alist))
         (first-target (car replacee-positions))
         (first-target-begin (fyure:byte-to-position (car first-target)))
         (first-target-end (fyure:byte-to-position (cdr first-target))))
    (goto-char first-target-begin)
    (fyure:push-highlight first-target-begin first-target-end)))

(defun fyure:decorate-target (target hyoki-alist)
  (format "%s\n   %s"
          target
          (mapconcat 'identity (fyure:get-hyoki-list hyoki-alist) ", ")))

(defun fyure:undecorate-target (decorated-target)
  (nth 0 (split-string decorated-target "\n")))

(defun fyure:get-candidates-list-from-result-alist (result-alist)
  (let ((results (loop for (target . hyoki-alist) in result-alist
                       collect (fyure:decorate-target target hyoki-alist))))
    results))

(defun fyure:select-target (result-alist next)
  (fyure:helm
   :sources
   '((name . "Select target word-group")
     (persistent-action . (lambda (decorated-target)
                            (fyure:highlight-target (fyure:undecorate-target decorated-target)
                                                    result-alist)))
     (cleanup . fyure:clear-highlights)
     (candidates . (lambda () (fyure:get-candidates-list-from-result-alist result-alist)))
     (multiline)
     (display-to-real . (lambda (decorated-target) (fyure:undecorate-target decorated-target)))
     (action . (lambda (target) (funcall next target))))))

;; Hyoki-list handling

(defun fyure:highlight-hyoki (hyoki hyoki-alist)
  (fyure:clear-highlights)
  (let* ((replacee-positions (assoc-default hyoki hyoki-alist))
         (first-target (car replacee-positions))
         (first-target-begin (fyure:byte-to-position (car first-target)))
         (first-target-end (fyure:byte-to-position (cdr first-target))))
    (goto-char first-target-begin)
    (fyure:push-highlight first-target-begin first-target-end)))

(defun fyure:decorate-hyoki (hyoki positions)
  (format "%s(%d)" hyoki (length positions)))

(defun fyure:undecorate-hyoki (decorated-hyoki)
  (nth 0 (split-string decorated-hyoki "(")))

(defun fyure:get-hyoki-list (hyoki-alist)
  (setq hyoki-alist
        (sort (copy-sequence hyoki-alist)
              (lambda (a b) (> (length (cdr a)) (length (cdr b))))))
  (loop for (hyoki . positions) in hyoki-alist
        collect (fyure:decorate-hyoki hyoki positions)))

(defun fyure:select-preferred-hyoki-for-target (result-alist target next)
  (let ((hyoki-alist (fyure:get-hyoki-alist-for-target target result-alist)))
    (when hyoki-alist
      (fyure:helm
       :sources
       '((name . "Select preferred representation")
         (persistent-action . (lambda (decorated-hyoki)
                                (fyure:highlight-hyoki (fyure:undecorate-hyoki decorated-hyoki)
                                                       hyoki-alist)))
         (cleanup . fyure:clear-highlights)
         (candidates . (lambda () (fyure:get-hyoki-list hyoki-alist)))
         (display-to-real . (lambda (decorated-hyoki) (fyure:undecorate-hyoki decorated-hyoki)))
         (action . (lambda (selected) (funcall next selected hyoki-alist))))))))

;; Python command invocation

(defun fyure:get-result-alist-for-current-buffer ()
  (let ((checker-command (fyure:build-checker-command)))
    (read (with-output-to-string
            (apply 'call-process-region
                   (append (list (point-min)
                                 (point-max)
                                 (car checker-command)
                                 nil standard-output nil)
                           ;; args
                           (cdr checker-command)))))))

;; Result formatting

(defun fyure:collect-replacee-positions (hyoki-alist &optional except-hyoki)
  "Merge position list and sort"
  (sort
   (loop for (hyoki . positions) in hyoki-alist
         unless (and except-hyoki (string= hyoki except-hyoki))
         append positions)
   (lambda (a b) (< (car a) (car b)))))

;; Main command

(defun fyure:start-fixing ()
  (interactive)
  (let ((result-alist (fyure:get-result-alist-for-current-buffer))
        (number-of-replace 0))
    (if result-alist
        (unwind-protect
            (fyure:select-target
             result-alist
             (lambda (target)
               (fyure:select-preferred-hyoki-for-target
                result-alist
                target
                (lambda (preferred-hyoki hyoki-alist)
                  (setq number-of-replace
                        (fyure:query-replace-position preferred-hyoki
                                                      (fyure:collect-replacee-positions hyoki-alist
                                                                                        preferred-hyoki)))))))
          (fyure:clear-highlights))
      ;; otherwise
      (message "fyure: No fixing candidates"))
    (when (> number-of-replace 0)
      (message "fyure: Replaced %d words" number-of-replace))))

(provide 'fyure)
;;; fyure.el ends here
