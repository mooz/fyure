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

;; Commands

(defvar fyure:source-directory (if load-file-name
                                   (file-name-directory load-file-name)
                                 default-directory))

(defvar fyure:python-executable "python")

(defvar fyure:checker-command
  (list fyure:python-executable
        (concat fyure:source-directory "fyure.py")))

;; Keymap

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
    (define-key map "^" 'backup)
    (define-key map "\C-g" 'quit)
    (define-key map "\C-]" 'quit)
    (define-key map "\e" 'exit-prefix)
    (define-key map [escape] 'exit-prefix)
    map)
  "Keymap that defines the responses to questions in `fyure:query-replace-position'.
The \"bindings\" in this map are not commands; they are answers.
The valid answers include `act', `skip', `exit', `act-and-exit', `recenter',
`automatic', `backup', and `exit-prefix'.")

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

(defmacro fyure:highlight-overlay (overlay-variable begin end)
  `(if ,overlay-variable
       (move-overlay ,overlay-variable ,begin ,end)
     ;; Create highlight
     (setq ,overlay-variable (make-overlay ,begin ,end))
     (overlay-put ,overlay-variable 'priority 1001)
     (overlay-put ,overlay-variable 'face 'query-replace)))

(defmacro fyure:helm-or-anything (&rest args)
  `(cond ((and (featurep 'helm) (fboundp 'helm))
          (helm ,@args))
         ((and (featurep 'anything) (fboundp 'anything))
          (anything ,@args))
         (t
          (error "Neither `helm' nor `anything' found (required for fyure.el)"))))

;; ------------------------------------------------------------ ;;
;; Query-replace like replacing mechanism
;; ------------------------------------------------------------ ;;

(defun fyure:do-replace (replacement from-string-begin from-string-end)
  (delete-region from-string-begin from-string-end)
  (goto-char from-string-begin)
  (insert replacement))

(defsubst fyure:byte-to-position (buffer byte)
  (with-current-buffer buffer
    (byte-to-position (1+ byte))))

(defun fyure:query-replace-position (replacement from-positions)
  (fyure:with-duplicated-buffer untouched-buffer
    (let ((offset 0)
          (replace-overlay)
          (replacement-length (length replacement))
          (automatic-mode)
          (number-of-replace 0))
      (unwind-protect
          (loop named asking-loop for (from-byte . to-byte) in from-positions
                do (let* ((from-position (fyure:byte-to-position untouched-buffer from-byte))
                          (to-position (fyure:byte-to-position untouched-buffer to-byte))
                          (from-string-length (- to-position from-position))
                          (from-string-begin (+ from-position offset))
                          (from-string-end (+ to-position offset))
                          (command))
                     (if automatic-mode
                         (setq command 'act)
                       ;; Ask command
                       (fyure:highlight-overlay replace-overlay from-string-begin from-string-end)
                       (goto-char from-string-begin)
                       (message "fyure: Replace `%s' => `%s'?"
                                (buffer-substring from-string-begin
                                                  from-string-end)
                                replacement)
                       (setq command (lookup-key fyure:ask-command-map
                                                 (vector (read-event)))))
                     ;; Automatic -> Act
                     (when (eq command 'automatic)
                       (setq automatic-mode t)
                       (setq command 'act))
                     ;; Command specific deeds
                     (case command
                       ((act act-and-exit)
                        (fyure:do-replace replacement from-string-begin from-string-end)
                        (setq offset (+ offset (- replacement-length from-string-length)))
                        (setq number-of-replace (1+ number-of-replace)))
                       ('recenter
                        (recenter nil)))
                     ;; Exit?
                     (case command
                       ((exit act-and-exit)
                        (return-from 'asking-loop)))))
        ;; Finally, delete highlights
        (when replace-overlay
          (delete-overlay replace-overlay)))
      (when (> number-of-replace 0)
        (message "fyure: Replaced %d words" number-of-replace)))))

;; ------------------------------------------------------------ ;;
;; fyure.py integration
;; ------------------------------------------------------------ ;;

;; Target-list handling

(defun fyure:get-candidates-list-from-result-alist (result-alist)
  (loop for (key . hyoki-alist) in result-alist
        collect (format "%s\n   %s"
                        key
                        (mapconcat 'identity (fyure:get-hyoki-list hyoki-alist) ", "))))

(defun fyure:select-target (result-alist next)
  (fyure:helm-or-anything
   :sources
   '((name . "Select target word-group")
     (candidates . (lambda () (fyure:get-candidates-list-from-result-alist result-alist)))
     (multiline)
     (display-to-real . (lambda (target-verbose) (nth 0 (split-string target-verbose "\n"))))
     (action . (("Select hyoki for the target-group" . (lambda (target) (funcall next target))))))))

;; Hyoki-list handling

(defun fyure:get-hyoki-list (hyoki-alist)
  (setq hyoki-alist
        (sort (copy-sequence hyoki-alist)
              (lambda (a b) (> (length (cdr a)) (length (cdr b))))))
  (loop for (hyoki . positions) in hyoki-alist
        collect (format "%s(%d)" hyoki (length positions))))

(defun fyure:select-preferred-hyoki-for-target (result-alist target-name next)
  (let ((hyoki-alist (cdr (assoc target-name result-alist))))
    (when hyoki-alist
      (fyure:helm-or-anything
       :sources
       '((name . "Select preferred hyoki")
         (candidates . (lambda () (fyure:get-hyoki-list hyoki-alist)))
         (display-to-real . (lambda (hyoki-verbose) (nth 0 (split-string hyoki-verbose "("))))
         (action . (("Fix occurrence" . (lambda (selected) (funcall next selected hyoki-alist))))))))))

;; Python command invocation

(defun fyure:get-result-alist-for-current-buffer ()
  (read (with-output-to-string
          (apply 'call-process-region
                 (append (list (point-min)
                               (point-max)
                               (car fyure:checker-command)
                               nil standard-output nil)
                         ;; args
                         (cdr fyure:checker-command))))))

;; Result formatting

(defun fyure:collect-replacee-positions (preferred-hyoki hyoki-alist)
  "Merge position list and sort"
  (sort
   (loop for (hyoki . positions) in hyoki-alist
         unless (string= hyoki preferred-hyoki)
         append positions)
   (lambda (a b) (< (car a) (car b)))))

;; Main command

(defun fyure:start-fixing ()
  (interactive)
  (let ((result-alist (fyure:get-result-alist-for-current-buffer)))
    (if result-alist
        (fyure:select-target
         result-alist
         (lambda (target)
           (fyure:select-preferred-hyoki-for-target
            result-alist
            target
            (lambda (preferred-hyoki hyoki-alist)
              (fyure:query-replace-position preferred-hyoki
                                            (fyure:collect-replacee-positions preferred-hyoki
                                                                              hyoki-alist))))))
      ;; otherwise
      (message "fyure: No fixing candidates"))))

(provide 'fyure)
;;; fyure.el ends here
