;;; php-cs-fixer.el --- The php-cs-fixer wrapper -*- lexical-binding: t; -*-

;;; License:
;; Copyright 2015 OVYA (Renée Costes Group). All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

;;; Author: Philippe Ivaldi for OVYA
;; Source: Some pieces of code are copied from go-mode.el https://github.com/dominikh/go-mode.el
;; Version: 2.1.0
;; Keywords: languages php
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/pivaldi/php-cs-fixer
;;
;;; Commentary:
;; This file is not part of GNU Emacs.
;; See the file README.org for further information.

;;; Code:

(require 'cl-lib)

;;;###autoload
(defgroup php-cs-fixer nil
  "The php-cs-fixer wrapper group."
  :tag "PHP"
  :prefix "php-cs-fixer-"
  :group 'languages
  :link '(url-link :tag "Source code repository" "https://github.com/OVYA/php-cs-fixer")
  :link '(url-link :tag "Executable dependency" "https://github.com/FriendsOfPHP/PHP-CS-Fixer"))

(defcustom php-cs-fixer-command "php-cs-fixer"
  "The php-cs-fixer command."
  :type 'string
  :group 'php-cs-fixer)

(defcustom php-cs-fixer-config-option nil
  "The php-cs-fixer config option.
If not nil `php-cs-rules-level-part-options`
and `php-cs-rules-fixer-part-options` are not used."
  :type 'string
  :group 'php-cs-fixer)

(defcustom php-cs-fixer-rules-level-part-options '("@Symfony")
  "The php-cs-fixer --rules base part options."
  :type '(repeat
          (choice
           ;; (const :tag "Not set" :value nil)
           (const :value "@DoctrineAnnotation")
           (const :value "@PHP54Migration")
           (const :value "@PHP56Migration:risky")
           (const :value "@PHP70Migration")
           (const :value "@PHP70Migration:risky")
           (const :value "@PHP71Migration")
           (const :value "@PHP71Migration:risky")
           (const :value "@PHP73Migration")
           (const :value "@PHP74Migration")
           (const :value "@PHP74Migration:risky")
           (const :value "@PHP80Migration")
           (const :value "@PHP80Migration:risky")
           (const :value "@PHP81Migration")
           (const :value "@PHPUnit30Migration:risky")
           (const :value "@PHPUnit32Migration:risky")
           (const :value "@PHPUnit35Migration:risky")
           (const :value "@PHPUnit43Migration:risky")
           (const :value "@PHPUnit48Migration:risky")
           (const :value "@PHPUnit50Migration:risky")
           (const :value "@PHPUnit52Migration:risky")
           (const :value "@PHPUnit54Migration:risky")
           (const :value "@PHPUnit55Migration:risky")
           (const :value "@PHPUnit56Migration:risky")
           (const :value "@PHPUnit57Migration:risky")
           (const :value "@PHPUnit60Migration:risky")
           (const :value "@PHPUnit75Migration:risky")
           (const :value "@PHPUnit84Migration:risky")
           (const :value "@PSR1")
           (const :value "@PSR12")
           (const :value "@PSR12:risky")
           (const :value "@PSR2")
           (const :value "@PhpCsFixer")
           (const :value "@PhpCsFixer:risky")
           (const :value "@Symfony")
           (const :value "@Symfony:risky")))
  :group 'php-cs-fixer)

(defcustom php-cs-fixer-rules-fixer-part-options
  '("multiline_whitespace_before_semicolons" "concat_space")
  "The php-cs-fixer --rules part options.
These options are not part of `php-cs-fixer-rules-level-part-options`."
  :type '(repeat string)
  :group 'php-cs-fixer)

(defcustom php-cs-fixer-fix-popup-on-error nil
  "Should `php-cs-fixer-fix` popup an error buffer on error ?"
  :type 'boolean
  :group 'php-cs-fixer)


;; Copy of go--goto-line from https://github.com/dominikh/go-mode.el
(defun php-cs-fixer--goto-line (line)
  "Private goto line to LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun php-cs-fixer--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring`.
Derived from the function `kill-whole-line'.
ARG is defined as for that function."
  (delete-region
   (progn (forward-line 0) (point))
   (progn (forward-line (or arg 0)) (point))))

;; Derivated of go--apply-rcs-patch from https://github.com/dominikh/go-mode.el
(defun php-cs-fixer--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in php-cs-fixer--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (php-cs-fixer--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (php-cs-fixer--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in php-cs-fixer--apply-rcs-patch")))))))))

(defun php-cs-fixer--kill-error-buffer (errbuf)
  "Private function that kill the error buffer ERRBUF."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

(defun php-cs-fixer--build-rules-options ()
  "Private method to build the --rules options."
  (if php-cs-fixer-config-option ""
    (let ((base-opts
           (concat
            (if php-cs-fixer-rules-level-part-options
                (mapconcat 'identity php-cs-fixer-rules-level-part-options ",")
              nil)))
          (other-opts (if php-cs-fixer-rules-fixer-part-options (concat "," (mapconcat 'identity php-cs-fixer-rules-fixer-part-options ",")) nil)))
      (concat
       "--rules=" base-opts
       (if other-opts other-opts "")))))

(defvar php-cs-fixer-command-not-found-msg "Command php-cs-fixer not found.
Fix this issue removing the Emacs package php-cs-fixer or installing the program php-cs-fixer")

(defvar php-cs-fixer-command-bad-version-msg "Command php-cs-fixer version not supported.
Fix this issue removing the Emacs package php-cs-fixer or updating the program php-cs-fixer to version 2.*")

(defvar php-cs-fixer-is-command-ok-var nil)

(defun php-cs-fixer--is-command-ok ()
  "Private Method.
Return t if the command `php-cs-fixer-command`
is available and supported by this package,  return nil otherwise.
The test is done at first call and the same result will returns
for the next calls."
  (if php-cs-fixer-is-command-ok-var
      (= 1 php-cs-fixer-is-command-ok-var)
    (progn
      (message "Testing php-cs-fixer existence and version...")
      (setq php-cs-fixer-is-command-ok-var 0)

      (if (executable-find php-cs-fixer-command)
          (if (string-match ".+ [2-3].[0-9]+.*"
                            (shell-command-to-string
                             (concat php-cs-fixer-command " --version")))
              (progn (setq php-cs-fixer-is-command-ok-var 1) t)
            (progn
              (warn php-cs-fixer-command-bad-version-msg)
              nil))
        (progn (warn php-cs-fixer-command-not-found-msg) nil)))))

;;;###autoload
(defun php-cs-fixer-fix ()
  "Formats the current PHP buffer according to the PHP-CS-Fixer tool."
  (interactive)
  (when (php-cs-fixer--is-command-ok)
    (let* ((tmpfile (make-temp-file "PHP-CS-Fixer-" nil ".php"))
           (filename (buffer-file-name))
           (date (format-time-string "%Y-%m-%d %H:%M:%S %Z" (current-time)))
           (patchbuf (get-buffer-create "*PHP-CS-Fixer patch*"))
           (errbuf (get-buffer-create
                    (format
                     "*PHP-CS-Fixer Errors %s*"
                     (substring (md5 filename) 0 10))))
           (coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8)
           (errorp nil))
      (save-restriction
        (widen)
        (if errbuf
            (with-current-buffer errbuf
              (setq buffer-read-only nil)
              (erase-buffer)
              (insert (format "%s - An error occurs fixing the file `%s`\n\n" date filename))))
        (with-current-buffer patchbuf
          (erase-buffer))

        (write-region nil nil tmpfile)

        ;; We're using errbuf for the mixed stdout and stderr output. This
        ;; is not an issue because  php-cs-fixer -q does not produce any stdout
        ;; output in case of success.
        (if (and (zerop (call-process "php" nil errbuf nil "-l" tmpfile))
                 (zerop (call-process php-cs-fixer-command
                                      nil errbuf nil
                                      "fix"
                                      (if php-cs-fixer-config-option
                                          (concat "--config=" (shell-quote-argument php-cs-fixer-config-option))
                                        (php-cs-fixer--build-rules-options))
                                      "--using-cache=no"
                                      "--quiet"
                                      tmpfile)))
            (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                (message "Buffer is already php-cs-fixed")
              (progn
                (php-cs-fixer--apply-rcs-patch patchbuf)
                (message "Applied php-cs-fixer")))
          (progn
            (if php-cs-fixer-fix-popup-on-error
                (let ((window (display-buffer errbuf '(nil (allow-no-window . t)))))
                  (setq errorp t)
                  (set-window-point window 1)
                  (set-window-dedicated-p window t)
                  (and (window-full-width-p window)
                       (not (eq window (frame-root-window (window-frame window))))
                       (save-excursion
                         (save-selected-window
                           (select-window window)
                           (enlarge-window (- (* 2 window-min-height) (window-height)))))))
              (warn (with-current-buffer errbuf (buffer-string)))))))
      (unless errorp (php-cs-fixer--kill-error-buffer errbuf))
      (kill-buffer patchbuf)
      (delete-file tmpfile))))

;; Prevents warning reference to free variable.
(defvar geben-temporary-file-directory)

;;;###autoload
(defun php-cs-fixer-before-save ()
  "Used to automatically fix the file saving the buffer.
Add this to .emacs to run php-cs-fix on the current buffer when saving:
 (add-hook \\='before-save-hook \\='php-cs-fixer-before-save)."
  (interactive)
  (when (and
         buffer-file-name
         (string= (file-name-extension buffer-file-name) "php")
         (or (not (boundp 'geben-temporary-file-directory))
             (not (string-match geben-temporary-file-directory (file-name-directory buffer-file-name)))))
    (php-cs-fixer-fix)))

(provide 'php-cs-fixer)
;;; php-cs-fixer.el ends here
