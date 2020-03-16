;;; php-cs-fixer.el --- php-cs-fixer wrapper.

;;; License:
;; Copyright 2015 OVYA (Renée Costes Group), 2020 Hugo Sales. All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

;;; Author: Hugo Sales, Philippe Ivaldi for OVYA
;; Source: Some pieces of code are copied from go-mode.el https://github.com/dominikh/go-mode.el
;; Version: 1.0Beta2
;; Keywords: languages php
;; Package-Requires: ((cl-lib "0.5"))
;; URL: https://github.com/OVYA/php-cs-fixer
;;
;;; Commentary:
;; This file is not part of GNU Emacs.
;; See the file README.org for further information.

;;; Code:

(require 'cl-lib)

;;;###autoload
(defgroup php-cs-fixer nil
  "php-cs-fixer wrapper."
  :tag "PHP"
  :prefix "php-cs-fixer-"
  :group 'languages
  :link '(url-link :tag "Source code repository" "https://github.com/OVYA/php-cs-fixer")
  :link '(url-link :tag "Executable dependency" "https://github.com/FriendsOfPHP/PHP-CS-Fixer"))

(defcustom php-cs-fixer-command "php-cs-fixer"
  "The 'php-cs-fixer' command."
  :type 'string
  :group 'php-cs-fixer)

(defcustom php-cs-fixer-config-option nil
  "The 'php-cs-fixer' config option.
If not nil `php-cs-rules-level-part-options`
and `php-cs-rules-fixer-part-options` are not used."
  :type 'string
  :group 'php-cs-fixer)

(defcustom php-cs-fixer-rules-level-part-options '("@Symfony")
  "The 'php-cs-fixer' --rules base part options."
  :type '(repeat
          (choice
           ;; (const :tag "Not set" :value nil)
           (const :value "@PSR0")
           (const :value "@PSR1")
           (const :value "@PSR2")
           (const :value "@Symfony")
           (const :value "@Symfony::risky")
           (const :value "@PHP70Migration:risky")
           ))
  :group 'php-cs-fixer)

(defcustom php-cs-fixer-rules-fixer-part-options
  '("no_multiline_whitespace_before_semicolons" "concat_space")
  "The 'php-cs-fixer' --rules part options.
These options are not part of `php-cs-fixer-rules-level-part-options`."
  :type '(repeat string)
  :group 'php-cs-fixer)

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
      php-cs-fixer-is-command-ok-var
    (message "Testing php-cs-fixer existence and version...")
    (setq php-cs-fixer-command "php-cs-fixer")
    (if (and (executable-find php-cs-fixer-command)
             (string-match ".+ 2.[0-9]+.*"
                           (shell-command-to-string
                            (concat php-cs-fixer-command " --version"))))
        (setq php-cs-fixer-is-command-ok-var t)
      (warn php-cs-fixer-command-bad-version-msg))
    (php-cs-fixer-is-command-ok-var)))

;;;###autoload
(defun php-cs-fixer-fix ()
  "Formats the current PHP buffer according to the PHP-CS-Fixer tool."
  (interactive)

  (when (php-cs-fixer--is-command-ok)
    (when (zerop (call-process php-cs-fixer-command
                               nil nil nil
                               "fix"
                               (if php-cs-fixer-config-option
                                   (concat "--config="
                                           (shell-quote-argument
                                            (expand-file-name php-cs-fixer-config-option)))
                                 (php-cs-fixer--build-rules-options))
                               "--using-cache=no"
                               "--quiet"
                               (buffer-file-name)))
      (progn (revert-buffer t t t)
             (message "Applied php-cs-fixer")))))

;;;###autoload
(defun php-cs-fixer-after-save ()
  "Used to automatically fix the file saving the buffer.
Add this to .emacs to run php-cs-fix on the current buffer when saving:
 (add-hook 'after-save-hook 'php-cs-fixer-before-save)."
  (when (eq major-mode 'php-mode)
    (progn
      (php-cs-fixer-fix))))

(provide 'php-cs-fixer)

;;; php-cs-fixer ends here
