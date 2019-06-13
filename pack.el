;;; pack.el --- Pack and unpack archive files  -*- lexical-binding: t -*-

;; Author: 10sr <8.slashes@gmail.com>
;; URL: https://github.com/10sr/pack-el
;; Version: 0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Keywords: files dired

;; This file is not part of GNU Emacs.

;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at

;;
;;   http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;;; Commentary:

;; Provides some commands and functions to pack and unpack
;; archives in a simple way.

;; Commands to pack/unpack archive files can be defined by setting `pack-program-alist'
;; variable.

;; Use from Dired
;; --------------

;; To pack/unpack files from dired buffers, add following to your dired confiugration:

;; (define-key dired-mode-map "P" 'pack-dired-dwim))

;; This command creates an archive file from marked files, or unpack the file when
;; only one file is selected and that has an extension for archive.

;;; Code:

(eval-when-compile
  (require 'simple))

(require 'cl-lib)

(declare-function dired-dwim-target-directory "dired-aux")
(declare-function dired-get-marked-files "dired")

(defgroup pack nil
  "Pack and unpack archive files."
  :tag "Pack"
  :prefix "pack-"
  :group 'tools)

(defcustom pack-buffer-name "*Pack*"
  "Buffer name for `pack' process."
  :type 'string
  :group 'pack)

(defcustom pack-dired-default-extension
  ".tgz"
  "Default suffix for `pack-dired-do-pack' functions.
Filename with this suffix must matches one of the cars in
`pack-program-alist'."
  :type 'string
  :group 'pack)

(defcustom pack-program-alist
  `(
    ("\\.7z\\'"
     :pack ("7z" "a" archive sources)
     :unpack ("7z" "x" archive))
    ("\\.zip\\'"
     :pack ("zip" "-r" archive sources)
     :unpack ("unzip" archive))
    ("\\.tar\\'"
     :pack ("tar" "-cf" archive sources)
     :unpack ("tar" "-xf" archive))
    ("\\.tgz\\'"
     :pack ("tar" "-czf" archive sources)
     :unpack ("tar" "-xf" archive))
    ("\\.tar\\.gz\\'"
     :pack ("tar" "-czf" archive sources)
     :unpack ("tar" "-xf" archive))
    ("\\.tar\\.bz2\\'"
     :pack ("tar" "-cjf" archive sources)
     :unpack ("tar" "-xf" archive))
    ("\\.tar\\.xz\\'"
     :pack ("tar" "-cJf" archive sources)
     :unpack ("tar" "-xf" archive))
    ("\\.txz\\'"
     :pack ("tar" "-cJf" archive sources)
     :unpack ("tar" "-xf" archive))
    )
  "Alist of filename patterns, and command for pack and unpack.

Each element should look like (REGEXP . PLIST).
PLIST should be a plist that may have `:pack' and `:unpack' keys, whose
values will be used as commands to pack and unpack files respectively.
These can be omitted when pack/unpack cannot be done.

Each command should be in format like '(COMMAND ARGS...).
ARGS may contain symbol `archive' and `sources', which will be
replaced when executing these commands.
For backward compatibility, command also can be just a string, in which
case commands to execute will be generated as follows:

    COMMAND ARCHIVES [SOURCES...]


Alist is searched from the beginning.  So, for example, pattern for \".tar.gz\"
should be ahead of pattern for \".gz\""
  :group 'pack
  :type '(alist :key-type string
                :value-type (plist :key-type symbol
                                   :value-type (choice string
                                                       (repeat (choice string
                                                                       (const archive)
                                                                       (const sources)))))))

(defcustom pack-silence nil
  "When set to non-nil, do not pop-up result buffer of pack and unpack processes."
  :type 'boolean
  :group 'pack)

;;;###autoload
(defun pack-dired-dwim (&rest files)
  "Pack or unpack FILES in dired.

If selecting one file and that has a archive suffix defined in
`pack-program-alist', unpack that.
Otherwise, creates archive from marked files, prompting user for archive filename."
  (interactive (dired-get-marked-files t))
  (let ((firstfile (car files)))
    (if (and (eq 1 (length files))
             (pack--get-commands-for firstfile))
        (pack-dired-do-unpack firstfile)
      (apply 'pack-dired-do-pack files))))

;;;###autoload
(defun pack-dired-do-unpack (&rest files)
  "Unpack FILES.

Prompt user to unpack files for sure."
  (interactive (dired-get-marked-files t))
  (dolist (file files)
    (when (yes-or-no-p (format "Unpack %s?: " file))
      (pack-unpack (expand-file-name file))))
  (revert-buffer))

;;;###autoload
(defun pack-dired-do-pack (&rest files)
  "Pack FILES.

Prompt user for archive filename."
  (interactive (dired-get-marked-files t))
  (let* ((dir-default (if (require 'dired-aux nil t)
                          (dired-dwim-target-directory)
                        default-directory))
         (archive-default (concat (file-name-nondirectory (car files))
                                  pack-dired-default-extension))
         (archive ;; (if (interactive-p)
          (read-file-name "Archive file name: "
                          (expand-file-name archive-default dir-default)
                          (expand-file-name archive-default dir-default))
          ))
    (apply 'pack-pack
           (expand-file-name archive)
           files))
  (revert-buffer))

(defun pack--get-commands-for (filename)
  "Return commands to pack and unpack FILENAME archive file.

If the pattern matching FILENAME is found at car of the list in
`pack-program-alist', return cdr of that list.  Otherwise, return nil."
  (let ((case-fold-search nil))
    (assoc-default filename
                   pack-program-alist
                   'string-match-p
                   nil)))

(defun pack--generate-command (command archive &optional sources)
  "Generate command string to execute.

COMMAND should be a list that may contain `archive' and `sources' symbol.

ARCHIVE should be the file path of archive to pack or unpack.
SOURCES, if given, should be a list of file paths to create ARCHIVE
from.
These arguments are put in place of each symbol in COMMAND argument.

For backward compatibility, string arguments are allowed for COMMAND.
In that case, ARCIVES and SOURCES will be quoted and appended to that
command, as follows:

    COMMAND ARCHIVE [SOURCES ...]"
  (cl-assert command)
  (cl-assert archive)
  (if (stringp command)
      (let ((result command))
        (setq result (concat result
                             " "
                             (shell-quote-argument archive)))
        (when sources
          (setq result (concat result
                               " "
                               (mapconcat 'shell-quote-argument
                                          sources
                                          " "))))
        result)
    (let* ((expanded (eval `(list ,@command)
                           `((archive . ,archive)
                             (sources . ,sources))))
           ;; Flatten sources
           (flattened (cl-loop for e in expanded
                               nconc
                               (if (listp e)
                                   (cl-copy-list e)
                                 (list e)))))
      (mapconcat 'shell-quote-argument
                 flattened
                 " "))))

;; (pack--generate-command '("unzip" archive) "a b.zip")
;; (pack--generate-command '("tar" "-czf" archive sources) "a b.zip" '("a.txt" "b c.txt"))
;; (pack--generate-command "tar -xf" "a b.zip")
;; (pack--generate-command "tar -czf" "a b.zip" '("a b.txt" "b.txt"))


(defun pack-unpack (archive)
  "Unpack ARCHIVE.
Command for unpacking is defined in `pack-program-alist'."
  (interactive "fArchive to extract: ")
  (setq archive (expand-file-name archive))
  (let ((cmd (plist-get (pack--get-commands-for archive)
                        :unpack)))
    (if cmd
        (let ((c (current-window-configuration)))
          (async-shell-command (pack--generate-command cmd archive)
                               (get-buffer-create pack-buffer-name))
          (when pack-silence
            (set-window-configuration c)))
      (error "Cannot find unpacking command for %s"
             archive))))

(defun pack-pack (archive &rest files)
  "Make ARCHIVE from FILES.

If ARCHIVE have extension defined in `pack-program-alist', use that command.
Otherwise error will be thrown."
  (cl-assert files
             "FILES to pack are empty")
  (setq archive (expand-file-name archive))
  (let* ((cmd (plist-get (pack--get-commands-for archive)
                         :pack)))
    (if cmd
        (let ((c (current-window-configuration)))
          (async-shell-command (pack--generate-command cmd archive files)
                               (get-buffer-create pack-buffer-name))
          (when pack-silence
            (set-window-configuration c)))
      (error "Cannot find packing command for: %s"
             archive))))

(provide 'pack)

;;; pack.el ends here
