;;; pack.el --- Pack and unpack archive files

;; Author: 10sr <8.slashes@gmail.com>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/pack.el
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: files

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; This library provides some commands and functions to pack and unpack
;; archives.

;; Commands to pack/unpack archive files are defined in `pack-program-alist'.

;; Use from Dired
;; --------------

;; To pack/unpack files from dired buffers, add following to your init.el:

;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map "P" 'pack-dired-dwim))

;; Now you can create an archive file from marked files, or unpack the file when
;; only one file is selected and that seems to be an archive.

;;; Code:

(eval-when-compile
  (require 'simple))

(declare-function dired-dwim-target-directory "dired-aux")
(declare-function dired-get-marked-files "dired")

(defgroup pack nil
  "Pack and unpack archive files."
  :tag "Pack"
  :prefix "pack-"
  :group 'tools)

(defcustom pack-buffer-name "*Pack*"
  "Buffer name for `pack'."
  :type 'string
  :group 'pack)

(defcustom pack-dired-default-extension
  ".7z"
  "Default suffix for pack-dired functions.
Filename with this suffix must matches one of the cars in
`pack-program-alist'."
  :type 'string
  :group 'pack)

(defcustom pack-program-alist
  `(
    ("\\.7z\\'" "7z a" "7z x")
    ("\\.zip\\'" "zip -r" "unzip")
    ("\\.tar\\'" "tar cf" "tar xf")
    ("\\.tgz\\'" "tar czf" "tar xzf")
    ("\\.tar\\.gz\\'" "tar czf" "tar xzf")
    )
  "Alist of filename patterns, and command for pack and unpack.

Each element looks like (REGEXP PACKING-COMMAND UNPACKING-COMMAND).
PACKING-COMMAND and UNPACKING-COMMAND can be nil if the command is not
available.  Alist is searched from the beginning so pattern for \".tar.gz\"
should be ahead of pattern for \".gz\""
  :group 'pack
  :type '(alist :key-type string :value-type (repeat string)))

;;;###autoload
(defun pack-dired-dwim (&rest files)
  "Pack or unpack FILES in dired.

If targetting one file and that has a archive suffix defined in
`pack-program-alist', unpack that.
Otherwise, pack marked files, prompting user to decide archive filename."
  (interactive (dired-get-marked-files t))
  (let ((firstfile (car files)))
    (if (and (eq 1 (length files))
             (pack--get-commands-for firstfile))
        (pack-dired-do-unpack firstfile)
      (pack-dired-do-pack files))))

;;;###autoload
(defun pack-dired-do-unpack (&rest files)
  "Unpack FILES.

Prompt user to unpack files for sure."
  (interactive (dired-get-marked-files t))
  (dolist (file files)
    (when (yes-or-no-p (format "Unpack %s?: " file))
      (pack-unpack file)))
  (revert-buffer))

;;;###autoload
(defun pack-dired-do-pack (&rest files)
  "Pack FILES.

Prompt user to input output archive file name."
  (interactive (dired-get-marked-files t))
  (let* ((dir-default (if (require 'dired-aux nil t)
                          (dired-dwim-target-directory)
                        default-directory))
         (archive-default (concat (file-name-nondirectory (car files))
                                  pack-dired-default-extension))
         (archive ;; (if (interactive-p)
          (read-file-name "Archive file name: "
                          dir-default
                          nil
                          nil
                          archive-default)
          ;; (concat dir-default archive-default)
          ))
    (apply 'pack-pack
           archive
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

;;;###autoload
(defun pack-unpack (archive)
  "Unpack ARCHIVE.

Command for unpacking is defined in `pack-program-alist'."
  (interactive "fArchive to extract: ")
  (let* ((earchive (expand-file-name archive))
         (cmd (nth 1
                   (pack--get-commands-for earchive)))
         )
    (if cmd
        (async-shell-command (concat cmd
                               " "
                               (shell-quote-argument earchive))
                       (get-buffer-create pack-buffer-name))
      (error "Cannot find unpacking command for %s"
               archive))))

(defun pack-pack (archive &rest files)
  "Make ARCHIVE from FILES.

If ARCHIVE have extension defined in `pack-program-alist', use that command.
Otherwise, use `pack-default-extension' for pack."
  (let* ((cmd (car (pack--get-commands-for archive))))
    (if cmd
        (async-shell-command (concat cmd
                                     " "
                                     (shell-quote-argument (expand-file-name
                                                            archive))
                                     " "
                                     (mapconcat (lambda (f)
                                                  (shell-quote-argument
                                                   (expand-file-name
                                                    f)))
                                                files
                                                " "))
                             (get-buffer-create pack-buffer-name))
      (error "Invalid extension for packing: %s"
               archive))))

(provide 'pack)

;;; pack.el ends here
