;;; torrentinfo-mode.el --- View information about torrent files -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 23 Oct 2014
;; Version: 0.2.1
;; Package-Requires: ()

;;; Commentary:

;;; Code:

(require 'ansi-color)
(require 'seq)

(defgroup torrentinfo-mode nil
  "Mode for viewing *.torrent files."
  :prefix "torrentinfo-"
  :group 'external)

(defcustom torrentinfo-detail-level 'minimal
  "Level of detail displayed about torrent file."
  :group 'torrentinfo-mode
  :type '(radio
          (const :tag "information about torrent + list of files" files)
          (const :tag "only general information" minimal)
          (const :tag "all information there is" everything)))

(make-variable-buffer-local 'torrentinfo-detail-level)

(defmacro torrentinfo-with-detail-level (&rest clauses)
  (declare (indent 0))
  `(pcase torrentinfo-detail-level
     ,@clauses
     (_ (error "invalid detail level: %s" torrentinfo-detail-level))))

(defun torrentinfo--cmd (file)
  "Command line to get information about FILE."
  (format "torrentinfo %s %s"
          (torrentinfo-with-detail-level
            (`files "--files")
            (`everything "--everything")
            (`minimal ""))
          (shell-quote-argument file)))

(defun torrentinfo--next-detail-level ()
  (torrentinfo-with-detail-level
    (`files 'everything)
    (`everything 'minimal)
    (`minimal 'files)))

(defun torrentinfo-next-detail-level ()
  (interactive)
  (torrentinfo-change-detail-level (torrentinfo--next-detail-level)))

(defun torrentinfo--get-info (file)
  (ansi-color-apply (shell-command-to-string (torrentinfo--cmd file))))

(defun torrentinfo-change-detail-level (level)
  (unless (eq level torrentinfo-detail-level)
    (setq torrentinfo-detail-level level)
    (torrentinfo--insert-info)))

(defun torrentinfo--insert-info ()
  (read-only-mode -1)
  (erase-buffer)
  (insert (torrentinfo--get-info buffer-file-name))
  (goto-char (point-min))
  (read-only-mode 1)
  (set-buffer-modified-p nil)
  (setq mode-line-process (format " %s" torrentinfo-detail-level)))

;;;###autoload
(defun torrentinfo-file-handler (_ file &rest __)
  (unless (executable-find "torrentinfo")
    (error "torrentinfo(1) appears to not be installed"))
  (setq buffer-file-name (expand-file-name file)))

;;;###autoload
(define-derived-mode torrentinfo-mode fundamental-mode "TorrentInfo"
  "Major mode for viewing information about torrent files."
  (torrentinfo--insert-info)
  (hl-line-mode)
  (message "Press <tab> to view file with different detail level"))

(seq-each (pcase-lambda (`(,key . ,cmd))
            (define-key torrentinfo-mode-map (kbd key) cmd))
          '(("p"     . previous-line)
            ("n"     . next-line)
            ("SPC"   . scroll-up-command)
            ("S-SPC" . scroll-down-command)
            ("q"     . (lambda () (interactive) (kill-buffer (current-buffer))))
            ("<tab>" . torrentinfo-next-detail-level)))

;;;###autoload
(progn
  (put 'torrentinfo-file-handler 'safe-magic t)
  (put 'torrentinfo-file-handler 'operations '(insert-file-contents))
  (add-to-list 'file-name-handler-alist '("\\.torrent\\'" . torrentinfo-file-handler))
  (add-to-list 'auto-mode-alist '("\\.torrent\\'" . torrentinfo-mode)))

(provide 'torrentinfo-mode)

;;; torrentinfo-mode.el ends here
