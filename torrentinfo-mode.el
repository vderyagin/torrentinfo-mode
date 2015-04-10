;;; torrentinfo-mode.el --- View information about torrent files -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 23 Oct 2014
;; Version: 0.1
;; Package-Requires: ()

;;; Commentary:

;;; Code:

(require 'ansi-color)
(require 'ido)

(defgroup torrentinfo-mode nil
  "Mode for viewing *.torrent files."
  :prefix "torrentinfo-"
  :group 'external)

(defcustom torrentinfo-detail-level 'files
  "Level of detail displayed about torrent file."
  :group 'torrentinfo-mode
  :type '(radio
          (const :tag "information about torrent + list of files" files)
          (const :tag "only general information" minimal)
          (const :tag "all information there is" everything)))

(make-variable-buffer-local 'torrentinfo-detail-level)

(defun torrentinfo--cmd (file)
  "Command line to get information about FILE."
  (format "torrentinfo %s %s"
          (pcase torrentinfo-detail-level
            (`files "--files")
            (`everything "--everything")
            (`minimal "")
            (_ (error "invalid detail level: %s" torrentinfo-detail-level)))
          (shell-quote-argument file)))

(defun torrentinfo--get-info (file)
  (ansi-color-apply (shell-command-to-string (torrentinfo--cmd file))))

(defun torrentinfo-change-detail-level (&optional level)
  (interactive)
  (let ((new-level (or level (torrentinfo--select-detail-level))))
    (unless (eq new-level torrentinfo-detail-level)
      (setq torrentinfo-detail-level new-level)
      (torrentinfo--insert-info))))

(defun torrentinfo--select-detail-level ()
  (intern
   (ido-completing-read "Detail level: "
                        (mapcar #'symbol-name
                                '(files everything minimal)))))

(defun torrentinfo--insert-info ()
  (read-only-mode -1)
  (erase-buffer)
  (insert (torrentinfo--get-info buffer-file-name))
  (goto-char (point-min))
  (read-only-mode 1)
  (set-buffer-modified-p nil))

;;;###autoload
(defun torrentinfo-file-handler (_ file &rest __)
  (unless (executable-find "torrentinfo")
    (error "torrentinfo(1) appears to not be installed"))
  (setq buffer-file-name (expand-file-name file))
  (insert (torrentinfo--get-info file))
  (goto-char (point-min)))

;;;###autoload
(define-derived-mode torrentinfo-mode fundamental-mode "TorrentInfo"
  "Major mode for viewing information about torrent files."
  (read-only-mode 1)
  (hl-line-mode))

(define-key torrentinfo-mode-map (kbd "p") 'previous-line)
(define-key torrentinfo-mode-map (kbd "n") 'next-line)
(define-key torrentinfo-mode-map (kbd "SPC") 'scroll-up-command)
(define-key torrentinfo-mode-map (kbd "S-SPC") 'scroll-down-command)
(define-key torrentinfo-mode-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
(define-key torrentinfo-mode-map (kbd "<tab>") 'torrentinfo-change-detail-level)

;;;###autoload
(progn
  (put 'torrentinfo-file-handler 'safe-magic t)
  (put 'torrentinfo-file-handler 'operations '(insert-file-contents))
  (add-to-list 'file-name-handler-alist '("\\.torrent\\'" . torrentinfo-file-handler))
  (add-to-list 'auto-mode-alist '("\\.torrent\\'" . torrentinfo-mode)))

(provide 'torrentinfo-mode)

;;; torrentinfo-mode.el ends here
