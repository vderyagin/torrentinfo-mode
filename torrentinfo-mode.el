(require 'ansi-color)

(defvar torrentinfo-detail-level 'files
  "Level of detail displayed about torrent file.")

(define-derived-mode torrentinfo-mode fundamental-mode "TorrentInfo"
  "Major mode for viewing information about torrent files."
  (read-only-mode t)
  (hl-line-mode))

(defun torrentinfo--cmd (file)
  "Command line to get information about FILE."
  (let ((flag (cond
               ((eq torrentinfo-detail-level 'files) "--files")
               ((eq torrentinfo-detail-level 'everything) "--everything")
               ((eq torrentinfo-detail-level 'minimal) "")
               (t (error "invalid detail level: %s" (symbol-name torrentinfo-detail-level)))))
        (file (shell-quote-argument file)))
      (format "torrentinfo %s %s" flag file)))

(defun torrentinfo-file-handler (operation file &optional visit beg end replace)
  (setq buffer-file-name (expand-file-name file))
  (let ((info (shell-command-to-string (torrentinfo--cmd file))))
    (insert info)
    (goto-char (point-min))
    (ansi-color-apply-on-region (point-min) (point-max))))

(put 'torrentinfo-file-handler 'safe-magic t)
(put 'torrentinfo-file-handler 'operations '(insert-file-contents))
(add-to-list 'file-name-handler-alist '("\\.torrent\\'" . torrentinfo-file-handler))

(define-key torrentinfo-mode-map (kbd "p") 'previous-line)
(define-key torrentinfo-mode-map (kbd "n") 'next-line)
(define-key torrentinfo-mode-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))

(add-to-list 'auto-mode-alist '("\\.torrent\\'" . torrentinfo-mode))

(provide 'torrentinfo-mode)
;;; torrentinfo-mode.el ends here
