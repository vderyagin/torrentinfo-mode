(require 'ansi-color)

(define-derived-mode torrentinfo-mode fundamental-mode "TorrentInfo"
  "Major mode for viewing information about torrent files."
  (read-only-mode t)
  (hl-line-mode))

(define-key torrentinfo-mode-map (kbd "p") 'previous-line)
(define-key torrentinfo-mode-map (kbd "n") 'next-line)
(define-key torrentinfo-mode-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))

(defun torrentinfo-file-handler (operation file &optional visit beg end replace)
  (setq buffer-file-name (expand-file-name file))
  (let ((info (shell-command-to-string
               (format "torrentinfo --files %s" (shell-quote-argument file)))))
    (insert info)
    (goto-char (point-min))
    (ansi-color-apply-on-region (point-min) (point-max))))

(put 'torrentinfo-file-handler 'safe-magic t)
(put 'torrentinfo-file-handler 'operations '(insert-file-contents))
(add-to-list 'file-name-handler-alist '("\\.torrent\\'" . torrentinfo-file-handler))

(add-to-list 'auto-mode-alist '("\\.torrent\\'" . torrentinfo-mode))

(provide 'torrentinfo-mode)
;;; torrentinfo-mode.el ends here
