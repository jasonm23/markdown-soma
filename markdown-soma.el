;;; markdown-soma --- live preview for Markdown, yes it's the best one.
;;
;;; Commentary:
;;  A section marked commentatry.
;;
;;; Code:

(defun soma-render (text)
  (interactive)
  (process-send-string "soma" (format "%s\n" text))
  (process-send-eof))

(defun soma-render-buffer (&rest _)
  (interactive)
  (soma-render
   (format "<!-- SOMA: {\"scrollTo\": %.5f} -->\n%s"
           (soma-current-scroll-percent)
           (buffer-string))))

(defun soma-hooks-add ()
  (add-hook 'post-command-hook #'soma-render-buffer nil t)
  (add-hook 'after-change-functions #'soma-render-buffer nil t)
  (add-hook 'after-save-hook #'soma-render-buffer nil t)
  (add-hook 'after-revert-hook #'soma-render-buffer nil t))

(defun soma-hooks-remove ()
  (remove-hook 'post-command-hook #'soma-render-buffer t)
  (remove-hook 'after-change-functions #'soma-render-buffer t)
  (remove-hook 'after-save-hook #'soma-render-buffer t)
  (remove-hook 'after-revert-hook #'soma-render-buffer t))

(defun soma-start ()
       (interactive)
       (start-process "soma" "*soma*" "soma")
       (soma-render "# Waiting...")
       (soma-render-buffer)
       (soma-hooks-add))

(defun soma-stop ()
  (interactive)
  (stop-process "soma")
  (kill-buffer "*soma*")
  (soma-hooks-remove))

(defun soma-current-scroll-percent ()
(/ (line-number-at-pos (point))
   (count-lines 1 (buffer-size)) 1.0))

(provide 'markdown-soma)
;;; markdown-soma.el ends here
