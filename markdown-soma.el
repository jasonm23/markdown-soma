;;; markdown-soma.el --- live preview for Markdown.

;; Copyright (C) 2022 Jason Milkins

;: Author: Jason Milkins <jasonm23@gmail.com>
;: URL: https://github.com/jasonm23/markdown-soma
;; Keywords: text markdown
;: Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:
;;  Markdown-soma live markdown preview for Emacs. Based on
;;  vim-markdown-composer, using the same back-end rust based markdown service
;;
;;; Code:

(defgroup markdown-soma nil
  "Realtime Markdown Preview"
  :group 'markdown
  :prefix "markdown-soma-")

(define-minor-mode markdown-soma-mode
  "Live Markdown Preview"
  :group      'markdown-soma
  :init-value nil
  :global     nil
  (if markdown-soma-mode
      (markdown-soma-start)
    (markdown-soma-stop)))

(defun markdown-soma-render (text)
  (process-send-string "*markdown-soma*" (format "%s\n" text))
  (process-send-eof "*markdown-soma*"))

(defun markdown-soma-render-buffer (&rest _)
  (markdown-soma-render
   (format "<!-- SOMA: {\"scrollTo\": %.5f} -->\n%s"
           (markdown-soma-current-scroll-percent)
           (buffer-string))))

(defun markdown-soma-hooks-add ()
  (add-hook 'kill-buffer-hook #'markdown-soma-stop nil t)
  (add-hook 'post-command-hook #'markdown-soma-render-buffer nil t)
  (add-hook 'after-change-functions #'markdown-soma-render-buffer nil t)
  (add-hook 'after-save-hook #'markdown-soma-render-buffer nil t)
  (add-hook 'after-revert-hook #'markdown-soma-render-buffer nil t))

(defun markdown-soma-hooks-remove ()
  (remove-hook 'kill-buffer-hook #'markdown-soma-stop t)
  (remove-hook 'post-command-hook #'markdown-soma-render-buffer t)
  (remove-hook 'after-change-functions #'markdown-soma-render-buffer t)
  (remove-hook 'after-save-hook #'markdown-soma-render-buffer t)
  (remove-hook 'after-revert-hook #'markdown-soma-render-buffer t))

(defun markdown-soma-start ()

  ;; check soma executable exists...
  ;; if not, provide a message + how to build

  (if (executable-find "soma")
      (progn
        (message "markdown-soma-start")
        (start-process "markdown-soma" "*markdown-soma*" "soma")
        (set-process-query-on-exit-flag (get-process "markdown-soma") nil)
        (markdown-soma-render "# Waiting...")
        (markdown-soma-render-buffer)
        (markdown-soma-hooks-add))
    ;; else
    (message "Markdown soma execuatble `soma` not found.\
\
use:\
\
cargo build --release\
\
to compile it to: ./target/release/soma\
\
it must be findable via exec-path.")))

(defun markdown-soma-stop ()
  (message "markdown-soma-stop")
  (stop-process "markdown-soma")
  (kill-buffer "*markdown-soma*")
  (markdown-soma-hooks-remove))

(defun markdown-soma-current-scroll-percent ()
  "Calculate the position of point as decimal percentage of the buffer size."
  (if (= 0 (buffer-size)) 0.0
    ;; else
    (/ (line-number-at-pos (point))
       (count-lines 1 (buffer-size)) 1.0)))

(provide 'markdown-soma)
;;; markdown-soma.el ends here
