# Markdown Soma

## Live Markdown in Emacs

Markdown Soma gives you live rendering of Markdown text -> HTML from Emacs. Powered by websockets and Rust.

Based on the Vim plugin `vim-markdown-composer` and using a version of the same back-end service, [Aurelius](https://github.com/euclio/aurelius)

[Markdown-Soma](https://github.com/jasonm23/soma) uses the absolute simplest inter-process communication, `stdio`.  Emacs will push the buffer string to `soma` through `stdin` using `(process-send-string buffer-text)`.

## Install

TODO: add to MELPA

## Usage

From a `markdown-mode` buffer do:

```
M-x markdown-soma
```

**(*cough* not yet, see below this is pre-alpha WIP software.)**

`soma` will launch your default browser and open a tab with the rendered markdown view.

When running edits and commands will trigger a render on the browser, and scroll to make sure you can see what you're editing.

## Licence

GPL3

### Code Scratch...

This is a work in progress and is using a technique, I suppose you could call, `*scratch*` driven development.

This `README.md` began as a test file for `soma`.

To use `soma` right now, you'd need to compile the rust source:

```
cargo build --release

# compiles:
# -> target/release/soma 
```

Make sure `soma` is in your path.

Next `eval C-x C-e` the defuns below.


```lisp
(defun soma-render (text)
  (interactive)
  (process-send-string "soma" (format "%s\n" text))
  (process-send-eof "

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
  (remove-hook 'post-command-hook #'soma-render-buffer nil t)
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
```

 

```lisp
(soma-start)
(soma-hooks-add)
(soma-hooks-remove)
(soma-stop)
```
