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

in Emacs Eval: `(load-file "markdown-soma.el")`

`M-x soma-start` 

To quit, `M-x soma-stop`

Things may be broken, open an issue maybe?
