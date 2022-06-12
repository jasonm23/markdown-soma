# Markdown Soma

## Live Markdown in Emacs

`markdown-soma` is an Emacs minor-mode which gives you live rendering of Markdown -> HTML / Emacs -> Browser. Powered by WebSockets and Rust.

Based on the Vim plugin `vim-markdown-composer`, using a version of the same back-end service, [Aurelius](https://github.com/euclio/aurelius) forked to `jasonm23/aurelius`.

## Install

Via [MELPA](https://melpa.org)

```
M-x package-install markdown-soma
```

Using [Doom Emacs](https://github.com/doomemacs/doomemacs)

```
;; in ~/.doom.d/packages.el

(package! markdown-soma)
```

### Install `soma` binary

From the package folder.

```
cargo install --path . 

# compiles:
# ⟶ ~/.cargo/bin/soma 
```

I recommend [`rustup`][rustup] to get your rust environment installed.]

## Usage

To start and stop `markdown-soma` do: 

```
M-x markdown-soma
```

This will launch your default browser, opening a tab with the rendered markdown view.

Edits and commands in your current Emacs buffer, will trigger a new markdown render in the browser. The browser view will automatically scroll so you can see what you're editing.

## Licence

GPL3

- - -

NOTE: This is a work in progress. I used a technique which I suppose you could call, `*scratch*` driven development.

This `README.md` began as a `*scratch*` file.  All the Emacs side code for this minor-mode began here as evaluated `sexp`.

## Technical note. 

[Markdown-Soma](https://github.com/jasonm23/soma) does process communication via `stdin`.  Emacs sends text from the current buffer to `soma` using `(process-send-string BUFFER-TEXT PROCESS)`.

`soma` expects input to be markdown text. This will broadcast to connected clients (as HTML), via WebSocket.

You can embed a value for `scrollTo`, into the input with a magic comment. Emacs will do this for you. 

For example:

```json
<!-- SOMA: {"scrollTo": 0} // scrolls to the top.  -->
```

[`Aurelius`](https://github.com/euclio/aurelius) is providing the core markdown service, soma is just wrapping `aurelius` as a `stdin` buffering process. 

### How does it work?

Starting `soma` on the command line will help to illustrate how it works.

```
$ soma
=> listening on [::1]:58943
```

A browser tab will open at `[::1]:58943` _(or some other randomly assigned available port.)_ 

Type or paste any text, at the start of a new-line press `^D` (i.e. <kbd>Ctrl</kbd>+<kbd>D</kbd>)

The browser will now render the text passed to `stdin`.

Try it again, type something ⟶  press enter ⟶  press `^D` and the browser will receive the  update and render.

<kbd>Ctrl+C</kbd> will quit `soma`.

[rustup]: https://rustup.rs
