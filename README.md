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

### `soma` binary

You'll need to compile the rust source:

```
cargo build --release

# compiles:
# -> target/release/soma 
```

`soma` must be in your exec path. 

[I recommend `rustup` to get your rust environment installed.](https://rustup.rs/)

## Usage

To start and stop `markdown-soma` do: 

```
M-x markdown-soma
```

This will launch your default browser, opening a tab with the rendered markdown view.

Edits and commands in Emacs, will trigger a render on the browser. The browser view will automatically scroll so you can see what you're editing.

## Licence

GPL3

- - -

NOTE: This is a work in progress. I used a technique which I suppose you could call, `*scratch*` driven development.

This `README.md` began as a `*scratch*` file.  All the Emacs side code for this minor-mode began here as evaluated `sexp`.

## Technical note. 

[Markdown-Soma](https://github.com/jasonm23/soma) does process communication via `stdin`.  Emacs sends text from the current buffer to `soma` using `(process-send-string BUFFER-TEXT PROCESS)`.

`soma` expects all input to be markdown text and then processes it to HTML, which is sent via websocket to browser client(s).

We can pass anything we want the client side to process via HTML comments in the markdown text.  This way we can avoid using additional transport mechanisms or protocols, and instead keep things as simple as possible.

On the client end, `aurelius` is providing the core markdown service, soma is just wrapping `aurelius` as a `stdin` buffering process.

Starting `soma` on the command line will help to illustrate how it works.

```
soma
listening on [::1]:xxxxx <- random port is generated.
```

A browser tab will open at `[::1]:xxxxx` 

Type or paste any text, and press enter. Immediately after a new-line press `^D` <kbd>Ctrl</kbd>+<kbd>D</kbd>

You'll see the text render in the browser.

You can repeat: type something `->` press enter `->` press `^D`, _et voilà!_ The browser view will be replaced by the new text.

# Todo

- [ ] User should be able to customise the following:
  - [ ] host address, port
  - [ ] Markdown css
  - [ ] `highlight.js` theme
  - [ ] browser  

# Bugs
