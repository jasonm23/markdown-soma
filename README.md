# Markdown Soma

## Live Markdown in Emacs

`markdown-soma` is an Emacs minor-mode which gives you live rendering of Markdown ⟶ HTML / Emacs ⟶ Browser. Powered by WebSockets and Rust.

Based on the Vim plugin [`vim-markdown-composer`][vmc], using a version of the same back-end service, [Aurelius][aurelius] forked to [`jasonm23/aurelius`][jason-aurelius].

## Markdown support

Soma converts markdown to HTML, using [pulldown-cmark][pulldown-cmark].
It is 100% compliant with the common-markdown spec.

### Extensions

- Github flavored markdown (gfm) tables
- GFM code fences
- GFM task lists
- Strike-through

---

TeX/MathJax support  using [KaTeX][katex]

e.g. `\\sqrt{3x-1}` wrapped in `$` ⟶ `$\\sqrt{3x-1}$` to    display the expression ⟶  $\\sqrt{3x-1}$

wrapping with `\\(\\)` e.g. `\\(\\sqrt{3x-1}\\)` gives the same \\(\\sqrt{3x-1}\\) inline.

Use `$$...$$` or `\\[..\\]`to center the expression in a presentation style.

e.g. `$$\\sqrt{3x-1}$$`

$$\\sqrt{3x-1}$$

`$$n = {A \pm \sqrt{b^4-4ac} \over 2a}$$`

$$n = {A \pm \sqrt{b^4-4ac} \over 2a}$$

`$$\xleftrightharpoons{abc}$$`

$$\xleftrightharpoons{abc}$$

---

<!--
TODO:

[Mermaid.js][mermaid] diagram support on the browser side. Allowing you to create directed graphs, state charts, sequence diagrams etc. 

```
graph TD;
    A-->B;
    A-->C;
    B-->D;
    C-->D;
```

```mermaid
graph TD;
    A-->B;
    A-->C;
    B-->D;
    C-->D;
```
-->

## Install

Via [MELPA](https://melpa.org)

```
M-x package-install markdown-soma
```

Using [Doom Emacs](https://github.com/doomemacs/doomemacs)

in `~/.doom.d/packages.el`

```lisp
(package! markdown-soma)
```

### Install `soma` executable

`soma` source code is provided with the package.

Use [`rustup`][rustup].

Once rust is ready to use, open a terminal at the package folder.

```shell
$ cargo install --path .

# compiles:
# ⟶ ~/.cargo/bin/soma
```

## Usage

To start and stop `markdown-soma` from a buffer:

```
M-x markdown-soma
```

This will launch your default browser, opening a tab with the rendered markdown view.

Edits and commands in your current Emacs buffer, will trigger a new markdown render in the browser. The browser view will automatically scroll so you can see what you're editing.

## Licence

GPL3

## Technical note.

[Markdown-Soma](https://github.com/jasonm23/soma) does process communication via `stdin`.  Emacs sends text from the current buffer to `soma` using `(process-send-string BUFFER-TEXT PROCESS)`.

`soma` expects input to be markdown text. This will broadcast to connected clients (as HTML), via WebSocket.

You can embed a value for `scrollTo`, into the input with a magic comment. Emacs will do this for you.

For example:

```json
<!-- SOMA: {"scrollTo": 0} // scrolls to the top.  -->
```

[`Aurelius`](https://github.com/euclio/aurelius) is providing the core markdown service, `soma` is  wrapping `aurelius` as a `stdin` buffering process.

[rustup]: https://rustup.rs
[pulldown-cmark]: https://github.com/raphlinus/pulldown-cmark
[mermaid]: https://mermaid-js.github.io
[katex]: https://katex.org
[aurelius]: https://github.com/euclio/aurelius
[jason-aurelius]: https://github.com/jasonm23/aurelius
[vmc]: https://github.com/euclio/vim-markdown-composer
