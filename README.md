# md-mode

Read Markdown files in Emacs as rendered documents rather than as marked-up
source.

`M-x md-mode` in a Markdown buffer replaces the window with the rendered
document: proportional body text, scaled headings, drawn tables, quoted
passages with a bar down their left edge, syntax-highlighted code and inline
images. `M-x md-mode` again, or `q`, returns to the source, and point comes
back with you.

This is not syntax highlighting. `markdown-mode` makes the source prettier and
leaves every `#`, `**` and `[label](url)` on screen, which is the right choice
for editing. `md-mode` is for reading: the markup goes away and a document
appears.

## Requirements

Emacs 29.1 or later. Nothing else - no pandoc, no Node, no browser. Rendering
goes through `shr`, the HTML engine that ships with Emacs and drives `eww`.

## Install

    git clone https://github.com/hayate/md-mode.git ~/.emacs.d/md-mode

Then, in your init file:

```elisp
(add-to-list 'load-path "~/.emacs.d/md-mode")
(autoload 'md-mode "md-mode" "Render the Markdown in this buffer." t)
```

With `use-package` and a recent Emacs:

```elisp
(use-package md-mode
  :vc (:url "https://github.com/hayate/md-mode" :rev :newest)
  :commands (md-mode))
```

There is nothing to compile and no build step. `md-mode` is an autoloaded
command, so nothing is loaded until you first run it.

## Keys in a rendered document

| Key | Does |
|---|---|
| `q` | back to the Markdown source |
| `g` | re-render |
| `s` | source and document side by side, in step |
| `RET` | follow the link at point |
| `TAB` | fold or unfold the section at point; elsewhere, next link |
| `S-TAB` | cycle the folding of the whole document |
| `n` / `p` | next / previous link |
| `i` | jump to a heading with imenu |
| `t` | jump to a heading from a table of contents |
| `l` / `r` | back / forward through documents you have followed |
| `^` | open the directory the document lives in |

The view re-renders when you save the source, when you revert it, and when the
window width changes. Folds survive a re-render.

## Navigating

A design document is usually half prose and half an index into a codebase, so
md-mode makes that index navigable.

**File references.** An inline code span holding something like
`` `lib/thing.py:147` `` becomes a link that opens the file at line 147. It
becomes a link *only if the file is actually there*, resolved against the
document's directory, then its project root, then `md-link-roots`. An
unreachable reference stays ordinary inline code - so linkiness is a live
signal that the document and the code still agree. Move the document, or the
file, and the link quietly stops being one.

Set `md-link-roots` when a document points into a different checkout than the
one it lives in, which is the normal case for a folder of specs:

```elisp
;; .dir-locals.el in your specs directory
((nil . ((md-link-roots . ("~/src/api" "~/src/models")))))
```

**Other documents.** A link to a local `.md` file opens it rendered rather than
in a browser, and `l` / `r` walk back and forward, so a folder of
cross-referencing specs reads like a small wiki. External URLs still go to the
browser.

**Long documents.** Headings are real outline headings: fold with `TAB`, cycle
the whole document with `S-TAB`, jump with `i` or `t`.

**Side by side.** `s` puts the source and the rendered document in two windows
that follow each other. They are kept in step by *source line*, not by buffer
position - rendering is lossy, since a whole paragraph maps to its first line,
so round-tripping raw positions would make point crawl.

## What it renders

Headings, both ATX and setext. Bold, italic, `***both***`, inline code,
strikethrough. Links, reference links, autolinks and bare URLs. Images, resolved
relative to the document. Ordered, unordered and nested lists, tight and loose,
including task lists with checkboxes. Blockquotes, nested, with lists and tables
inside them. Fenced and indented code, syntax-highlighted when the language is
known. GFM pipe tables with alignment. Thematic breaks, hard line breaks,
backslash escapes, HTML entities. YAML front matter is hidden, as GitHub does.
Raw HTML blocks are parsed and spliced in.

Deliberately out of scope: footnotes, definition lists, math and full
CommonMark conformance. The target is the Markdown people actually write.

## Safety

Markdown files arrive in cloned repositories, so a document is treated as
untrusted input:

- Remote images are **not** fetched. They render as a label. Set
  `md-render-remote-images` to `t` if you want them.
- URLs go through a scheme allowlist - `http`, `https`, `mailto`, `ftp` and a
  few others. Anything else, `javascript:` and `data:` included, is dropped and
  the label is left inert. Control characters are stripped before the scheme is
  read, so an entity-encoded newline cannot smuggle one past. This applies to
  Markdown links and images, not only to raw HTML.
- Remote (TRAMP) paths such as `/ssh:host:/x.png` are refused outright. Merely
  asking whether such a file is readable would open a network connection.
- Raw HTML goes through a tag allowlist. `script`, `style`, `base`, `object`,
  `iframe`, `svg`, form and media tags are dropped before anything reaches the
  renderer.
- Code fences select a highlighting mode from `md-render-language-modes`, an
  allowlist. A mode name is never derived from a fence label, so a document
  cannot activate an arbitrary major mode.
- Nesting depth (block and inline), HTML node count and image size are all
  bounded, and the inline scanner is linear rather than quadratic, so a
  document full of unmatched brackets cannot freeze Emacs.

Local image paths are *not* confined to the document's directory. A document
can point at `../assets/logo.png`, which real repositories do constantly, and
at any other readable file. Nothing leaves the machine, so the containment
would cost more than it buys.

## Customization

| Variable | Default | Allowed values | Meaning |
|---|---|---|---|
| `md-render-max-width` | `100` | integer columns; under 20 is treated as 20 | cap on line width, so text stays readable in a wide window |
| `md-render-remote-images` | `nil` | `t` or `nil` | download and display images with remote URLs |
| `md-render-highlight-code` | `t` | `t` or `nil` | syntax-highlight fenced code |
| `md-render-language-modes` | 32 entries | alist of `("label" . mode-symbol)` | fence label to the major mode that highlights it |
| `md-render-max-highlight-size` | `20000` | integer characters | fences larger than this are left unhighlighted |
| `md-render-max-image-size` | `8388608` | integer bytes | image files larger than this are not displayed |
| `md-parse-front-matter` | `nil` | `t` or `nil` | render YAML front matter instead of hiding it |
| `md-parse-html-blocks` | `t` | `t` or `nil` | parse raw HTML blocks and splice them in |
| `md-auto-rerender-max-size` | `200000` | integer characters | above this, re-render only on demand with `g` |
| `md-rerender-delay` | `0.3` | number of seconds | idle time after a resize before re-rendering |
| `md-view-margin` | `2` | integer columns; `0` disables | blank left margin in the rendered view |
| `md-view-hide-line-numbers` | `t` | `t` or `nil` | hide the gutter, whose numbers count rendered lines |
| `md-link-roots` | `nil` | list of directories | extra roots that file references may resolve inside |
| `md-link-max-references` | `500` | integer | most references resolved in one document |
| `md-link-follow-markdown` | `t` | `t` or `nil` | a link to a local `.md` opens rendered |
| `md-outline-fold-on-render` | `t` | `t` or `nil` | folded sections stay folded across a re-render |

Every one of these is a `defcustom`, so `M-x customize-group RET md RET` shows
them with their documentation and enforces the types above.

Faces: `md-code-block`, `md-blockquote`, `md-blockquote-bar`, plus the standard
`shr-h1` ... `shr-h6`, `shr-link` and `shr-text`.

## How it works

    markdown text --md-parse--> DOM --shr--> rendered buffer

    md-parse.el    markdown -> DOM
    md-render.el   DOM -> buffer, via shr
    md-link.el     file references and links out of the document
    md-outline.el  folding, imenu, table of contents
    md-mode.el     the command, the view mode, split view, history

`md-parse.el` turns Markdown into the DOM representation `dom.el` uses and
`shr` consumes: `(TAG ATTRIBUTE-ALIST CHILD...)`, strings as text nodes. No HTML
text is generated anywhere, so there is nothing to escape and nothing to
reparse. `md-render.el` hands that tree to `shr` and overrides three tags whose
defaults are wrong for a Markdown reader: `pre` gains syntax highlighting,
`blockquote` gains its bar, `img` resolves relative paths and refuses the
network. Every other block tag is wrapped only to record its source line, which
is how point survives the toggle.

The rendered document lives in its own read-only buffer, so the file on disk is
never touched by rendering and cannot be saved in rendered form.

The view also drops editor furniture that means nothing in a document: it takes
a small left margin, and it hides line numbers, whose values would count
rendered lines rather than source ones. Both are applied on each render rather
than from the major mode, because a global minor mode such as
`global-display-line-numbers-mode` turns itself on from
`after-change-major-mode-hook`, which runs after the mode body.

## Tests

    ./run-tests.sh

Byte-compiles the five files and runs 84 ERT tests: the parser against DOM
shapes, the renderer against the buffer it produces, and navigation against
real fixture documents - including regressions for every defect found in
review.

## Known limits

- A rendered buffer has one layout, so it cannot serve two windows of different
  widths at once. Showing a view in a second window leaves it alone; press `g`
  in the window you want it sized for.
- Position mapping is per block, with two exceptions: code blocks and table
  rows map line by line. Inside a paragraph it can only be per block, because
  filling means a source line no longer corresponds to any one rendered line.
  Toggling from the middle of a long paragraph returns you to its first line.
- A re-render erases and rebuilds the buffer, so anything anchored to a
  position has to be re-derived. Point, window starts and folds are carried
  across by source line and heading name, and imenu's index is rebuilt.
  Anything added later should do the same rather than hold a position.
- Rendering is linear in document size but not free. Measured on this machine:
  32 KB takes about 110 ms end to end, 128 KB about 580 ms. The parsed tree is
  cached against the buffer's modification tick, so a resize re-renders without
  re-parsing. Above `md-auto-rerender-max-size` (200 KB) the automatic
  re-render is skipped and `g` remains.

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).

Emacs packages are GPL by convention, and here it is also a requirement:
`md--render-li` in `md-render.el` is derived from `shr-tag-li` in GNU Emacs'
`shr.el`, copyright (C) Free Software Foundation, Inc. That code is
GPL-3.0-or-later, so this package is too.

## Contributing

Bug reports and patches are welcome. If you send a patch, please run
`./run-tests.sh` first and add a test for whatever you fixed - the suite exists
so that the awkward cases (nested blockquotes, loose lists, unmatched
delimiters, hostile HTML) stay fixed.
