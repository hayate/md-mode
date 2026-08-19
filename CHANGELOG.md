# Changelog

All notable changes to md-mode. This project follows
[semantic versioning](https://semver.org).

## 0.2.0 - 2026-08-20

Navigation. A design document is half prose and half an index into a
codebase; this release makes that index navigable.

### Added

- **File references.** An inline code span such as `` `lib/thing.py:147` ``
  becomes a link that opens the file at that line. It becomes a link only if
  the file resolves, so an unreachable reference stays plain text and
  linkiness is a live signal that document and code still agree. Resolution
  tries the document's directory, its project root, then `md-link-roots`.
- **Links between documents.** A link to a local `.md` file opens it
  rendered rather than in a browser, with `l` and `r` walking the history.
- **Outline.** `TAB` folds a section, `S-TAB` cycles the document, `i` and
  `t` jump to a heading. Folds survive a re-render.
- **Side by side.** `s` shows source and rendered document in two windows
  that follow each other, kept in step by source line.
- An Info manual, `doc/md-mode.texi`, readable with `C-h i`.
- Continuous integration across Emacs 29.1, 29.4 and 30.1.

### Changed

- Rendering is now a transaction. A render erases the buffer, so point,
  window starts and folds are captured as source lines and heading names
  beforehand and resolved against the new text afterwards; caches that point
  into the old buffer are dropped.
- Position lookup is indexed and binary-searched rather than a walk over the
  whole buffer, since the split view looks one up on every command.

### Fixed

- The inline scanner was quadratic on unmatched delimiters: 8000 unmatched
  brackets took about ten seconds and could freeze Emacs. Delimiter matches
  are now precomputed and failed searches remembered; the same input takes
  18 ms.
- Deeply nested links exhausted the Lisp stack. Inline nesting is bounded as
  block nesting already was.
- A tight list rendered loose when followed by a blank line and a different
  list.
- Ordered list items lost the `.` after their number.

## 0.1.0 - 2026-08-20

First release. Renders a Markdown buffer as a document: proportional body
text, scaled headings, drawn tables, blockquotes with a bar, inline images
and syntax-highlighted code, via `shr` and with no external tools.

### Security

Markdown arrives in cloned repositories, so a document is treated as
untrusted input: no network fetches, a URL scheme allowlist, a tag allowlist
for raw HTML, no major mode ever derived from a fence label, refusal of
remote (TRAMP) paths, and bounds on nesting depth, node count and image size.
