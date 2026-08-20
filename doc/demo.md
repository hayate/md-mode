# Rendering a design document

A paragraph of **bold**, *italic*, `inline code` and ~~struck~~ text, with a
[link to the manual](./md-mode.texi) and a footnote-free [reference][spec].

## What the renderer covers

- Lists, with real bullets
  - and nesting
- [x] task items, checked
- [ ] and unchecked

> A blockquote gets a bar down its left edge.
>
> > Nested ones stack their bars.

## Where the code lives

The parser is `md-parse.el:1`; rendering runs through `md-render.el:100`.
References resolve against the project, so those two are links and
`common/views/webhook.py:507` - which lives in another repository - is not.

```elisp
(defun md-render-width ()
  "Width, in characters, to render at in the selected window."
  (max 20 (min md-render-max-width (- (window-body-width) 2))))
```

| Feature | Where | Notes |
|:--------|:-----:|------:|
| parsing | `md-parse.el` | markdown to DOM |
| rendering | `md-render.el` | DOM to buffer |
| navigation | `md-link.el` | references and links |

---

[spec]: https://spec.commonmark.org/
