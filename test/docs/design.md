# Design: the navigation features

Ticket: [86eyk9fd2](https://example.com/t/86eyk9fd2). See also
[the QA notes](./notes.md) and [the missing one](./nope.md).

## Where the code lives

The parser is `md-parse.el:1` and the renderer `md-render.el:100`.
The toggle is in `md-mode.el` with no line at all.

`md-render-dom` runs at `md-render.el:446` and `:452` in the same function.

## References that cannot resolve

`common/views/webhook.py:507` lives in another repo entirely, and
`ops/libs/esim_email.py:147` likewise. These must stay plain text.

Not references at all: `12:30`, `a.b.c`, `https://x.example:8080/p`,
`{"key": 42}`, `C:\Users\x\file.txt`.

## A table

| file | purpose |
|------|---------|
| `md-parse.el` | markdown to DOM |
| `md-render.el` | DOM to buffer |
