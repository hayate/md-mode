---
title: Corpus document
tags: [render, test]
---

# Heading one

Intro paragraph with **bold**, *italic*, `inline code`, ~~struck~~ text and a
[link to somewhere](https://example.com "with a title"). A snake_case_identifier
must not turn into emphasis, and neither must 2*3*4 arithmetic.

Setext heading
==============

## Heading two

- first bullet
- second bullet with `code`
  - nested bullet
  - another nested
- third bullet

1. ordered one
2. ordered two
   1. nested ordered
3. ordered three

- [ ] unchecked task
- [x] checked task

Loose list:

- item with its own paragraph

- second loose item

> A blockquote with **bold** inside.
>
> - and a list inside the quote
> - second item
>
> > nested quote

```elisp
(defun md--example (x)
  "Docstring here."
  (+ x 1))
```

```
fence with no language
```

~~~python
def f(x):
    return x + 1
~~~

    indented code block
    second line

| Left | Center | Right |
|:-----|:------:|------:|
| a    | b      | c     |
| longer cell | x | 42 |

---

Term list and HTML block:

<div class="note">
  <p>Raw <b>HTML</b> block passed through.</p>
</div>

Hard break at the end of this line  
and the continuation.

Autolink: <https://example.org> and bare https://example.net too.

![Local image](img/diagram.png)

Escaped \*not emphasis\* and a literal backtick: `` ` ``.

Reference [link][ref] and [shortcut].

[ref]: https://example.com/ref
[shortcut]: https://example.com/short
