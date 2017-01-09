# The long lost Common Lisp string manipulation library

straight from the famous Emacs Lisp's [s.el](https://github.com/magnars/s.el).

Why ?

* modernity, simplicity and discoverability:

`(s-trim s)` instead of `  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) s))`,
or `s-concat strings` instead of an unusual `format` construct;

* consistance and composability, where `s` is always the last argument, which makes it
  easier to feed pipes and arrows.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->

**Table of Contents**

- [The long lost Common Lisp string manipulation library](#the-long-lost-common-lisp-string-manipulation-library)
    - [Functions and macros](#functions-and-macros)
        - [s-trim `(s)`](#s-trim-s)
        - [s-join `(separator list-of-strings)`](#s-join-separator-list-of-strings)
        - [s-concat `(&rest strings)`](#s-concat-rest-strings)
        - [s-split `(separator s)`](#s-split-separator-s)
        - [s-replace `(old new s)`](#s-replace-old-new-s)
    - [Build the doc](#build-the-doc)

<!-- markdown-toc end -->

## Functions and macros

### s-trim `(s)`
Remove whitespaces at the beginning and end of `s`.

```cl
(s-trim "  rst  ") ;; => "rst"
```

Also `s-trim-left` and `s-trim-right`.

Uses the built-in `string-trim` where whitespaces are `'(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)`.

### s-join `(separator list-of-strings)`

Join strings in list `list-of-strings` with `separator` in between.

```cl
(s-join " " '("foo" "bar" "baz")) ;; => "foo bar baz"
```

Uses a specific `format` syntax.

### s-concat `(&rest strings)`

Join strings into one.

```cl
(s-concat "f" "o" "o") ;; => "foo"
```

Simple macro around the built-in `concatenate`.

### s-split `(separator s)`

Split into subtrings (with a regexp).

```cl
(s-split " " "foo bar") ;; => ("foo" "bar")
```

Simple wrapper around `cl-ppcre:split`.

### s-replace `(old new s)`

Replace `old` by `new` (no regexs) in `s`.

```cl
(s-replace "a" "o" "faa") ;; => "foo"
```

Uses `cl-ppcre:regex-replace-all` with quoting the user input to not treat it as a regex.

## Build the doc

[https://commondoc.github.io/codex/docs/tutorial.html](https://commondoc.github.io/codex/docs/tutorial.html)

    (codex:document :cl-s)
