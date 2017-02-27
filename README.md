# A modern and consistent Common Lisp string manipulation library

straight from the famous Emacs Lisp's [s.el](https://github.com/magnars/s.el).

Why ?

* modernity, simplicity and discoverability:

`(str:trim s)` instead of `  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) s))`,
or `str:concat strings` instead of an unusual `format` construct;

* consistance and composability, where `s` is always the last argument, which makes it
  easier to feed pipes and arrows.

## Install

Install with [Quicklisp](https://www.quicklisp.org/beta/):

    (ql:quickload :str)

(don't have a full Common Lisp development environment yet ? Get
[Portacle](https://shinmera.github.io/portacle/), a portable and
multiplatform development environment shipping Emacs, Quicklisp, SBCL
and Git).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [A modern and consistent Common Lisp string manipulation library](#a-modern-and-consistant-common-lisp-string-manipulation-library)
    - [Functions and macros](#functions-and-macros)
        - [trim `(s)`](#trim-s)
        - [join `(separator list-of-strings)`](#join-separator-list-of-strings)
        - [concat `(&rest strings)`](#concat-rest-strings)
        - [split `(separator s)`](#split-separator-s)
        - [replace `(old new s)`](#replace-old-new-s)
        - [blank-p `(s)`](#blank-p-s)
        - [blank-str-p `(s)`](#blank-str-p-s)
    - [Dev and test](#dev-and-test)
    - [Build the doc](#build-the-doc)

<!-- markdown-toc end -->

## Functions and macros

### trim `(s)`
Remove whitespaces at the beginning and end of `s`.

```cl
(trim "  rst  ") ;; => "rst"
```

Also `trim-left` and `trim-right`.

Uses the built-in `string-trim` where whitespaces are `'(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)`.

### join `(separator list-of-strings)`

Join strings in list `list-of-strings` with `separator` in between.

```cl
(join " " '("foo" "bar" "baz")) ;; => "foo bar baz"
```

Uses a specific `format` syntax.

### concat `(&rest strings)`

Join strings into one.

```cl
(concat "f" "o" "o") ;; => "foo"
```

Simple macro around the built-in `concatenate`.

### split `(separator s)`

Split into subtrings (with a regexp).

```cl
(split " " "foo bar") ;; => ("foo" "bar")
```

Simple wrapper around `cl-ppcre:split`.

### replace `(old new s)`

Replace `old` by `new` (no regexs) in `s`.

```cl
(replace "a" "o" "faa") ;; => "foo"
```

Uses `cl-ppcre:regex-replace-all` with quoting the user input to not treat it as a regex.

### blank-p `(s)`

True if `s` is nil or the empty string:

```cl
  (blank? nil) ;; => T
  (blank? "")  ;; => T
  (blank? " ") ;; => NIL
```

### blank-str-p `(s)`

Also True if `s` only contains whitespaces.


## Dev and test

Test with [prove](https://github.com/fukamachi/prove).

## Build the doc

[https://commondoc.github.io/codex/docs/tutorial.html](https://commondoc.github.io/codex/docs/tutorial.html)

    (codex:document :cl-str)
