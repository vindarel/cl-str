# A modern and consistent Common Lisp string manipulation library

straight from the famous Emacs Lisp's [s.el](https://github.com/magnars/s.el).

Why ?

* modernity, simplicity and discoverability:

  - `(str:trim s)` instead of `  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) s))`,
or `str:concat strings` instead of an unusual `format` construct; one discoverable library instead of many;

* consistance and composability, where `s` is always the last argument, which makes it
  easier to feed pipes and arrows.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->

**Table of Contents**

- [A modern and consistent Common Lisp string manipulation library](#a-modern-and-consistent-common-lisp-string-manipulation-library)
    - [Install](#install)
    - [Functions and macros](#functions-and-macros)
        - [trim `(s)`](#trim-s)
        - [join `(separator list-of-strings)`](#join-separator-list-of-strings)
        - [concat `(&rest strings)`](#concat-rest-strings)
        - [split `(separator s &key omit-nulls)`](#split-separator-s-key-omit-nulls)
        - [replace `(old new s)`](#replace-old-new-s)
        - [emptyp `(s)`](#emptyp-s)
        - [blankp `(s)`](#blankp-s)
    - [Dev and test](#dev-and-test)
    - [Build the doc](#build-the-doc)
    - [See also](#see-also)

<!-- markdown-toc end -->

## Install

Install with [Quicklisp](https://www.quicklisp.org/beta/):

    (ql:quickload :str)

beware, this is a young and unstable library.

(don't have a full Common Lisp development environment yet ? Get
[Portacle](https://shinmera.github.io/portacle/), a portable and
multiplatform development environment shipping Emacs, Quicklisp, SBCL
and Git).

## Functions and macros

### trim `(s)`
Remove whitespaces at the beginning and end of `s`.

```cl
(trim "  rst  ") ;; => "rst"
```

Also `trim-left` and `trim-right`.

Uses the built-in
[string-trim](https://lispcookbook.github.io/cl-cookbook/strings.html#trimming-blanks-from-the-ends-of-a-string)
where whitespaces are `'(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)`.

### join `(separator list-of-strings)`

Join strings in list `list-of-strings` with `separator` in between.

```cl
(join " " '("foo" "bar" "baz")) ;; => "foo bar baz"
```

Uses a specific [format](http://jtra.cz/stuff/lisp/sclr/format.html) syntax.

### concat `(&rest strings)`

Join strings into one.

```cl
(concat "f" "o" "o") ;; => "foo"
```

Simple call of the built-in [concatenate](https://lispcookbook.github.io/cl-cookbook/strings.html#concatenating-strings).

### split `(separator s &key omit-nulls)`

Split into subtrings (unlike cl-ppcre, without a regexp). If
`omit-nulls` is non-nil, zero-length substrings are omitted.

```cl
(split "+" "foo++bar") ;; => ("foo" "" "bar")
(split "+" "foo++bar" :omit-nulls t) ;; => ("foo" "bar")
```

Wrapper around [cl-ppcre:split](http://weitz.de/cl-ppcre/#split) but:

- our separator is a simple string, where cl-ppcre takes a regexp,
- we fix an inconsistency:

```
(cl-ppcre:split "," ",a,b,,c,") ;; => ("" "a" "b" "" "c")
```

and we return a trailing `""`:

    (split "," ",a,b,,c,") ;; => ("" "a" "b" "" "c" "")

### replace `(old new s)`

Replace `old` by `new` (no regexs) in `s`.

```cl
(replace "a" "o" "faa") ;; => "foo"
```

Uses
[cl-ppcre:regex-replace-all](http://weitz.de/cl-ppcre/#regex-replace-all)
but quotes the user input to not treat it as a regex.

### emptyp `(s)`

True if `s` is nil or the empty string:

```cl
  (empty? nil) ;; => T
  (empty? "")  ;; => T
  (empty? " ") ;; => NIL
```

### blankp `(s)`

True if `s` is empty or only contains whitespaces.

    (blankp "") ;; => T
    (blankp " ") ;; => T
    (emptyp " ") ;; => NIL

## Dev and test

Test with [prove](https://github.com/fukamachi/prove).

## Build the doc

[https://commondoc.github.io/codex/docs/tutorial.html](https://commondoc.github.io/codex/docs/tutorial.html)

    (codex:document :cl-str)

## See also

* [cl-strings](https://github.com/diogoalexandrefranco/cl-strings), a
  similar (discovered afterwards), maybe more complete library, that
  does not use established libraries as dependencies as we do (with
  *potential* implementation
  [issues](https://github.com/diogoalexandrefranco/cl-strings/issues/2)).
