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
    - [Functions](#functions)
        - [Tweak whitespace](#tweak-whitespace)
            - [trim `(s)`](#trim-s)
        - [To longer strings](#to-longer-strings)
            - [join `(separator list-of-strings)`](#join-separator-list-of-strings)
            - [concat `(&rest strings)`](#concat-rest-strings)
            - [repeat `(count s)`](#repeat-count-s)
        - [To and from lists](#to-and-from-lists)
            - [words `(s)`](#words-s)
            - [unwords `(strings)`](#unwords-strings)
            - [lines `(s)`](#lines-s)
            - [unlines `(strings)`](#unlines-strings)
            - [split `(separator s &key omit-nulls)`](#split-separator-s-key-omit-nulls)
        - [Predicates](#predicates)
            - [empty?, emptyp `(s)`](#empty-emptyp-s)
            - [blank?, blankp `(s)`](#blank-blankp-s)
            - [starts-with?, starts-with-p `(start s &key ignore-case)`](#starts-with-starts-with-p-start-s-key-ignore-case)
            - [ends-with?, ends-with-p `(end s &key ignore-case)`](#ends-with-ends-with-p-end-s-key-ignore-case)
        - [Others](#others)
            - [replace `(old new s)`](#replace-old-new-s)
    - [Dev and test](#dev-and-test)
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

## Functions

### Tweak whitespace

#### trim `(s)`
Remove whitespaces at the beginning and end of `s`.

```cl
(trim "  rst  ") ;; => "rst"
```

Also `trim-left` and `trim-right`.

Uses the built-in
[string-trim](https://lispcookbook.github.io/cl-cookbook/strings.html#trimming-blanks-from-the-ends-of-a-string)
where whitespaces are `'(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)`.

### To longer strings

#### join `(separator list-of-strings)`

Join strings in list `list-of-strings` with `separator` in between.

```cl
(join " " '("foo" "bar" "baz")) ;; => "foo bar baz"
```

Uses a specific [format](http://jtra.cz/stuff/lisp/sclr/format.html) syntax.

#### concat `(&rest strings)`

Join strings into one.

```cl
(concat "f" "o" "o") ;; => "foo"
```

Simple call of the built-in [concatenate](https://lispcookbook.github.io/cl-cookbook/strings.html#concatenating-strings).

#### repeat `(count s)`

Make a string of `s` repeated `count` times.

```cl
(repeat 3 "foo") ;; => "foofoofoo"
```


### To and from lists

#### words `(s)`

Return list of words, which were delimited by whitespace.

#### unwords `(strings)`

Join the list of strings with a whitespace.

#### lines `(s)`

Split string by newline character and return list of lines.

#### unlines `(strings)`

Join the list of strings with a newline character.

#### split `(separator s &key omit-nulls)`

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


### Predicates

#### empty?, emptyp `(s)`

True if `s` is nil or the empty string:

```cl
  (empty? nil) ;; => T
  (empty? "")  ;; => T
  (empty? " ") ;; => NIL
```

#### blank?, blankp `(s)`

True if `s` is empty or only contains whitespaces.

    (blankp "") ;; => T
    (blankp " ") ;; => T
    (emptyp " ") ;; => NIL

#### starts-with?, starts-with-p `(start s &key ignore-case)`

True if `s` starts with the substring `start`, nil otherwise. Ignore
case by default.

    (starts-with? "foo" "foobar") ;; => T
    (starts-with? "FOO" "foobar") ;; => NIL
    (starts-with? "FOO" "foobar" :ignore-case t) ;; => T

Calls `string=` or `string-equal` depending on the case, with their
`:start` and `:end` delimiters.

#### ends-with?, ends-with-p `(end s &key ignore-case)`

True if `s` ends with the substring `end`. Ignore case by default.

    (ends-with? "bar" "foobar") ;; => T

### Others

#### replace `(old new s)`

Replace `old` by `new` (no regexs) in `s`.

```cl
(replace "a" "o" "faa") ;; => "foo"
```

Uses
[cl-ppcre:regex-replace-all](http://weitz.de/cl-ppcre/#regex-replace-all)
but quotes the user input to not treat it as a regex.



## Dev and test

Test with [prove](https://github.com/fukamachi/prove).


## See also

* [cl-strings](https://github.com/diogoalexandrefranco/cl-strings), a
  similar (discovered afterwards), maybe more complete library, that
  does not use established libraries as dependencies as we do (with
  *potential* implementation
  [issues](https://github.com/diogoalexandrefranco/cl-strings/issues/2)).
* [cl-change-case](https://github.com/rudolfochrist/cl-change-case) to
  convert strings between camelCase, param-case, snake_case and more.
* the [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/strings.html), strings page.
