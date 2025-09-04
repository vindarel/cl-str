[![Quicklisp](http://quickdocs.org/badge/cl-str.svg)](http://quickdocs.org/cl-str)
# A modern and consistent Common Lisp string manipulation library

    (ql:quickload "str")

also on [Ultralisp](http://ultralisp.org/), [ocicl](https://github.com/ocicl/ocicl/) and [vend](https://github.com/fosskers/vend/).

Why ?

* modernity, simplicity and discoverability:

  - `(str:trim s)` instead of `(string-trim '(#\Backspace #\Tab #\Linefeed #\Newline #\Vt #\Page #\Return #\Space #\Rubout #\Next-Line #\No-break_space) s))`,
or `str:concat strings` instead of an unusual `format` construct; one discoverable library instead of many;

* consistence and composability, where `s` is always the last argument, which makes it
  easier to feed pipes and arrows.

* fixing built-in surprises: `(string-downcase nil`)  => `"nil"` the string, whereas `(str:downcase nil)` => `nil`.


The only dependency is `cl-ppcre`.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [A modern and consistent Common Lisp string manipulation library](#a-modern-and-consistent-common-lisp-string-manipulation-library)
    - [Install](#install)
    - [Global parameters](#global-parameters)
    - [Functions](#functions)
        - [Tweak whitespace](#tweak-whitespace)
            - [trim `(s &key (char-bag *whitespaces*))`](#trim-s-key-char-bag-whitespaces)
            - [collapse-whitespaces `(s)`](#collapse-whitespaces-s)
        - [To longer strings](#to-longer-strings)
            - [join `(separator list-of-strings)`](#join-separator-list-of-strings)
            - [concat `(&rest strings)`](#concat-rest-strings)
            - [ensure `(s &key wrapped-in prefix suffix)` NEW in March, 2023](#ensure-s-key-wrapped-in-prefix-suffix-new-in-march-2023)
            - [ensure-prefix, ensure-suffix `(start/end s)` NEW in March, 2023](#ensure-prefix-ensure-suffix-startend-s-new-in-march-2023)
            - [ensure-wrapped-in `(start/end s)`](#ensure-wrapped-in-startend-s)
            - [insert `(string/char index s)`](#insert-stringchar-index-s)
            - [repeat `(count s)`](#repeat-count-s)
            - [add-prefix, add-suffix `(items s)`](#add-prefix-add-suffix-items-s)
            - [pad `(len s &key (pad-side :right) (pad-char #\Space))`, pad-left, pad-right, pad-center (new in 0.16, 2019/12)](#pad-len-s-key-pad-side-right-pad-char-space-pad-left-pad-right-pad-center-new-in-016-201912)
        - [To shorter strings](#to-shorter-strings)
            - [substring `(start end s)`](#substring-start-end-s)
            - [s-first `(s)`](#s-first-s)
            - [s-last `(s)`](#s-last-s)
            - [s-rest `(s)`](#s-rest-s)
            - [s-nth `(n s)`](#s-nth-n-s)
            - [shorten `(len s &key ellipsis)`](#shorten-len-s-key-ellipsis)
        - [To a fixed length](#to-a-fixed-length)
            - [fit `(len s)`](#fit-len-s)
        - [To and from lists](#to-and-from-lists)
            - [words `(s)`](#words-s)
            - [unwords `(strings)`](#unwords-strings)
            - [lines `(s &key omit-nulls)`](#lines-s-key-omit-nulls)
            - [unlines `(strings)`](#unlines-strings)
            - [split `(separator s &key omit-nulls limit start end regex)`](#split-separator-s-key-omit-nulls-limit-start-end-regex)
            - [rsplit `(separator s &key limit regex)`](#rsplit-separator-s-key-limit-regex)
            - [split-omit-nulls `(separator s &key regex)`](#split-omit-nulls-separator-s-key-regex)
        - [To and from files](#to-and-from-files)
            - [from-file `(filename)`](#from-file-filename)
            - [to-file `(filename s)`](#to-file-filename-s)
        - [Predicates](#predicates)
            - [emptyp `(s)`](#emptyp-s)
            - [blankp `(s)`](#blankp-s)
            - [starts-with-p `(start s &key ignore-case)`](#starts-with-p-start-s-key-ignore-case)
            - [ends-with-p `(end s &key ignore-case)`](#ends-with-p-end-s-key-ignore-case)
            - [containsp `(substring s &key (ignore-case nil))`](#containsp-substring-s-key-ignore-case-nil)
            - [s-member `(list s &key (ignore-case *ignore-case*) (test #'string=))`](#s-member-list-s-key-ignore-case-ignore-case-test-string)
            - [prefixp and suffixp `(items s)`](#prefixp-and-suffixp-items-s)
            - [wrapped-in-p (`start/end` `s`) NEW in March, 2023](#wrapped-in-p-startend-s-new-in-march-2023)
        - [Case](#case)
            - [Functions to change case: camel-case, snake-case,...](#functions-to-change-case-camel-case-snake-case)
            - [downcase, upcase, capitalize `(s)` fixing a built-in suprise.](#downcase-upcase-capitalize-s-fixing-a-built-in-suprise)
            - [downcasep, upcasep `(s)`](#downcasep-upcasep-s)
            - [alphap, lettersp `(s)`](#alphap-lettersp-s)
            - [alphanump, lettersnump `(s)`](#alphanump-lettersnump-s)
            - [ascii-p `(char/s)`](#ascii-p-chars)
            - [digitp `(s)`](#digitp-s)
            - [has-alpha-p, has-letters-p, has-alphanum-p `(s)`](#has-alpha-p-has-letters-p-has-alphanum-p-s)
        - [Others](#others)
            - [replace-first `(old new s &key regex)`](#replace-first-old-new-s-key-regex)
            - [replace-all `(old new s &key regex)`](#replace-all-old-new-s-key-regex)
            - [replace-using `(replacement-list s &key regex)`](#replace-using-replacement-list-s-key-regex)
            - [remove-punctuation (s &key replacement)](#remove-punctuation-s-key-replacement)
            - [prefix `(list-of-strings)` (renamed in 0.9)](#prefix-list-of-strings-renamed-in-09)
            - [suffix `(list-of-strings)`](#suffix-list-of-strings)
            - [count-substring `(substring s &key start end)`](#count-substring-substring-s-key-start-end)
            - [s-assoc-value `(alist key)`](#s-assoc-value-alist-key)
    - [Macros](#macros)
        - [string-case](#string-case)
        - [match (experimental) · new in Feb, 2024](#match-experimental--new-in-feb-2024)
    - [Changelog](#changelog)
    - [Dev and test](#dev-and-test)
        - [Main test suite](#main-test-suite)
        - [Specific test suite](#specific-test-suite)
        - [Specific test](#specific-test)
        - [Test when defined](#test-when-defined)
    - [See also](#see-also)

<!-- markdown-toc end -->

## Install

Install with [Quicklisp](https://www.quicklisp.org/beta/):

    (ql:quickload :str)

Add it in your .asd's project dependencies, and call functions with the `str` prefix. It is not recommended to `:use :str` in a package. It's safer to use the `str` prefix.

Check its version:

    (str:version)

To get a newer version, you need to update the Quicklisp dist (think
of QL as Debian's apt rather than pip/npm/etc):

    (ql:update-dist "quicklisp")

Don't have a full Common Lisp development environment yet ? Get
[Portacle](https://shinmera.github.io/portacle/), a portable and
multiplatform development environment shipping Emacs, Quicklisp, SBCL
and Git. See also [editor
support](https://lispcookbook.github.io/cl-cookbook/editor-support.html)
(Vim, Lem, Atom, Eclipse,…).

## Global parameters

Some parameters are common to various functions and often used:
`:ignore-case` and `:omit-nulls`.

Consequently we can also manage them with global parameters:

~~~lisp
(let ((str:*ignore-case* t))
  (str:ends-with-p "BAR" "foobar"))
~~~

is equivalent to

~~~lisp
(str:ends-with-p "BAR" "foobar" :ignore-case t)
~~~

## Functions

### Tweak whitespace

#### trim `(s &key (char-bag *whitespaces*))`

Removes all characters in `char-bag` (default: whitespaces) at the beginning and end of `s`.
If supplied, `char-bag` has to be a sequence (e.g. string or list of characters).

```lisp
(str:trim "  rst  ") ;; => "rst"
(str:trim "+-*foo-bar*-+" :char-bag "+-*") => "foo-bar"
(str:trim "afood" :char-bag (concat "a" "d")) => "foo""
(str:trim "cdoooh" :char-bag (str:concat "c" "d" "h")) => "ooo"
```

Also `trim-left` and `trim-right`.

Uses the built-in
[string-trim](https://lispcookbook.github.io/cl-cookbook/strings.html#trimming-blanks-from-the-ends-of-a-string)
where whitespaces are `'(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)`.

#### collapse-whitespaces `(s)`

Ensure there is only one space character between words. Remove newlines.

~~~lisp
(str:collapse-whitespaces "foo  bar


  baz")
;; "foo bar baz"
;;T
~~~


### To longer strings

#### join `(separator list-of-strings)`

Join strings in list `list-of-strings` with `separator` (either a string or a char) in between.

```lisp
(join " " '("foo" "bar" "baz")) ;; => "foo bar baz"
(join #\Space '("foo" "bar" "baz")) ;; => "foo bar baz"
```

#### concat `(&rest strings)`

Join strings into one.

```lisp
(concat "f" "o" "o") ;; => "foo"
```

Simple call of the built-in [concatenate](https://lispcookbook.github.io/cl-cookbook/strings.html#concatenating-strings).

We actually also have `uiop:strcat`.

#### ensure `(s &key wrapped-in prefix suffix)` NEW in March, 2023

The "ensure-" functions return a string that has the specified prefix or suffix, appended if necessary.

This `str:ensure` function looks for the following key parameters, in order:

- `:wrapped-in`: if non nil, call `str:ensure-wrapped-in`. This checks that `s` both starts and ends with the supplied string or character.
- `:prefix` and `:suffix`: if both are supplied and non-nil, call `str:ensure-suffix` followed by `str:ensure-prefix`.
- `:prefix`: call `str:ensure-prefix`
- `:suffix`: call `str:ensure-suffix`.

Example:

~~~lisp
(str:ensure "abc" :wrapped-in "/")  ;; => "/abc/"
(str:ensure "/abc" :prefix "/")  ;; => "/abc"  => no change, still one "/"
(str:ensure "/abc" :suffix "/")  ;; => "/abc/" => added a "/" suffix.
~~~

These functions accept strings and characters:

~~~lisp
(str:ensure "/abc" :prefix #\/)
~~~

warn: if both `:wrapped-in` and `:prefix` (and/or `:suffix`) are supplied together, `:wrapped-in` takes precedence and `:prefix` (and/or `:suffix`) is ignored.


#### ensure-prefix, ensure-suffix `(start/end s)` NEW in March, 2023

Ensure that `s` starts with `start/end` (or ends with `start/end`, respectively).

Return a new string with its prefix (or suffix) added, if necessary.

Example:

~~~lisp
(str:ensure-prefix "/" "abc/") => "/abc/" (a prefix was added)
;; and
(str:ensure-prefix "/" "/abc/") => "/abc/" (does nothing)
~~~

#### ensure-wrapped-in `(start/end s)`

Ensure that `s` both starts and ends with `start/end`.

Return a new string with the necessary added bits, if required.

It simply calls `str:ensure-suffix` followed by `str:ensure-prefix`.

See also `str:wrapped-in-p` and `uiop:string-enclosed-p prefix s suffix`.

~~~lisp
(str:ensure-wrapped-in "/" "abc") ;; => "/abc/"  (added both a prefix and a suffix)
(str:ensure-wrapped-in "/" "/abc/") ;; => "/abc/" (does nothing)
~~~

#### insert `(string/char index s)`

Insert the given string (or character) at the index `index` into `s` and return a
new string.

If `index` is out of bounds, just return `s`.

```lisp
(str:insert "l" 2 "helo") ; => "hello"

(str:insert "o" 99 "hell") : => "hell"
```

#### repeat `(count s)`

Make a string of `s` repeated `count` times.

```lisp
(repeat 3 "foo") ;; => "foofoofoo"
```

**NEW** as of April, 2025: when the expected output is more than 10,000
characters, we generate it in chunks to avoid stack/heap overflows.

#### add-prefix, add-suffix `(items s)`

Respectively prepend or append `s` to the front of each item.


#### pad `(len s &key (pad-side :right) (pad-char #\Space))`, pad-left, pad-right, pad-center (new in 0.16, 2019/12)

Fill `s` with characters until it is of the given length. By default,
add spaces on the right:

~~~lisp
(str:pad 10 "foo")
"foo       "
~~~

* `pad-side`: one of `:right` (the default), `:left` or `:center`. See `*pad-side*`.
* `pad-char`: the padding character (or string of one character). Defaults to a space. See `*pad-char*`.

~~~lisp
(str:pad 10 "foo" :pad-side :center :pad-char "+")
"+++foo++++"
~~~

If the given length is smaller than the length o `s`, return `s`.

Filling with spaces can easily be done with format:

~~~lisp
(format nil "~va" len s) ;; => "foo       "
(format nil "~v@a" 10 "foo") ;; => "       foo" (with @)
~~~


### To shorter strings

#### substring `(start end s)`

Return the substring of `s` from `start` to `end`.

It uses `subseq` with differences:

* argument order, s at the end
* `start` and `end` can be lower than 0 or bigger than the length of s.
* for convenience `end` can be nil or t to denote the end of the string.

Examples:

```lisp
  (is "abcd" (substring 0 t "abcd") "t denotes the end of the string")
  (is "abcd" (substring 0 nil "abcd") "nil too")
  (is "abcd" (substring 0 100 "abcd") "end can be too large")
  (is "abc" (substring 0 -1 "abcd") "end can be negative. Counts from the end.")
  (is "" (substring 0 -100 "abcd") "end can be negative and too low")
  (is "" (substring 100 1 "abcd") "start can be too big")
  (is "abcd" (substring -100 4 "abcd") "start can also be too low")
  (is "" (substring 2 1 "abcd") "start is bigger than end")
```

#### s-first `(s)`

Return the first letter of `s`.


Examples:

```lisp
  (s-first "foobar") ;; => "f"
  (s-first "") ;; => ""
```

#### s-last `(s)`

Return the last letter of `s`.


#### s-rest `(s)`

Return the rest substring of `s`.

Examples:

```lisp
  (s-rest "foobar") ;; => "oobar"
  (s-rest "") ;; => ""
```

#### s-nth `(n s)`

Return the nth letter of `s`.

Examples:

```lisp
  (s-nth 3 "foobar") ;; => "b"
  (s-nth 3 "") ;; => ""
```

You could also use

~~~lisp
(elt "test" 1)
;; => #\e
(string (elt "test" 1))
;; => "e"
~~~

#### shorten `(len s &key ellipsis)`

If `s` is longer than `len`, truncate it and add an ellipsis at the
end (`...` by default). `s` is cut down to `len` minus the length of
the ellipsis (3 by default).

Optionally, give an `:ellipsis` keyword argument. Also set it globally
with `*ellipsis*`.

~~~lisp
(shorten 8 "hello world")
;; => "hello..."
(shorten 3 "hello world")
;; => "..."
(shorten 8 "hello world" :ellipsis "-")
;; => "hello w-"
(let ((*ellipsis* "-"))
  (shorten 8 "hello world"))
;; => "hello w-"
~~~

### To a fixed length

#### fit `(len s)`

Fit this string to the given length:

- if it's too long, shorten it (showing the `ellipsis`),
- if it's too short, add paddding (to the side `pad-side`, adding the
  character `pad-char`).

As such, it accepts the same key arguments as `str:shorten` and
`str:pad`: `ellipsis`, `pad-side`, `pad-char`…

~~~lisp
CL-USER> (str:fit 10 "hello" :pad-char "+")
"hello+++++"

CL-USER> (str:fit 10 "hello world" :ellipsis "…")
"hello wor…"
~~~

If, like me, you want to print a list of data as a table, see:

- [cl-ansi-term](https://github.com/vindarel/cl-ansi-term/)

~~~lisp
CL-USER> (ql:quickload "cl-ansi-term")
CL-USER> (term:table '(("name" "age" "email")
              ("me" 7 "some@blah")
              ("me" 7 "some@with-some-longer.email"))
             :column-width '(10 4 20))
+---------+---+-------------------+
|name     |age|email              |
+---------+---+-------------------+
|me       |7  |some@blah          |
+---------+---+-------------------+
|me       |7  |some@with-some-l(…)|
+---------+---+-------------------+
~~~

- [cl-ascii-table](https://github.com/telephil/cl-ascii-table/)

~~~lisp
CL-USER> (ql:quickload "cl-ascii-table")
CL-USER> (let ((table (ascii-table:make-table '("Id" "Name" "Amount") :header "Infos")))
  (ascii-table:add-row table '(1 "Bob" 150))
  (ascii-table:add-row table '(2 "Joe" 200))
  (ascii-table:add-separator table)
  (ascii-table:add-row table '("" "Total" 350))
  (ascii-table:display table))

.---------------------.
|        Infos        |
+----+-------+--------+
| Id | Name  | Amount |
+----+-------+--------+
|  1 | Bob   |    150 |
|  2 | Joe   |    200 |
+----+-------+--------+
|    | Total |    350 |
+----+-------+--------+
NIL
~~~


### To and from lists

#### words `(s)`

Return list of words, which were delimited by whitespace.

#### unwords `(strings)`

Join the list of strings with a whitespace.

#### lines `(s &key omit-nulls)`

Split string by newline character and return list of lines.

A terminal newline character does *not* result in an extra empty string
(new in **v0.14**, october 2019).


#### unlines `(strings)`

Join the list of strings with a newline character.

#### paragraphs, unparagraphs

Split the string by \n\n: paragraphs are sections of text separated by a blank line
(two #\Newline characters in a row).

Return a list of strings.

Each paragraph has whitespace strimmed around it. As such, the
operation `(unparagraphs (paragraphs s))` doesn't always re-create
`s`, it creates a new string with less blank lines.

Equivalent to `ppcre:split "\\n\\n" s`, plus trimming whitespace on the results.

The `unparagraphs` functions joins the list of strings by a blank line.


#### split `(separator s &key omit-nulls limit start end regex)`

Split into subtrings. If
`omit-nulls` is non-nil, zero-length substrings are omitted.

By default, metacharacters are treated as normal characters.
 If `regex` is not `nil`, then `separator` is treated as regular expression.

```lisp
(split "+" "foo++bar") ;; => ("foo" "" "bar")
(split #\+ "foo++bar") ;; => ("foo" "" "bar")
(split "+" "foo++bar" :omit-nulls t) ;; => ("foo" "bar")

(split "[,|;]" "foo,bar;baz") ;; => ("foo,bar;baz")
(split "[,|;]" "foo,bar;baz" :regex t) ;; => ("foo" "bar" "baz")
```

cl-ppcre has an inconsistency such that when the separator appears at
the end, it doesn't return a trailing empty string. But we do **since
v0.14** (october, 2019).

#### rsplit `(separator s &key limit regex)`

Similar to `split`, but split from the end. In particular, this will
be different from `split` when a `:limit` is provided, but in more
obscure cases it can be different when there are multiple different
ways to split the string.

```lisp
(rsplit "/" "/var/log/mail.log" :limit 2) ;; => ("/var/log" "mail.log")
```

~~~lisp
(cl-ppcre:split " " "a b c ")
("a" "b" "c")

(str:split " " "a b c ")
("a" "b" "c" "")
~~~


#### split-omit-nulls `(separator s &key regex)`

Because it is a common pattern and it can be clearer than an option
coming after many parenthesis.


### To and from files

#### from-file `(filename)`

Read the file and return its content as a string.

Example: `(str:from-file "path/to/file.txt")`.

`:external-format`: if nil, the system default. Can be bound to `:utf-8`.

But you might just call
[uiop's `uiop:read-file-string`](https://github.com/fare/asdf/blob/master/uiop/stream.lisp#L445)
directly.

There is also `uiop:read-file-lines`.

#### to-file `(filename s)`

Write the string `s` to the file `filename`. If the file does not
exist, create it, if it already exists, replace it.

Options:

* `:if-does-not-exist`: `:create` (default), `:error`
* `:if-exists`: `:supersede` (default), `:append`, `:overwrite`, `:rename`, `:error`,...

Returns the string written to file.


### Predicates

#### emptyp `(s)`

True if `s` is nil or the empty string:

```lisp
  (emptyp nil) ;; => T
  (emptyp "")  ;; => T
  (emptyp " ") ;; => NIL
```

See also `str:non-empty-string-p`, which adds a `stringp` check.

#### blankp `(s)`

True if `s` is empty or only contains whitespaces.

    (blankp "") ;; => T
    (blankp " ") ;; => T
    (emptyp " ") ;; => NIL

See also `str:non-blank-string-p`.

#### starts-with-p `(start s &key ignore-case)`

True if `s` starts with the substring `start`, nil otherwise. Ignore
case by default.

    (starts-with-p "foo" "foobar") ;; => T
    (starts-with-p "FOO" "foobar") ;; => NIL
    (starts-with-p "FOO" "foobar" :ignore-case t) ;; => T

Calls `string=` or `string-equal` depending on the case, with their
`:start` and `:end` delimiters.

#### ends-with-p `(end s &key ignore-case)`

True if `s` ends with the substring `end`. Ignore case by default.

    (ends-with-p "bar" "foobar") ;; => T

`end` can be a string or a character.

#### containsp `(substring s &key (ignore-case nil))`

Return true if `s` contains `substring`, nil otherwise. Ignore the
case with `:ignore-case t` (don't ignore by default).

Based on a simple call to the built-in `search` (which returns the
position of the substring).

#### s-member `(list s &key (ignore-case *ignore-case*) (test #'string=))`

Return T if `s' is a member of `list'. Do not ignore case by default.

NOTE: `s-member`'s arguments' order is the reverse of CL's `member`.

If `:ignore-case` or `*ignore-case*` are not nil, ignore case (using
`string-equal` instead of `string=`).

Unlike CL's `member`, `s-member` returns T or NIL, instead of the tail of LIST whose first element satisfies the test.


#### prefixp and suffixp `(items s)`

Return `s` if all `items` start (or end) with it.

See also `uiop:string-prefix-p prefix s`, which returns `t` if
`prefix` is a prefix of `s`,

and `uiop:string-enclosed-p prefix s suffix`, which returns `t` if `s`
begins with `prefix` and ends with `suffix`.

#### wrapped-in-p (`start/end` `s`) NEW in March, 2023

Does `s` start and end with `start/end'?

If true, return `s`. Otherwise, return nil.

Example:

~~~lisp
(str:wrapped-in-p "/" "/foo/"  ;; => "/foo/"
(str:wrapped-in-p "/" "/foo"  ;; => nil
~~~

See also: `UIOP:STRING-ENCLOSED-P (prefix s suffix)`.


### Case

#### Functions to change case: camel-case, snake-case,...

We use
[cl-change-case](https://github.com/rudolfochrist/cl-change-case/) (go
thank him and star the repo!).
We adapt these functions to also accept symbols and characters (like the inbuilt casing functions).
Also the functions return `nil` when argument is `nil`.

The available functions are:

```
:no-case (s &key replacement)
:camel-case (s &key merge-numbers)
:dot-case
:header-case
:param-case
:pascal-case
:path-case
:sentence-case
:snake-case
:swap-case
:title-case
:constant-case
```

More documentation and examples are there.


#### downcase, upcase, capitalize `(s)` fixing a built-in suprise.

The functions `str:downcase`, `str:upcase` and `str:capitalize` return
a new string. They call the built-in `string-downcase`,
`string-upcase` and `string-capitalize` respectively, but they fix
something surprising. When the argument is `nil`, the built-ins return
"nil" or "NIL" or "Nil", a *string*. Indeed, they work on anything:

    (string-downcase nil) ;; => "nil" the string !
    (str:downcase nil) ;; nil

    (string-downcase :FOO) ;; => "foo"

#### downcasep, upcasep `(s)`

These functions return `t` if the given string contains at least one
letter and all its letters are lowercase or uppercase, respectively.

```lisp
(is (downcasep " a+,. ") t "downcasep with one letter and punctuation is true.")
(is (downcasep " +,. ") nil "downcasep with only punctuation or spaces is false")
```

#### alphap, lettersp `(s)`

`alphap` returns t if `s` contains at least one character and all characters are
  alpha (as in `"^[a-zA-Z]+$"`).

`lettersp` works for unicode letters too.

~~~lisp
(is (alphap "abcdeé") nil "alphap is nil with accents")
(is (lettersp "éß") t "lettersp is t with accents and ß")
~~~

#### alphanump, lettersnump `(s)`

`alphanump` returns t if `s` contains at least one character and all characters are alphanumeric (as in `^[a-zA-Z0-9]+$`).

`lettersnump` also works on unicode letters (as in `^[\\p{L}a-zA-Z0-9]+$`).

#### ascii-p `(char/s)`

Return t if the character / string is an ASCII character / is composed of ASCII characters.

An ASCII character has a `char-code` inferior to 128.

#### digitp `(s)`

Returns t if `s` contains at least one character and all characters are numerical (as for `digit-char-p`).

#### has-alpha-p, has-letters-p, has-alphanum-p `(s)`

Return t if `s` has at least one alpha, letter, alphanum character (as with `alphanumericp`).

### Others

#### replace-first `(old new s &key regex)`

Replace the first occurence of `old` by `new` in `s`.

By default, metacharacters are treated as normal characters.
If `regex` is not `nil`, then `old` is treated as regular expression.

```lisp
(replace-first "a" "o" "faa") ;; => "foa"
(replace-first "fo+" "frob" "foofoo bar" :regex t) ;; => "frobfoo bar"
```

Uses
[cl-ppcre:regex-replace](http://weitz.de/cl-ppcre/#regex-replace)
but quotes the user input to not treat it as a regex (if regex is nil).

#### replace-all `(old new s &key regex)`

Replace all occurences of `old` by `new` in `s`.

By default, metacharacters are treated as normal characters.
If `regex` is not `nil`, `old` is treated as regular expression.

```lisp
(replace-all "a" "o" "faa") ;; => "foo"
(replace-all "fo+" "frob" "foofoo bar" :regex t) ;; => "frobfrob bar"
```

Uses
[cl-ppcre:regex-replace-all](http://weitz.de/cl-ppcre/#regex-replace-all)
but quotes the user input to not treat it as a regex (if regex is nil).

If the replacement is only one character, you can use `substitute`:

    (substitute #\+ #\Space "foo bar baz")
    ;; "foo+bar+baz"


#### replace-using `(replacement-list s &key regex)`

Replace all associations given by pairs in a replacement-list and return a new string.

The `replacement-list` alternates a string to replace (case sensitive) and its replacement.
By default, metacharacters in the string to replace are treated as normal characters.
If `regex` is not `nil`, strings to replace are treated as regular expression.

Example:

```lisp
(replace-using (list "%phone%" "987")
               "call %phone%")
;; => "call 987"

(replace-using (list "fo+" "frob"
                       "ba+" "Bob")
                 "foo bar"
                 :regex t)
;; => "frob Bobr"
```

#### remove-punctuation (s &key replacement)

Remove the punctuation characters from `s`, replace them with
`replacement` (defaults to a space) and strip continuous whitespace.

~~~lisp
(str:remove-punctuation "I say: - 'Hello, world?'") ;; => "I say Hello world"
~~~

Use `str:no-case` to remove punctuation and return the string as lower-case.


#### prefix `(list-of-strings)` (renamed in 0.9)

(renamed from `common-prefix` in v0.9)

Find the common prefix between strings.

Example: `(str:prefix '(\"foobar\" \"foozz\"))` => \"foo\"

Uses the built-in `mismatch`, that returns the position at which
the strings fail to match.

Return a string or nil when the input is the void list.


#### suffix `(list-of-strings)`

Find the common suffix between strings.

#### count-substring `(substring s &key start end)`
Counts the non-overlapping occurrences of `substring` in `s`.
You could also count only the ocurrencies between `start` and `end`.

Examples:
~~~ lisp
(count-substring "abc" "abcxabcxabc")
;; => 3
~~~
~~~lisp
(count-substring "abc" "abcxabcxabc" :start 3 :end 7)
;; => 1
~~~

#### s-assoc-value `(alist key)`

Returns the value of a cons cell in `alist` with key `key`, when `key` is a string.
The second return value is the cons cell, if any was matched.

The arguments are in the opposite order of `cl:assoc`'s, but are consistent
with `alexandria:assoc-value` (and `str`).

```lisp
(s-assoc-value '(("hello" . 1)) "hello")
;; 1
;; ("hello" . 1)

(alexandria:assoc-value '(("hello" . 1)) "hello")
;; NIL
(alexandria:assoc-value '(("hello" . 1)) "hello" :test #'string=)
;; 1
;; ("hello" . 1)

(assoc "hello" '(("hello" . 1)))
;; NIL
(assoc "hello" '(("hello" . 1)) :test #'string=)
;; ("hello" . 1)
(cdr *)
;; 1
```

## Macros

### string-case

A case-like macro that works with strings (CL case's test function is
`eql`, and that isn't enough for strings).

Example:

~~~lisp
(str:string-case "hello"
  ("foo" 1)
  (("hello" "test") 5)
  (nil (print "input is nil"))
  (otherwise (print "non of the previous forms was caught.")))
~~~

You might also like pattern matching. The example below with
[trivia](https://github.com/guicho271828/trivia/) is very similar:

~~~lisp
(trivia:match "hey"
  ("hey" (print "it matched"))
  (otherwise :nothing))
~~~

Note that there is also http://quickdocs.org/string-case/.

### match (experimental) · new in Feb, 2024

A COND-like macro to match substrings and bind variables to matches. Regular expressions are allowed for matches.

`_` is a placeholder that is ignored.

THIS MACRO IS EXPERIMENTAL and might break in future releases.

Example:

```lisp
(str:match "a 1 b 2 d"
  (("a " x " b " y " d") ;; => matched
   (+ (parse-integer x) (parse-integer y)))
  (t
   'default-but-not-for-this-case)) ;; default branch
;; => 3

(str:match "a 1 b c d"
  (("a 2 b" _ "d") ;; => not matched
   (print "pass"))
  (("a " _ " b c d") ;; => matched
   "here we go")
  (t 'default-but-not-for-this-case)) ;; default branch
;; => "here we go"
```

Match with regexs:

```lisp
(str:match "123 hello 456"
 (("\\d+" s "\\d+")
   s)
 (t "nothing"))
;; => " hello "
```

## Changelog

* September, 2025:
  * optimized `repeat` further and fixed for LispWorks (no more using `apply` and its call argument limit), thanks to @yehouda.
  * small backward uncompatible correctness changes, thanks to @swapneils:
    * `join` now treats a `nil` separator as the empty string, rather than the string `"nil"`, conforming to the semantics in the README of not converting `nil` to `"nil"`.
    * `containsp` now automatically returns `nil` if either string input is `nil`.
    * `alphanump` and similar now use the correct regex checks for multi-line strings, rather than accepting an input if it has any line matching their specification.
  * optimization changes, thanks to @swapneils:
    * multiple non-recursive functions have been inlined.
    * multiple functions have `ftype` declarations.
* April, 2025:
  * optimized `repeat` for large workloads. When the workload is
    sufficiently large, `str:repeat` allocates all chunks of the
    expected output onto the stack to call concat, causing an
    overflow. For workloads larger than 10,000 characters in the
    expected output, it now chunks the work to minimize the chance of
    overflowing the stack or heap, while retaining decent performance.
* Feb, 2024:
  * added the `match` macro. It is EXPERIMENTAL and might change in future versions. We welcome your bug reports and feedback.
* 0.21, November, 2023:
  * added the `regex` key argument to `split`, `rsplit`, `split-omit-nulls`.
* August, 2023:
  * added the `regex` key argument to the `replace-*` functions.
* March, 2023:
  * added `str:ensure`, `str:ensure-prefix`, `str:ensure-suffix`, `str:ensure-wrapped-in` and `str:wrapped-in-p`.
* January, 2023: added the `:char-barg` parameter to `trim`, `trim-left`, `trim-right`.
  - minor: `ends-with-p` now works with a character.
* June, 2022: small breaking change: fixed `prefixp` when used with a smaller prefix: "f" was not recognized as a prefix of "foobar" and "foobuz", only "foo" was. Now it is fixed. Same for `suffixp`.
* Feb, 2022: added `fit`: fit the string to the given length: either shorten it, either padd padding.
* 0.20, May, 2021: added `ascii-p`.
* 0.19.1, May, 2021: speed up `join` (by a factor of 4).
* 0.19, October, 2020: added s-member
*0.18.1, September, 2020: fix replace-all edge case when the replacement string ends with two backslashes and a single quote.
* 0.18, June, 2020: added `replace-using`.
* 0.17, April 2020:
  - added `collapse-whitespaces`
  - `join` and `split` also accept a char as separator
  - fixed `remove-punctuation` that did not respect the case. Use `no-case` for this
  - fixed `from-file` "odd number of arguments" error.
* 0.16, November 2019: added `pad`, `pad-[left, right, center]`.
* 0.15, October 2019: added functions to change case (based on cl-change-case).
  added remove-punctuation.
* 0.14, October, 2019: fixed the cl-ppcre inconsistency in `split` and `lines`. A trailing separator now returns a trailing empty string.

Before:

~~~lisp
(str:split " " "a b c ")
("a" "b" "c")  ;; like cl-ppcre:split
~~~

Now:

~~~lisp
(str:split " " "a b c ")
("a" "b" "c" "")
~~~

* august, 2019: deprecated `prune`, renamed to `shorten`.
* added `:limit` to `split`.
* 0.13 june, 2019
  - added `insert`
* 0.12
  - added case predicates (`downcasep`, `alphap`, `has-x` and friends).
* 0.11 (Quicklisp end of march, 2019, also in Ultralisp)
  - added `str:downcase`, `str:upcase` and `str:capitalize`, that fix the `nil` argument surprise.
* 0.10
  - `split` doesn't fix cl-ppcre's inconsistency anymore (when the separator appears at the end). See issue #18. So `(str:split "xx" "fooxxbarxx")` doesn't return a trailing `""`.
  - added `s-last`
  - `s-first` and friends return `nil` when appropriate, not `""`.
* 0.9
  - added `s-first` , `s-rest` and `s-nth`
  - added `prefix` and `suffix` functions and predicates.
  - added `prune`.
* 0.8 added `string-case`
* 0.7 added `version`
* 0.6 added `split-omit-nulls` (QL, january 2018)
* 0.5 added `common-prefix`
* 0.4 added `from-file` and `to-file`.
* 0.3 added `substring`.

## Dev and test

Regression testing is implemented with [fiveam](https://github.com/lispci/fiveam).

### Main test suite

Either use
```lisp
  (asdf:test-system :str)
```

or load the test package `str.test` and then
```lisp
  (fiveam:run! 'test-str:str)
```

### Specific test suite

```lisp
  (fiveam:run! 'test-str:replace-functions)
```

Test suite names:
- replace-functions
- lengthen-functions
- ensure-functions
- pad-functions
- substring-functions
- list-functions
- from-list-to-string
- from-list-to-list
- from-string-to-list
- predicates, case-functions
- miscellaneous

### Specific test

```lisp
  (fiveam:run! 'test-str::downcase) ;; (test symbols are unexported)
```

### Test when defined

First you need to
```lisp
(setf fiveam:*run-test-when-defined* t)
```
then the test is run after each definition / compilation.
This can be done with C-c C-c on emacs.

## See also

* the [Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/strings.html), strings page.
* my [Common Lisp course on Udemy: from novice to effective developer](https://www.udemy.com/course/common-lisp-programming/?referralCode=2F3D698BBC4326F94358). Check out my blog for regular coupons.
* https://lisp-journey.gitlab.io/
* video: [how to create a Common Lisp project from scratch with our project generator](https://www.youtube.com/watch?v=XFc513MJjos): it sums up in 5 minutes what took me a much longer time to gather.

Inspired by the famous Emacs Lisp's [s.el](https://github.com/magnars/s.el).
