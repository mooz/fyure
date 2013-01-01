# fyure

An interface to fix Japanese hyoki-yure for Emacs

## Requirements

- MeCab (http://mecab.googlecode.com/svn/trunk/mecab/doc/index.html)
  - Requires **UTF-8** dictionary (naist-jdic, ipadic, ...)
  - `naist-jdic` is recommended (Since it has hyoki-yure information whereas `ipadic` doesn't have)
- Python
  - python-mecab
- Emacs
  - helm.el (https://github.com/emacs-helm/helm) or anything.el (http://www.emacswiki.org/Anything)

## Installation

### Ubuntu

First, install required packages via `apt`.

    $ sudo apt-get install mecab mecab-ipadic-utf8 python-mecab

If your Emacs desn't have either `helm.el` or `anything.el`, install one of them.

Then, clone fyure and insall it to your `site-lisp` directory.

    $ git clone git://github.com/mooz/fyure.git
    $ cd fyure
    $ make
    $ cp fyure.el fyure.elc fyure.py YOUR_SITE_LISP_PATH

Finally, add the following configuration code into your `.emacs` file.

```lisp
(autoload 'fyure:start-fixing "fyure" "An interface to fix Japanese hyoki-yure." t)
```

## Usage

In your Emacs, type `M-x fyure:start-fixing` and start fixing procedure. Fixing procedure consists of the following three steps.

1. Select a target *word-group*.
2. Select representative *word* for the *selected word-group*.
3. Replace words in the *word-group* with the word selected in *step 2* in `query-replace` fashion.

## Setup

### Custom dictionary path

Without any settings, fyure.el uses *system-default mecab dictionary* specified in `/etc/mecabrc`. If you prefer custom dictionary for fyure.el, specify the dictionary's path to `fyure:mecab-dictionary-path` as follows.

```lisp
(setq fyure:mecab-dictionary-path "/usr/lib/mecab/dic/naist-jdic")
```

### Disable automatic follow-mode

By default, fyure.el automatically enables `helm-follow-mode` (`anything-follow-mode`). If you don't like this behavior, set `nil` to `fyure:follow-mode` as follows.

```lisp
(setq fyure:follow-mode nil)
```
