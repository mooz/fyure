# fyure

An interface to fix Japanese hyoki-yure for Emacs

## Requirements

- MeCab (http://mecab.googlecode.com/svn/trunk/mecab/doc/index.html)
  - Dictionary: ipadic *utf-8*
- Python
  - python-mecab
- Emacs
  - helm.el (https://github.com/emacs-helm/helm) or anything.el (http://www.emacswiki.org/Anything)

## Installation

### Ubuntu

First, install required packages via `apt`.

    $ sudo apt-get install mecab mecab-ipadic-utf8 python-mecab

If your Emacs don't have either `helm.el` nor `anything.el`, install one of them (instruction is omitted).

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

In your Emacs, type `M-x fyure:start-fixing` and start fixing. Fixing consists of the following three steps.

1. Select a target *word-group*.
2. Select representative *word* for the selected *word-group*.
3. Replace words in *word-group* with the word selected in *step 2.* in `query-replace` fashion.
