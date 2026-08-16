# elhints

Provide inline hints for Emacs Lisp buffers.


## Installation

1. Clone this repo and add the project directory to your Emacs `load-path`.
2. Install tree-sitter-elisp grammar (https://github.com/Wilfred/tree-sitter-elisp).
3. Update `elhints-elisp-grammar-source-dir` to point to the `tree-sitter-elisp` local project directory.

Example:
```cl
(setopt elhints-elisp-grammar-source-dir "/home/shermpay/Projects/tree-sitter-elisp")
```

## Usage

elhints defines `elhints-mode` as a minor mode that provides hints.

```cl
(require 'elhints)
(add-hook 'emacs-lisp-mode-hook 'elhints-mode)
```

## Example

![example of hints](./emacs_elhints_example.png?raw=true "elhints example")
