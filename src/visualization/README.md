# CL-QUIL visualization tools

## System Requirements

You will need `pdflatex` and `pdf2svg` in your path. On macOS, `pdflatex` is part of the `mactex` distribution. Both of these may be installed with homebrew:
```
brew install mactex
brew install pdf2svg
```

These executables are referred to in `cl-quil.visualization` by `*pdflatex-exe*` and `*pdf2svg-exe`, should you wish to override them.

To use this in a Jupyter notebook, you should use [common-lisp-jupyter](https://github.com/yitzchak/common-lisp-jupyter). As an installation note, some users have found that on macOS the default SBCL path associated with the jupyter kernel (e.g. in `~/Library/Jupyter/kernels/common-lisp/kernel.json`) is incorrect; this will result in `jupyter` being unable to start the Common Lisp kernel.