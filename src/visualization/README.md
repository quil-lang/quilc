# CL-QUIL visualization tools

## System Requirements

You will need `pdflatex` and `pdf2svg` in your path. On macOS, `pdflatex` is part of the `mactex` distribution. Both of these may be installed with homebrew:
```
brew install mactex
brew install pdf2svg
```

These executables are referred to in `cl-quil.visualization` by `*pdflatex-exe*` and `*pdf2svg-exe`, should you wish to override them.