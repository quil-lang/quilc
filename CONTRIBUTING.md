# Contributor Guidelines

We value and encourage contribution from the community. To reduce
friction in this process, we have collected some best-practices for
contributors:

* Testing. Before making a pull-request (PR), please make sure that
  you have added sufficient test coverage to your changes and that you
  have run the test suite. You can run tests by invoking `make test` in
  the project root. Code coverage statistics can be compiled by
  running `sbcl --load coverage-report.lisp` in the `coverage-report`
  directory.
* Documentation. Provide concise but clear documentation for your
  changes. In general, all public functions (those exported in
  `package.lisp`) *must* have docstrings. Internal functions should
  usually be documented though it is less important. Documentation
  should also be provided as comments to your code. In particular,
  non-obvious code should be accompanied by detailed explanation of
  its working.
* Pull Request. The typical workflow for contributing to an
  open-source project is
  1. Create a fork of the project.
  2. Create a branch for your work. This should be appropriately
  named, often with a descriptive prefix of `feature/` or `fix/`.
  3. Create a PR to the original project. The PR should have a concise
     title stating the intent of the PR, followed by a more detailed
     description of the proposed changes including arguments for those
     changes.
  4. Your code will be reviewed. You should participate in the review,
     making changes where suggested and pushing them to the PR branch.
  5. If all goes well, your code will be merged and you will be
     attributed in the ACKNOWLEDGEMENTS file.
* Style. In general, follow the [Google Common-Lisp style guide](https://google.github.io/styleguide/lispguide.xml). If
  there is an inconsistency between the style guide and neighboring
  code, follow the style of the neighboring code. Use code formatting
  (indentation) that is equivalent to that of GNU Emacs'
  `common-lisp-mode`.
* Be polite.
