# Development
# -----------

# Install dependencies
install:
    npm install

# Testing
# -------

# Run all tests
test:
    emacs -Q --batch -L lisp -l ert $(printf -- '-l %s ' test/*-test.el) -f ert-run-tests-batch-and-exit

# Run a single test file
test-file file:
    emacs -Q --batch -L lisp -l ert -l {{ file }} -f ert-run-tests-batch-and-exit

# Checks
# ------

# Lint and test everything
check: lint test

# Lint Emacs Lisp and Markdown
lint: lint-el lint-md

lint-el:
    emacs -Q --batch -l scripts/checkdoc.el init.el early-init.el lisp/*.el scripts/*.el test/*.el

lint-md:
    npx markdownlint-cli2
    vale README.md
