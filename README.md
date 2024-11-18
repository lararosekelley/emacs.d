# emacs.d

> My personal Emacs configuration

---

## Overview

A fully-featured Emacs configuration for users more comfortable with Vim/Neovim.

## Dependencies

- Emacs 29+

## Setup

1. Clone this repo into `~/.emacs.d`
2. Launch Emacs
3. Run `nerd-icons-install-fonts` (only needed once)
4. Run `treesit-auto-install-all` to install Treesitter grammars

## Screenshots

![start screen](./assets/dashboard.png)

**Start screen with emacs-dashboard**

![flymake](./assets/flymake.png)

**Flymake at-point diagnostics**

![ripgrep](./assets/ripgrep.png)

**Consult + ripgrep with previews**

![peek references](./assets/lsp-peek.png)

**Peek references in lsp-mode**

## To-do list

- [ ] TODO No previews on `consult-find`
- [ ] TODO ESLint/Prettier setup for JS/TS LSP mode
- [ ] TODO Shortcuts for Tramp/setup
- [ ] TODO DBee equivalent?
- [ ] TODO Flymake at-point diagnostics - show multiple per popup / less ugly highlighting
- [ ] TODO Use better colors in goggles package
- [ ] TODO centaur-tabs still feels a bit clunky - behavior for non-code file buffers is unintuitive
- [ ] TODO Refine `evil-quit` behavior to be smart about window splits, tabs, centaur tabs, etc.
- [ ] TODO Treemacs + Projectile - better auto-add project behavior?
- [ ] TODO LSP mode - cycle through peek references
- [ ] TODO LSP mode - peek docs on hover (right now uses leader command)
- [ ] TODO Revisit all keybindings and be smarter about mode-specific ones (lots of global bindings at the moment)
