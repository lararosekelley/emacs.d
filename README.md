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

## To-do list

- [ ] No previews on `consult-find`
- [ ] ESLint/Prettier setup for JS/TS LSP mode
- [ ] Shortcuts for Tramp/setup
- [ ] DBee equivalent?
- [ ] Flymake at-point diagnostics - show multiple per popup / less ugly highlighting
- [ ] Use better colors in goggles package
- [ ] centaur-tabs still feels a bit clunky - behavior for non-code file buffers is unintuitive
- [ ] Refine `evil-quit` behavior to be smart about window splits, tabs, centaur tabs, etc.
- [ ] Treemacs + Projectile - better auto-add project behavior?
- [ ] LSP mode - cycle through peek references
- [ ] LSP mode - peek docs on hover (right now uses leader command)
- [ ] Revisit all keybindings and be smarter about mode-specific ones (lots of global bindings at the moment)
