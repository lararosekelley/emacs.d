module.exports = {
  extends: ["@commitlint/config-conventional"],
  rules: {
    "scope-empty": [2, "never"],
    "scope-enum": [
      2,
      "always",
      [
        // entrypoints & bootstrap
        "core",
        "config",
        "straight",
        // lsp / diagnostics / completion / syntax
        "lsp",
        "completion",
        "treesitter",
        // language & tooling support
        "dap",
        "db",
        "docker",
        "go",
        "python",
        "paredit",
        // ui surface
        "ui",
        "theme",
        "fonts",
        "modeline",
        "tabs",
        "dashboard",
        "files",
        "treemacs",
        "projectile",
        "terminal",
        // editing
        "evil",
        "keys",
        "history",
        "git",
        // writing & knowledge
        "org",
        "markdown",
        "novel",
        "ai",
        // repo meta
        "deps",
        "docs",
        "ci",
        "build",
        "tooling",
        "tests",
        "security",
      ],
    ],
  },
};
