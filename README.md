[![Melpa Status](http://melpa.org/packages/flymake-flycheck-badge.svg)](https://melpa.org/#/flymake-flycheck)
[![Melpa Stable Status](http://stable.melpa.org/packages/flymake-flycheck-badge.svg)](http://stable.melpa.org/#/flymake-flycheck)
[![Build Status](https://github.com/purcell/flymake-flycheck/workflows/CI/badge.svg)](https://github.com/purcell/flymake-flycheck/actions)
<a href="https://www.patreon.com/sanityinc"><img alt="Support me" src="https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg"></a>

# flymake-flycheck.el: use any Emacs flycheck checker as a flymake backend

This package provides support for running any flycheck checker as a
flymake diagnostic backend. The effect is that flymake will control
when the checker runs, and flymake will receive its errors.

The core of the package is the ability to wrap a single checker into
a flymake diagnostic function which could be added to `flymake-diagnostic-functions`:

```el
(flymake-flycheck-diagnostic-function-for 'sh-shellcheck)
```

Flycheck has the convenient notion of "available" checkers, which is
determined at runtime according to mode and availability of necessary
tools, as well as config for explicitly "chained" checkers.

Accordingly, you can obtain the diagnostic functions for all checkers
that flycheck would enable in the current buffer like this:

```el
(flymake-flycheck-all-chained-diagnostic-functions)
```

In practical terms, **most users will want to simply enable all
available checkers** whenever `flymake-mode` is enabled:

```el
(add-hook 'flymake-mode-hook 'flymake-flycheck-auto)
```

If you find that `flymake` is now running `flycheck` checkers which
are redundant because there's already a `flymake` equivalent, simply
add those checkers to the `flycheck-disabled-checkers` variable, e.g.

```el
(add-to-list 'flycheck-disabled-checkers 'sh-shellcheck)
```

### Caveats

* Flycheck UI packages will have no idea of what the checkers are
  doing, because they are run without flycheck's coordination.
* Flycheck's notion of "chained checkers" is not handled
  automatically, so although multiple chained checkers can be used,
  they will all be executed simultaneously even if earlier checkers
  fail.  This could either be considered a feature, or lead to
  redundant confusing messages.

## Installation

Installable packages are available via MELPA: do
`M-x package-install RET flymake-flycheck RET`.

Alternatively, [download][]
the latest release or clone the repository, and install
`flymake-flycheck.el` with `M-x package-install-file`.

[download]: https://github.com/purcell/flymake-flycheck/tags

<hr>

[💝 Support this project and my other Open Source work via Patreon](https://www.patreon.com/sanityinc)

[💼 LinkedIn profile](https://uk.linkedin.com/in/stevepurcell)

[✍ sanityinc.com](http://www.sanityinc.com/)

[🐦 @sanityinc](https://twitter.com/sanityinc)
