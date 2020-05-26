[![Melpa Status](http://melpa.org/packages/envrc-badge.svg)](https://melpa.org/#/envrc)
[![Melpa Stable Status](http://stable.melpa.org/packages/envrc-badge.svg)](http://stable.melpa.org/#/envrc)
[![Build Status](https://github.com/purcell/envrc/workflows/CI/badge.svg)](https://github.com/purcell/envrc/actions)
<a href="https://www.patreon.com/sanityinc"><img alt="Support me" src="https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg"></a>

# envrc.el

A GNU Emacs library which uses [`direnv`](https://direnv.net/) to set
environment variables on a per-buffer basis. This means that when you
work across multiple projects which have `.envrc` files, all processes
launched from the buffers "in" those projects will be executed with
the environment variables specified in those files. This allows
different versions of linters and other tools to be installed in each
project if desired.

## Design notes

There is already [an excellent `direnv.el`
package](https://github.com/wbolster/emacs-direnv) available, which
works in a certain way that has particular trade-offs. I wanted to
explore a different approach. *The existing `direnv` package has many
happy users, and I make no claim that this one is better: it is just
different. Additionally, the `envrc.el` package is at an earlier stage
of development, and likely has bugs.*

By default, Emacs has a single global set of environment variables
used for all subprocesses, stored in the `process-environment`
variable. `direnv.el` switches that global environment using values
from `direnv` when the user performs certain actions, such as
switching between buffers in different projects.

In practice, this is simple and mostly works very well. But some
trade-offs of this simple approach are:

* When switching to a buffer that is not "inside" a project with an
  `.envrc` file, the buffer will see the last project's environment. I
  would prefer it to see the default Emacs environment.

* When `direnv` fails to execute in the course of switching to a
  buffer in a new project with an `.envrc` file (e.g. because that
  `.envrc` file is disallowed), buffers in the new project will see the
  environment variables from the previous project.

* Background buffers from a previous project will start seeing the new
  project's environment, so any processes they launch asynchronously
  after the switch will use the wrong environment. (This is probably
  quite rare in practice.)

Now, it is also possible to set `process-environment` locally in a
buffer. If this value could be correctly maintained in all buffers
based their various respective `.envrc` files, then buffers across
multiple projects could simultaneously be "connected" to the
environments of their corresponding project directories. I wrote
`envrc.el` to explore this approach.

`envrc.el` uses a global minor mode (`envrc-global-mode`) to hook into
practically every buffer created by Emacs, including hidden and
temporary ones. When a buffer is found to be "inside" an
`.envrc`-managed project, `process-environment` is set buffer-locally
by running `direnv`, the results of which are also cached indefinitely
so that this is not too costly overall. Each buffer has a local minor
mode (`envrc-mode`) with an indicator which displays whether or not a
direnv is in effect in that buffer. (Hooking into every buffer is
important, rather than just those with certain major modes, since
separate temporary, compilation and repl buffers are routinely used
for executing processes.)

This approach also has some trade-offs:

* Things like `*help*` buffers will have `envrc-mode` enabled based on
  the directory of the buffer which caused them to be created
  initially.

* There's a (very small) overhead every time a buffer is created.

* Updates are not automatic. `direnv.el` re-executes `direnv` when
  switching between buffers, whereas `envrc-mode` caches the
  environment until the user refreshes it explicitly with
  `envrc-reload`.

I hope that these will be worthwhile, since my ultimate goal is to
integrate with `lorri`, a daemon which re-builds Nix projects
automatically and presents their environments to `direnv`: if we can
hook into `lorri`'s rebuilds, then `envrc.el` will allow Emacs to
programmatically refresh the environment of specific subsets of
buffers, which is not currently possible with `direnv.el`.

It's also possible that there's a way to call `direnv` more
aggressively by allowing it to see values of `DIRENV_*` obtained
previously such that it becomes a no-op.

## Installation

Installable packages are available via MELPA: do
`M-x package-install RET envrc RET`.

Alternatively, [download][]
the latest release or clone the repository, and install
`envrc.el` with `M-x package-install-file`.

## Usage

Add the following to your `init.el` (after calling `package-initialize`):

```el
(envrc-global-mode)
```

*It's probably wise to do this late in your startup sequence* so that
this mode is enabled before other minor modes, e.g. `flycheck`, which
might look for the presence of particular executables.

You should only enable the mode if `direnv` is installed and available
in the default Emacs `exec-path`. (There is a local minor mode
`envrc-mode`, but you should not try to enable this
granularly, e.g. for certain modes or projects, because compilation and other buffers might not see 

Regarding interaction with the mode, see `envrc-mode-map`, and the
commands `envrc-reload`, `envrc-allow` and `envrc-deny`.

[download]: https://github.com/purcell/envrc/tags


<hr>


[💝 Support this project and my other Open Source work via Patreon](https://www.patreon.com/sanityinc)

[💼 LinkedIn profile](https://uk.linkedin.com/in/stevepurcell)

[✍ sanityinc.com](http://www.sanityinc.com/)

[🐦 @sanityinc](https://twitter.com/sanityinc)
