[![Build Status](https://travis-ci.com/smelc/zzbot.svg?branch=master)](https://travis-ci.com/smelc/zzbot)

# zzbot

zzbot is a tool to execute shell commands in a flexible manner and keep track of the history of past executions. Here's an example output:

![zzbot example output](https://i.imgur.com/QNkRVQa.png)

when zzbot executes this configuration: [kcg.xml](https://github.com/smelc/zzbot/blob/master/configs/examples/kcg.xml)

Documentation: [index.md](https://github.com/smelc/zzbot/blob/master/doc/src/index.md)

# Installation

```bash
git clone https://github.com/smelc/zzbot
cd zzbot
curl -sSL https://get.haskellstack.org/ | sh
stack build
stack install zzbot --local-bin-path INSTALL_PATH # INSTALL_PATH can for example be /usr/local/bin
```

# Development

Useful commands:

* `stack build` builds the `zzbot` executable
* `stack run zzbot configs/config.xml` executes the builders listed in `configs/config.xml`

Test and documentation commmands:

* `stack test` executes tests in `test/TestConfig.hs`
* `stack exec haddock -- --html $(find src -name \*.hs) -o html` generates the documentation of sources in `src/`

There's a commit pre-hook to execute CI before pushing, install it as follows:

```bash
cd .git/hooks
ln -s ../../hooks/pre-commit .
cd -
```

# Bazel

zzbot can be built using Tweag's
[rules_haskell](https://github.com/tweag/rules_haskell) extension
of [Bazel](https://bazel.build): `bazel build //...`
