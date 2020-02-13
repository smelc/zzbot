[![Build Status](https://travis-ci.com/smelc/zzbot.svg?branch=master)](https://travis-ci.com/smelc/zzbot)

# zzbot

zzbot is a tool to execute shell commands in a flexible manner and keep track of the history of past executions.

[zzbot example output](https://i.imgur.com/QNkRVQa.png)

Documentation: [index.md](https://github.com/smelc/zzbot/blob/master/doc/src/index.md)

Example input file: [config.xml](https://github.com/smelc/zzbot/blob/master/configs/examples/kcg.xml)

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
