[![Build Status](https://travis-ci.com/smelc/zzbot.svg?branch=master)](https://travis-ci.com/smelc/zzbot)

# zzbot

Documentation: [index.md](https://github.com/smelc/zzbot/blob/master/doc/src/index.md)

Example input file: [config.xml](https://github.com/smelc/zzbot/blob/master/configs/config.xml)

Useful commands for devs:

* `stack build` build the `zzbot` executable
* `stack exec zzbot -- configs/config.xml` executes the builders listed in `configs/config.xml`
* `stack test` executes tests in `test/TestConfig.hs`
* `stack exec haddock -- --html $(find src -name \*.hs) -o html` generates the documentation of sources in `src/`
