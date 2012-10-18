This repository contains my attempt to create site using [Haskell](http://haskell.org/) 
and [Snap framework](http://snapframework.com/).

I'm novice to Haskell so use code at your own risk.

PREREQUISITES
=============

1. Haskell Platform [http://www.haskell.org/platform/](http://www.haskell.org/platform/). I'm using 2012.1.0.0 from Ubuntu 12.04 repositoty.
2. Node.js and less compiler [http://lesscss.org/](http://lesscss.org/)
3. Closure linter [https://developers.google.com/closure/utilities/docs/linter_howto](https://developers.google.com/closure/utilities/docs/linter_howto)
4. Java and Python for Google Closure Compiler.

BUILD PROJECT
=============


To build project you need a few simple steps:

1. Checkout repository
2. Setup cabal if you didn't do that earlier: `cabal update` and `cabal install cabal-install`
3. Install project dependencies `cabal install --only-dependencies`. This step may require installation of dev-versions
of some libraries (libcurl3-dev, etc.)
4. In project root run `cabal configure` and `cabal build`
5. The build all required js and css files by running `./build_debug.sh`
6. Start server `dist/build/haskell-blog/haskell-blog`

That's all.

In development mode Snap framework dynamically rebuild source so all you
have to do is refresh page in browser. And Google Closure Library doesn't
require building in development mode.
Using uncompressed less files is not yet supported. You have to rebuild them
manually by running `./build_styles.js`

Please let me know if I've missed something.

BUILD FOR PRODUCTION
====================

1. Checkout production branch.
2. Comment out `rcp` command in `./build.sh`
3. Configure variables (see below)
4. Run `./build.sh`

You'll find haskell-blog.tar.lzma in project root.
It will contain all required files for running project.

CONFIGURING
===========

There is `src/Config.hs` which contains configuration of your environment.

1. `adminLogin`, `adminPassword` --- credentials for accessing vault.
2. `rackspaceAuthKey`, `rackspaceAuthUser` --- you don't plan to use Rackspace CloudFiles you can skip it
3. `disqusApiKey` --- disqus key for syncing comments
