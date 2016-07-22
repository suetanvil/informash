# Informash

Informash is a text generator using Markov Chains to generate new,
potentially amusing text from one or more input documents.

It is written in Haskell using GHC.

# Building

You will need:

* A not-too-old version of [GHC](https://www.haskell.org/ghc/).
* [Cabal](https://www.haskell.org/cabal) (usually included with GHC).
* A sufficiently Unix-ish environment containing GNU Make and Perl 5.

(That last isn't strictly necessary.  You can just run the necessary
commands by hand.  Perl is only needed to make the manual, so you can
live without that too.)

Assuming you wish to install **informash** in `/usr/local`, simply do
the following:

    make cabal-deps
    make
    sudo make install INSTDIR=/usr/local

# Documentation

See the included man page.  There's an online version
[here](https://github.com/suetanvil/informash/blob/master/informash.pod).

# Legal Stuff

`informash` is Copyright (C) 2016 Chris Reuter and is released under
the terms of the GNU General Public License version 2.

`informash` is distributed in the hope that it will be useful, but
**WITHOUT ANY WARRANTY**; without even the implied warranty of
**MERCHANTABILITY** or **FITNESS FOR A PARTICULAR PURPOSE**.  See the
GNU General Public License for more details.




