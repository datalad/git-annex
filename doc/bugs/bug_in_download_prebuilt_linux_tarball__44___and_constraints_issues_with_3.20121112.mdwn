What steps will reproduce the problem?

First issue:
* The prebuilt tarball doesn't synchronize properly.

Reproduce:
* Download and untar tarball
* Start gitannex webapp on multiple computers(i had three in use)
* Synchronize with xmpp and an ssh server backend.
* The tarball versions don't push/get from ssh server backend.

Second issue:
* I can't install git-annex on ubuntu 12.10(Works fine in debian unstable)
* http://hpaste.org/77684

Reproduce:
* cabal update
* cabal install --only-dependencies
* cabal configure
* cabal build
* cabal install --bindir=$HOME/bin

With these constraints the cabal install can work:
* cabal install --only-dependencies ./ --constraint=certificate==1.2.2 --constraint=crypto-pubkey-types==0.1.1

What is the expected output? What do you see instead?
* Tarball version doesn't push to ssh backend.
* cabal install git-annex gives http://hpaste.org/77684

What version of git-annex are you using? On what operating system?
* git-annex 3.20121112 on debian unstale (working)
* ubuntu 12.10(failing)

Please provide any additional information below.

With these constraints the cabal install can work:
* cabal install --only-dependencies ./ --constraint=certificate==1.2.2 --constraint=crypto-pubkey-types==0.1.1

NOTE:
I couldn't get the markdown to cooperate, so using pl pagetype.

> I suspect this is [[done]]..
> 
> I fixed some bugs in the prebuilt tarball in the past 2 days that prevented it
> from transferring files.
