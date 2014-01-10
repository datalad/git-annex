megaannex
=========

Hook program for gitannex to use mega.co.nz as backend

# Requirements:

    python2
    requests>=0.10
    pycrypto

Credit for the mega api interface goes to: https://github.com/richardasaurus/mega.py 

# Install
Clone the git repository in your home folder.

    git clone git://github.com/TobiasTheViking/megaannex.git 

This should make a ~/megaannex folder

# Setup
Make the file executable, and link it into PATH

    cd ~/megaannex; chmod +x git-annex-remote-mega; sudo ln -sf `pwd`/git-annex-remote-mega /usr/local/bin/git-annex-remote-mega

# Commands for gitannex:

    USERNAME="user" PASSWORD="pword" git annex initremote mega type=external externaltype=mega encryption=shared folder=gitannex
    git annex describe mega "the mega.co.nz library"
