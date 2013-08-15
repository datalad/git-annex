owncloudannex
=========

Hook program for gitannex to use owncloud as backend

# Requirements:

    python2

Credit for the Owncloud api interface goes to me.

# Install
Clone the git repository in your home folder.

    git clone git://github.com/TobiasTheViking/owncloudannex.git 

This should make a ~/owncloudannex folder

# Setup
Run the program once to set it up.

    cd ~/owncloudannex; python2 owncloudannex.py

# Commands for gitannex:

    git config annex.owncloud-hook '/usr/bin/python2 ~/owncloudannex/owncloudannex.py'
    git annex initremote owncloud type=hook hooktype=owncloud encryption=shared
    git annex describe owncloud "the owncloud library"
