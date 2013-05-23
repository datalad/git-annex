googledriveannex
=========

Hook program for gitannex to use Google Drive as backend

# Requirements:

    python2

Credit for the googledrive api interface goes to google

## Install
Clone the git repository in your home folder.

    git clone git://github.com/TobiasTheViking/googledriveannex.git 

This should make a ~/googledriveannex folder

## Setup
Run the program once to make an empty config file

    cd ~/googledriveannex; python2 googledriveannex.py

## Commands for gitannex:

    git config annex.googledrive-hook '/usr/bin/python2 ~/googledriveannex/googledriveannex.py'
    git annex initremote googledrive type=hook hooktype=googledrive encryption=shared
    git annex describe googledrive "the googledrive library"
