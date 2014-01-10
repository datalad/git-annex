dropboxannex
=========

Hook program for gitannex to use dropbox as backend

# Requirements:

    python2
    python-pkg-resources

Credit for the Dropbox api interface goes to Dropbox.

# Install
Clone the git repository in your home folder.

    git clone git://github.com/TobiasTheViking/dropboxannex.git 

This should make a ~/dropboxannex folder

# Setup
Make the file executable, and link it into PATH

    cd ~/dropboxannex; chmod +x git-annex-remote-dropbox; sudo ln -sf `pwd`/git-annex-remote-dropbox /usr/local/bin/git-annex-remote-dropbox

# Commands for gitannex:

    git annex initremote dropbox type=external externaltype=dropbox encryption=shared folder=gitannex
    git annex describe dropbox "the dropbox library"

