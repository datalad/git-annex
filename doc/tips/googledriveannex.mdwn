googledriveannex
=========

Hook program for gitannex to use Google Drive as backend

# Requirements:

    python2
    python-httplib2

Credit for the googledrive api interface goes to google

# Install
Clone the git repository in your home folder.

    git clone git://github.com/TobiasTheViking/googledriveannex.git 

This should make a ~/googledriveannex folder

# Setup
Make the file executable, and link it into PATH

    cd ~/googledriveannex; chmod +x git-annex-remote-googledrive; sudo ln -sf `pwd`/git-annex-remote-googledrive /usr/local/bin/git-annex-remote-googledrive

# Commands for gitannex:

    git annex initremote googledrive type=external externaltype=googledrive encryption=shared folder=gitannex

An oauth authentication link should now be launched in the default browser. Authenticate and you will be proved with a code.

    OAUTH='authentication code' git annex initremote googledrive type=external externaltype=googledrive encryption=shared folder=gitannex
    git annex describe googledrive "the googledrive library"
