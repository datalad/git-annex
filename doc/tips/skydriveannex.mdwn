skydriveannex
=========

Hook program for gitannex to use [skydrive](http://en.wikipedia.org/wiki/SkyDrive) (previously *Windows Live SkyDrive* and *Windows Live Folders*) as backend

# Requirements:

    python2
    python-yaml

Credit for the Skydrive api interface goes to https://github.com/mk-fg/python-skydrive

# Install
Clone the git repository in your home folder.

    git clone git://github.com/TobiasTheViking/skydriveannex.git 

This should make a ~/skydriveannex folder

# Setup
Make the file executable, and link it into PATH

    cd ~/skydriveannex; chmod +x git-annex-remote-skydrive; sudo ln -sf `pwd`/git-annex-remote-skydrive /usr/local/bin/git-annex-remote-skydrive

# Commands for gitannex:

    git annex initremote skydrive type=external externaltype=skydrive encryption=shared folder=gitannex
    
An oauth authentication link should now be launched in the default browser. Authenticate, and use the last url as OAUTH key.

    OAUTH='URL after last redirect' git annex initremote skydrive type=external externaltype=skydrive encryption=shared folder=gitannex
    git annex describe skydrive "the skydrive library"
