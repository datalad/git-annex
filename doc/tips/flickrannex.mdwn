flickrannex
=========

Hook program for gitannex to use flickr as backend

# Requirements:

    python2

Credit for the flickr api interface goes to: http://stuvel.eu/flickrapi
Credit for the png library goes to: https://github.com/drj11/pypng
Credit for the png tEXt patch goes to: https://code.google.com/p/pypng/issues/detail?id=65

# Install
Clone the git repository in your home folder.

    git clone git://github.com/TobiasTheViking/flickrannex.git 

This should make a ~/flickrannex folder

# Setup
Make the file executable, and link it into PATH

    cd ~/flickrannex; chmod +x git-annex-remote-flickr; sudo ln -sf `pwd`/git-annex-remote-flickr /usr/local/bin/git-annex-remote-flickr

# Commands for gitannex:

    USERNAME="username@provider.com" git annex initremote flickr type=external externaltype=flickr encryption=shared folder=gitannex

An oauth authentication link should now be launched in the default browser. The hook will wait for 30s for you to login and authenticate.

    git annex describe dropbox "the flickr library"

# Notes

## Unencrypted mode
The photo name on flickr is currently the GPGHMACSHA1 version.

## Encrypted mode
The current version base64 encodes all the data, which results in ~35% larger filesize.

## Including directories as tags
This feature is currently disabled, if it gets implemented again it will most likely not require user action to enable it.

In this case the image:
   /home/me/annex-photos/holidays/2013/Greenland/img001.jpg
would get the following tags:  "holidays" "2013" "Greenland"
(assuming "/home/me/annex-photos" is the top level in the annex...)

Caveat Emptor - Tags will *always* be NULL for indirect repos - we don't (easily) know the human-readable file name.
