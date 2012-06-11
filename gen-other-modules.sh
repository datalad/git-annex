#!/bin/bash

# Generate module list for 'Other-Modules:' field in git-annex.cabal.
# This would be simpler if the code were under ./src.

find Annex Annex.hs \
     Backend Backend.hs \
     Build \
     Checks.hs \
     CmdLine.hs \
     Command Command.hs \
     Common Common.hs \
     Config.hs \
     Crypto.hs \
     Git Git.hs \
     GitAnnex.hs \
     GitAnnexShell.hs \
     Init.hs \
     Limit.hs \
     Locations.hs \
     Logs \
     Messages Messages.hs \
     Option.hs \
     Remote Remote.hs \
     Seek.hs \
     Setup.hs \
     Types Types.hs \
     Upgrade Upgrade.hs \
     Usage.hs \
     Utility \
     -name '*.hs' \
| sed -r -e 's!.hs!!' -e 's!/!.!g'
