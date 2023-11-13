#!/bin/bash
# Script for updating local clone of datalad/git-annex repository, updating the
# testannex Conda environment if necessary, and running testannex.py within the
# testannex environment.

__conda_setup="$("$HOME/miniconda3/bin/conda" 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
        . "$HOME/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="$HOME/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup

set -ex
git -c pull.rebase=false pull origin master
conda install -n testannex --satisfied-skip-solve --file spec-file.txt --yes --quiet
conda activate testannex
python testannex.py "$@"
