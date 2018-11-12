# Use git-annex's built-in bash completion
# This is the same code output by git-annex --bash-completion-script git-annex
# This covers all commands, all options, and will never go out of date!
_git-annex()
{
    local CMDLINE
    local IFS=$'\n'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(git-annex "${CMDLINE[@]}") )
}

complete -o bashdefault -o default -o filenames -F _git-annex git-annex

# Called by git's bash completion script when completing "git annex"
# Translate to the "git-annex" completion above.
_git_annex() {
    local CMDLINE
    CMDLINE=(--bash-completion-index $(($COMP_CWORD - 1)))

    local seen_git
    local seen_annex
    for arg in ${COMP_WORDS[@]}; do
        if [ "$arg" = git ] && [ -z "$seen_git" ]; then
		seen_git=1
		CMDLINE=(${CMDLINE[@]} --bash-completion-word git-annex)
	elif [ "$arg" = annex ] && [ -z "$seen_annex" ]; then
		seen_annex=1
	else
		CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
	fi
    done

    # This is the same as __gitcomp_file_direct in git-completion.bash;
    local IFS=$'\n'
    COMPREPLY=( $(git-annex "${CMDLINE[@]}") )
    compopt -o filenames +o nospace ||
    compgen -f /non-existing-dir/ >/dev/null ||
    true
}
