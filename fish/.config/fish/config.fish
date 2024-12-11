if status is-interactive
    jump shell fish | source
    pyenv init - | source

    if not set -q TMUX; and [ "$TERM_PROGRAM" != "vscode" ]
        tmux new-session -A -s main
    end
end
