if status is-interactive
    pyenv init - | source
    jump shell fish | source

    if not set -q TMUX; and [ "$TERM_PROGRAM" != "vscode" ]
        tmux new-session -A -s main
    end
end
