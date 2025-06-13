if status is-interactive
    ~/.rbenv/bin/rbenv init - --no-rehash fish | source

    if not set -q TMUX; and [ "$TERM_PROGRAM" != "vscode" ]
        tmux new-session -A -s main
    end
end
