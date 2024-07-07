if status is-interactive
    jump shell fish | source
    pyenv init - | source

    if not set -q TMUX; and [ "$TERM_PROGRAM" != "vscode" ]
        tmux new-session -A -s main
    end
end
set -gx DISPLAY (ip route list default | awk '{print $3}'):0.0 # GWSL
set -gx PULSE_SERVER tcp:(ip route list default | awk '{print $3}') # GWSL
set -gx LIBGL_ALWAYS_INDIRECT 1 # GWSL
