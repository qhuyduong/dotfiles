if status is-interactive
    jump shell fish | source
    pyenv init - | source

    if not set -q TMUX; and [ "$TERM_PROGRAM" != "vscode" ]
        tmux new-session -A -s main
    end
end
set -gx DISPLAY (cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0 #GWSL
set -gx PULSE_SERVER tcp:(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}') #GWSL
set -gx LIBGL_ALWAYS_INDIRECT 1 #GWSL
