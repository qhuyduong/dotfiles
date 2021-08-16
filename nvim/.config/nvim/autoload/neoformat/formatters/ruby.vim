function! neoformat#formatters#ruby#enabled() abort
   return ['rubocopdaemon']
endfunction

function! neoformat#formatters#ruby#rubocopdaemon() abort
     return {
        \ 'exe': 'rubocop-daemon',
        \ 'args': ['exec', '--', '--auto-correct', '--stdin', '"%:p"', '2>/dev/null', '|', 'sed "1,/^====================$/d"'],
        \ 'stdin': 1,
        \ 'stderr': 1
        \ }
endfunction
