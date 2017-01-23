call plug#begin('~/.vim/plugged')
Plug 'chriskempson/base16-vim'
Plug 'ervandew/supertab'
Plug 'Raimondi/delimitMate'
Plug 'Yggdroot/indentLine'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'tpope/vim-fugitive'
Plug 'NLKNguyen/papercolor-theme'
Plug 'Shougo/deoplete.nvim'
Plug 'davidhalter/jedi-vim'
Plug 'artur-shaik/vim-javacomplete2'
Plug 'airblade/vim-rooter'
Plug 'luochen1990/rainbow'
call plug#end()

"user configuration
syntax on
colorscheme PaperColor 
set background=light
hi Normal ctermbg=none
set laststatus=2 ruler
set smartindent expandtab tabstop=4 shiftwidth=4
retab

let g:deoplete#enable_at_startup=1

"Java Compiler Stuff
"javac
"autocmd Filetype java set makeprg=javac\ %
"set errorformat=%A%f:%l:\ %m,%Z%p^,%-C%.%#
"ant
autocmd BufRead *.java set efm=%A\ %#[javac]\ %f:%l:\ %m,%-Z\ %#[javac]\ %p^,%-C%.%#
autocmd BufRead set makeprg=ant\ -find\ build.xml

"Maps
map <F7> :tabp<Return>
map <F8> :tabn<Return>
map <F9> :make<Return>
map <F10> :cprevious<Return>
map <F11> :cnext<Return>
"map <F12> :!java %:r<Return>
map <F12> :make run<Return>
