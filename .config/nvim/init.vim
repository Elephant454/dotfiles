"Vundle configuration
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'chriskempson/base16-vim'
Plugin 'ervandew/supertab'
Plugin 'Raimondi/delimitMate'
Plugin 'Yggdroot/indentLine'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'tpope/vim-fugitive'
Plugin 'NLKNguyen/papercolor-theme'
"Plugin 'Valloric/YouCompleteMe'
Plugin 'Shougo/deoplete.nvim'
Plugin 'davidhalter/jedi-vim'
Plugin 'artur-shaik/vim-javacomplete2'
Plugin 'airblade/vim-rooter'

call vundle#end()
filetype plugin indent on

"user configuration
syntax on
colorscheme PaperColor 
set background=dark
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
"map <F9> :make<Return>
map <F9> :make<Return>
map <F10> :cprevious<Return>
map <F11> :cnext<Return>
"map <F12> :!java %:r<Return>
map <F12> :make run<Return>
