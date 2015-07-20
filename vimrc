" LeanMC's .vimrc!
" I'll try to be as clear, concise, and cogent as I can

" Set tab and indent to four spaces, make tab keypress insert space characters
set tabstop=4 shiftwidth=4 expandtab

" Attempt to detect type of file based on name and contents
filetype indent plugin on

" Enable syntax highlighting and the excellent solarized colorscheme
syntax enable
set background=dark
colorscheme solarized

" Allow switching from unsaved buffers, disabled for now in favor of split/vsplit
" set hidden

" Better command-line completion
set wildmenu

" Show partial commands in last line of screen
set showcmd

" Highlight searches and search as you type
set hlsearch incsearch

" Make searches ignore case, except when capital letters are used
set ignorecase smartcase

" Allow backspacing over autoindent, line breaks, and start of insert action
set backspace=indent,eol,start

" Autoindent
set autoindent

" Smart indent
set smartindent

" Don't always go to the first non-blank (keep current column)
set nostartofline

" Show cursor position on status line
set ruler

" Always show a status line
set laststatus=2

" Instead of silently failing because of unsaved changes, ask to save changes
set confirm

" Show instead of tell when something bell-provocative is done
set visualbell

" Always allow mouse usage
set mouse=a

" Set command window to 2 lines, so you don't have to press <Enter> so much
set cmdheight=2

" Line numbers!
set number

" Pasting can be a pain in the ass, but so is setting and unsetting paste.
" This makes that hassle a simple press of F11
set pastetoggle=<F11>

" Highlight 80th column, to avoid excessively long lines of code and whatnot
set colorcolumn=80

" But let me go past 80 if I want to
set textwidth=0

" Highlight current line with a dark gray background
set cursorline
