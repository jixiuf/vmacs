" -*- coding:utf-8-*-
" An example for a vimrc file.
"
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last change:	2008 Jul 02
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"	      for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"	    for OpenVMS:  sys$login:.vimrc

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file
endif
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif
"设定搜索是的高亮显示  
set hlsearch  
" 不要使用 vi 的键盘模式，而是 vim 自己的  
set nocompatible  
" history 文件中需要记录的行数  
set history=100  
" 在处理未保存或只读文件的时候，弹出确认  
set confirm  
" 侦测文件类型  
filetype on  
" 载入文件类型插件  
filetype plugin on  
" 为特定文件类型载入相关缩进文件  
filetype indent on  
" 带有如下符号的单词不要被换行分割  
set iskeyword+=_,$,@,%,#,-  
" 语法高亮  
syntax on  

" 启动的时候不显示那个援助索马里儿童的提示  
set shortmess=atI  
set noerrorbells  
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""  
" 高亮显示匹配的括号  
set showmatch  
" 匹配括号高亮的时间（单位是十分之一秒）  
set matchtime=5  
" 在搜索的时候不忽略大小写  
set noignorecase  
" 不要高亮被搜索的句子（phrases）  
"set nohlsearch  
" 在搜索时，输入的词句的逐字符高亮（类似 firefox 的搜索）  
set incsearch  
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""  
" 自动格式化  
set formatoptions=tcrqn  
" 继承前一行的缩进方式，特别适用于多行注释  
 set autoindent  
 " 为 C 程序提供自动缩进  
 set smartindent  
 " 使用 C 样式的缩进  
 set cindent  
 " 制表符为 4  
 set tabstop=4  
 " 统一缩进为 4  
 set softtabstop=4  
 set shiftwidth=4  
 " 不要用空格代替制表符  
 set noexpandtab  
 set nowrap  
 set smarttab  
 nnoremap <space> @=((foldclosed(line('.')) < 0) ? 'zc' : 'zo')<CR>  
 let OmniCpp_DisplayMode = 1  
"===================================括号自动关闭========================
function! My_BracketComplete()
    let char = strpart(getline('.'), col('.')-1, 1)
    if (char == ")")
        return "\<Right>"
    else
        return ")"
    endif
endfunction
autocmd Filetype java,javascript,html imap ( ()<left>
"autocmd FileType java,javascript,html inoremap ) <C-R>=My_BracketComplete()<CR>

function! My_MidComplete()
    let char = strpart(getline('.'), col('.')-1, 1)
    if (char == "]")
        return "\<Right>"
    else
        return "]"
    endif
endfunction
autocmd Filetype java,javascript,html imap  [ []<left>
autocmd FileType java,javascript,html inoremap ] <C-R>=My_MidComplete()<CR>

autocmd Filetype java,javascript,html,css imap { {<esc>xA<esc>pa}<left><cr><esc>ko
function! My_BraceComplete()
    let char = strpart(getline('.'), col('.')-1, 1)
    if (char == "}")
        return "\<Right>"
    else
        return "}"
    endif
endfunction

function! My_appendSemicolon() "在句末添加分号后 ，光标仍回原位置 imap  ; <C-R>=My_appendSemicolon()<CR>
	let nowPos=col('.') "光标下标
	let endPos=col('$') "行尾下标
	let len=endPos-nowPos
	let line=getline('.')
	if matchend(line,";\\s\*$")==strlen(line) " 如果 此行以分号结尾（包括分号后面有空格的情况） 则不将分号加到末尾,而是光标处
		return ";" 
	else
		return repeat("\<Right>",len).";".repeat("\<Left>",len+1) "在行尾添加分号并回到原位置
	endif
endfunction 
"============================end of 括号自动关闭========================
"与 omni 结合使用的时候 当提示方法名的时个一般是这种情形 System.out.print( 
"而不是 System.out.println() 注意最后的括号，此函数要做的就是当使用提示的时候
"补上右括号
function! My_BracketComplete4omni()
	let line=getline('.')  "|  example: line=  System.ouout
	let  dotPos=strridx(line,".") "lengthOf('  System.')-1 最后一个点.所在的位置
	let cursePos=strlen(line)    "lengthOf('  System.ouout') 光标的位置
	let len=cursePos-dotPos   "lengthof(ouout)+1 光标到最后一个点之间 的长度
	let lastCharIndex=strridx(line,'(') "得到最后一个左括号的下标，判断是否需要补上一个右括号
	let bedot=strpart(line,0,dotPos) "  System  最后一个点之前的部分
	let afdot=strpart(line,dotPos+1,len) "ouout 最后一个点之后 的部分
	let b=match(afdot,'\(\w\+\)\1')   " the begin index of   ouou
	let e=matchend(afdot,'\(\w\+\)\1') " the end index of ouou 
	let ok=strpart(afdot,(e-b)/2)       " out    ,all the char after the first 'u' of ouout  
	"debug
	"return repeat("\<BS>",len-1).ok."\nline:".line."\ndotPos:".dotPos."\ncursePos:".cursePos."\nlen:".len."\nlastCharIndex:".lastCharIndex."\nbedot:".bedot."\nafdot:".afdot."\nb:".b."\ne:".e."\nok:".ok."\nrep:".rep
	let rep=repeat("\<Left>",strlen(ok)-1) "向前移动 strlen(ok) 个长度的位置，以便删除 ouout 最前面出现的重复的 ou
	let lenOfOk=strlen(ok)
	let lenOfBetweenDotAndOk=len-lenOfOk
	let rep=rep.repeat("\<BS>", lenOfBetweenDotAndOk-1) "删除 ouout 最前面出现的重复的 ou
	let rep=rep.repeat("\<Right>",lenOfOk) "光标向后移动到最初的位置
	if  lastCharIndex == -1
		if dotPos== -1
			return " "
		else
			return rep
		endif  
	else 
		return rep.")\<Left>"
	endif
endfunction

let g:closetag_html_style=1 
" 下一行不用改了，因为 在 closetag.vim 里设置了
"autocmd FileType xml,html,jsp imap  > ><C-_>
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete
autocmd Filetype java set omnifunc=javacomplete#Complete
autocmd Filetype java set completefunc=javacomplete#CompleteParamsInf

autocmd FileType java,javascript,html,css imap  ; <C-R>=My_appendSemicolon()<CR>
autocmd FileType java,javascript,,jsp,html,css map  ; i;<esc>
autocmd FileType java,javascript,html,jsp imap  "  "<cr><esc>kA<esc>xa<esc>ppJhi
autocmd FileType java,javascript,html,vim,jsp imap  '  '<cr><esc>kA<esc>xa<esc>ppJi
autocmd Filetype java,javascript,jsp inoremap <buffer>  .  .<C-X><C-O><C-P>
autocmd Filetype css inoremap <buffer>  :  :<C-X><C-O><C-P>
"tab 键高亮下一个条目
autocmd Filetype css,javascript,java inoremap <buffer>  <tab>  <C-N>
"以下三行，效果相同，将第一个条目上屏
autocmd Filetype java,javascript,css,html inoremap <buffer>  <F1>   <C-O><C-R>=My_BracketComplete4omni()<CR>
autocmd Filetype java,javascript,css,html, inoremap <buffer>  <F2>   <C-O><C-R>=My_BracketComplete4omni()<CR>
autocmd Filetype java,javascript,css,html inoremap <buffer>  <F3>   <C-O><C-R>=My_BracketComplete4omni()<CR>

" add jquery.vim to syntax dir
au BufRead,BufNewFile *.js set ft=javascript.jquery
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  a  a<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  b  b<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  c  c<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  d  d<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  e  e<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  f  f<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  g  g<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  h  h<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  i  i<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  j  j<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  k  k<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  l  l<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  m  m<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  n  n<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  o  o<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  p  p<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  q  q<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  r  r<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  s  s<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  t  t<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  u  u<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  v  v<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  w  w<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  x  x<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  y  y<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  z  z<C-N><C-P>

autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  A  A<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  B  B<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  C  C<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  D  D<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  E  E<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  F  F<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  G  G<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  H  H<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  I  I<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  J  J<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  K  K<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  L  L<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  M  M<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  N  N<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  O  O<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  P  P<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  Q  Q<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  R  R<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  S  S<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  T  T<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  U  U<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  V  V<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  W  W<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  X  X<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  Y  Y<C-N><C-P>
autocmd Filetype java,javascript,css,html,xml inoremap <buffer>  Z  Z<C-N><C-P>

"设定窗口位置及大小
"winpos 355 35
"set lines=35 columns=98 



set number


set backup
set backupcopy=yes
set backupdir=~/.cache/.vimbackup

"set guioptions-=m
"set guioptions-=T

set wrap
set sidescroll=10

set fileencoding=utf-8
set enc=utf-8
" 设置文件编码
set fenc=utf-8
" 设置文件编码检测类型及支持格式
set fencs=utf-8,gb18030,gbk,gb2312,cp936
 

" https://vim.fandom.com/wiki/Change_cursor_shape_in_different_modes
" Cursor settings:
"  1 -> blinking block
"  2 -> solid block 
"  3 -> blinking underscore
"  4 -> solid underscore
"  5 -> blinking vertical bar
"  6 -> solid vertical bar
let &t_SI="\e[5 q"
let &t_SR="\e[3 q"
let &t_EI="\e[1 q"
