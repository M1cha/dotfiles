" Source: https://github.com/ctrlpvim/ctrlp.vim

let s:wig_cond = v:version > 702 || ( v:version == 702 && has('patch051') )
let s:maxdepth = 40

fu! s:glbpath(...)
	retu call('globpath', s:wig_cond ? a:000 : a:000[:1])
endf

if exists('*fnameescape')
	if exists('+ssl')
		fu! s:fnesc(path, type, ...)
			if a:type == 'c'
				let path = escape(a:path, '%#')
			elsei a:type == 'f'
				let path = fnameescape(a:path)
			elsei a:type == 'g'
				let path = escape(a:path, '?*')
			en
			let path = substitute(path, '[', '[[]', 'g')
			retu a:0 ? escape(path, a:1) : path
		endf
	el
		fu! s:fnesc(path, type, ...)
			let path = fnameescape(a:path)
			retu a:0 ? escape(path, a:1) : path
		endf
	en
el
	if exists('+ssl')
		fu! s:fnesc(path, type, ...)
			if a:type == 'c'
				let path = escape(a:path, '%#')
			elsei a:type == 'f'
				let path = escape(a:path, " \t\n%#*?|<\"")
			elsei a:type == 'g'
				let path = escape(a:path, '?*')
			en
			let path = substitute(path, '[', '[[]', 'g')
			retu a:0 ? escape(path, a:1) : path
		endf
	el
		fu! s:fnesc(path, type, ...)
			let path = escape(a:path, " \t\n*?[{`$\\%#'\"|!<")
			retu a:0 ? escape(path, a:1) : path
		endf
	en
en

fu! s:getparent(item)
	let parent = substitute(a:item, '[\/][^\/]\+[\/:]\?$', '', '')
	return parent
endf

function! s:findroot(curr, mark, depth)
	let [depth, fnd] = [a:depth + 1, 0]

	if type(a:mark) == 1
		let fnd = s:glbpath(s:fnesc(a:curr, 'g', ','), a:mark, 1) != ''
	elsei type(a:mark) == 3
		for markr in a:mark
			if s:glbpath(s:fnesc(a:curr, 'g', ','), markr, 1) != ''
				let fnd = 1
				break
			endif
		endfor
	endif

	if fnd
		return a:curr
	elseif depth > s:maxdepth
		return v:null
	else
		let parent = s:getparent(a:curr)
		if parent != a:curr
			return s:findroot(parent, a:mark, depth)
		endif
	endif

	return v:null
endfunction

function! SmartCWD()
    let crfpath = expand('%:p:h', 1)
    let markers = ['.git', '.hg', '.svn', '.bzr', '_darcs']

    let root = s:findroot(crfpath, markers, 0)
    if root isnot v:null | return root | endif

    return crfpath
endfunction
