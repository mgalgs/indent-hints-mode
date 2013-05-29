README.md: make-readme-markdown.el indent-hints.el
	emacs --script $< < indent-hints.el >$@ 2>/dev/null

make-readme-markdown.el:
	wget -q -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

.INTERMEDIATE: make-readme-markdown.el
