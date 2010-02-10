pdf: 
	lhs2TeX --poly -o paper.tex paper.lhs
	latex paper
	bibtex paper
	latex paper
	pdflatex -halt-on-error paper.tex

