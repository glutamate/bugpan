pdf: 
	lhs2TeX --math -o paper.tex paper.lhs
	latex paper
	bibtex paper
	latex paper
	pdflatex -halt-on-error paper.tex
	latex2rtf paper
