pdf: 
	lhs2TeX --math -o paper.tex paper.lhs
	latex paper
	bibtex paper
	latex paper
	pdflatex -halt-on-error paper.tex

rtf:	
	latex2rtf paper

cover:  
	pdflatex cover_letter
	latex2rtf cover_letter

suppl:
	lhs2TeX --math -o supplement.tex supplement.lhs
	pdflatex supplement
