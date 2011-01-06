pdf: 
	lhs2TeX --math -o paper.tex paper.lhs
	latex -halt-on-error paper
	#bibtex paper
	latex -halt-on-error paper
	pdflatex -halt-on-error paper.tex

rtf:	
	latex2rtf paper

cover:  
	pdflatex cover_letter
	latex2rtf cover_letter

suppl:
	lhs2TeX --math -o supplement.tex supplement.lhs
	pdflatex supplement

diff:
	~/tnutils/Tex2One paper >paper_cuts.tex
	latexdiff paper_nocuts.tex paper_cuts.tex >paper_diff.tex
	pdflatex paper_diff.tex