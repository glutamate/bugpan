pdf: 
	lhs2TeX --math -o paper1.tex paper.lhs
	~/tnutils/Tex2One paper1 >paper2.tex
	~/hsbibtex/hsbibtex paper2.tex paper.bib >paper.tex
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
	latex paper_diff
	bibtex paper
	latex paper_diff
	pdflatex paper_diff.tex

nocuts:	
	~/tnutils/Tex2One paper >paper_nocuts.tex