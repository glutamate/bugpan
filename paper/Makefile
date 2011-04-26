pdf: 
	lhs2TeX --math -o paper1.tex paper.lhs
	tex2one paper1 >paper2.tex
	hsbibtex paper2.tex paper.bib >paper.tex
	pdflatex -halt-on-error paper.tex

pdfold: 
	lhs2TeX --math -o paper.tex paper.lhs
	latex -halt-on-error paper
	bibtex paper
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
	tex2one paper >paper_cuts.tex
	latexdiff paper_nocuts.tex paper_cuts.tex >paper_diff.tex
	latex paper_diff
	bibtex paper_diff
	latex paper_diff
	pdflatex paper_diff.tex

nocuts:	
	~/tnutils/Tex2One paper >paper_nocuts.tex

template:
	buganal Template
	patch Template.lhs < template_patch
	lhs2TeX --poly -o Template.tex Template.lhs
	pdflatex Template

template1:
	lhs2TeX --poly -o Template.tex Template.lhs
	pdflatex Template