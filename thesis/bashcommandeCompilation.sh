#!/bin/bash
	rm *aux; rm *blg; rm *.bbl; rm *.lof; rm *lot; rm *.out; rm *.toc
	echo '\includeonly{}' > TheseRef.tex
	cat These.tex >> TheseRef.tex
	pdflatex TheseRef.tex
	bibtex TheseRef
	cat These.tex > TheseRef.tex
	pdflatex TheseRef.tex
	bibtex chapitre1
	bibtex chapitre2
	bibtex chapitre3
	bibtex chapitre4
	bibtex annexe1
	bibtex annexe2 # mettre autant d'annexe que nécessaire
	pdflatex TheseRef.tex
	pdflatex TheseRef.tex
	pdflatex TheseRef.tex
	pdflatex TheseRef.tex
	rm *aux; rm *blg; rm *.bbl; rm *.lof; rm *.log; rm *lot; rm *.out; rm *.toc; rm TheseRef.tex; rm TheseRef.log
