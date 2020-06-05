figures.pdf: figures.tex figure-signal-cost.tex figure-label-errors.pdf figure-timings.tex
	pdflatex figures
figure-signal-cost.tex: figure-signal-cost.R
	R --vanilla < $<
figure-timings.tex: figure-timings.R
	R --vanilla < $<
figure-label-errors.pdf: figure-label-errors.R
	R --vanilla < $<
