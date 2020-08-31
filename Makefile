slides.pdf: slides.tex figure-candidates.tex figure-baselines.tex
	pdflatex slides
figure-baselines.tex: figure-baselines.R
	R --vanilla < $<
figure-candidates.tex: figure-candidates.R
	R --vanilla < $<
figures.pdf: figures.tex figure-signal-cost.tex figure-label-errors.pdf figure-timings.tex
	pdflatex figures
figure-signal-cost.tex: figure-signal-cost.R
	R --vanilla < $<
figure-timings.tex: figure-timings.R
	R --vanilla < $<
figure-label-errors.pdf: figure-label-errors.R
	R --vanilla < $<
figure-cv.pdf: figure-cv.R
	R --vanilla < $<
figure-cv-BIC.pdf: figure-cv-BIC.R
	R --vanilla < $<
figure-sequence-cv.pdf: figure-sequence-cv.R
	R --vanilla < $<
