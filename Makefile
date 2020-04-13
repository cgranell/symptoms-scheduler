# src: https://monashbioinformaticsplatform.github.io/2017-11-16-open-science-training/topics/automation.html

HTML_FILES=foo.html 

all : $(HTML_FILES) 
	@echo All files are now up to date

clean : 
	rm -f $(HTML_FILES)
	
%.html : %.Rmd
	Rscript -e 'rmarkdown::render("$<")'