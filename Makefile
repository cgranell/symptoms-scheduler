foo.html : foo.Rmd
    Rscript -e 'rmarkdown::render("foo.Rmd")'