index.html: index.Rmd */*_slides.html
	Rscript -e 'rmarkdown::render("index.Rmd")'

*/%_slides.html: */%_slides.Rmd
	cd $(@D); R CMD BATCH '--args $(<F)' ../create_slides.Rmd create_slides.Rout
#cp ../Lectures/lecture14/lec14_slides.Rmd lecture14/lec14_slides.Rmd; R CMD BATCH '--args lecture14/lec14_slides.html' create_slides.Rmd
