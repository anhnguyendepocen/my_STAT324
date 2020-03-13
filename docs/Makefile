update_web: index.html
	git add *; git commit -m "Update"; git push

index.html: index.Rmd */*_slides.html */*_slides.pdf
	Rscript -e 'rmarkdown::render("index.Rmd")'

*/%_slides.html: ../Lectures/*/%_slides.Rmd
	cp $< ./$(@D); cd $(@D); R CMD BATCH '--args $(<F)' ../create_slides.Rmd create_slides.Rout

*/%_slides.pdf: ../Lectures/*/%_slides.Rmd
	cp $< ./$(@D); cd $(@D); R CMD BATCH '--args $(<F)' ../create_slides.Rmd create_slides.Rout
