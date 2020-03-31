file <- commandArgs(trailingOnly = TRUE)[[1]]

print(file)

my_moon_reader <- function(...) xaringan::moon_reader(..., css = c('../css/uwmadison.css', 'default-fonts', '../css/extra-classes.css'),
                                                      lib_dir = 'libs',
                                                      nature = list(
                                                        titleSlideClass = c('center', 'top', '.title-slide'),
                                                        highlightStyle = 'github',
                                                        highlightLines = TRUE,
                                                        countIncrementalSlides = FALSE,
                                                        ratio = '16:10',
                                                        navigation = list(scroll = FALSE)
                                                      ),
                                                      self_contained = TRUE)

rmarkdown::render(file, "my_moon_reader")
pagedown::chrome_print(stringr::str_replace(file, ".Rmd", ".html"), timeout = 1200)
rmarkdown::render(file)