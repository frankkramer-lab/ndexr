##Regenerate documentation

require(roxygen2)
roxygenize(getwd(), roclets = c("namespace", "rd"))
