source("renv/activate.R")
if(interactive()){
  library(devtools)
  library(docthis)
  library(codetools)
  library(usethis)
  doc_function = function(fname){
    cat(docthis:::doc_this(fname))
  }
  find_globals = function(fname){
    codetools::findGlobals(main, merge = FALSE)$variables
  }
}