#When coding for this repo, run this file first.
library(devtools)
library(docthis)

doc_function = function(fname){
  cat(docthis:::doc_this(fname))
}