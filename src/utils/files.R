library(here)

createDirIfNone = function(dir){
  if (!dir.exists(dir)){
    dir.create(dir)
  }
}
