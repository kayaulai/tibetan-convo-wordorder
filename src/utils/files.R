createDirIfNone = function(dir){
  if (!dir.exists(dir)){
    dir.create(dir)
  }
}

getCurrDocNames = function(){
  readr::read_lines(here::here("data", "curr_texts.txt"))
}