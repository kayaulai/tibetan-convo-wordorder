cleanTrailingTsheg = function(strings){
  case_when(str_ends(strings, "་") ~ substr(strings, 1, nchar(strings) - 1),
            T ~ strings)
}

cleanTrailingSpace = function(strings){
  case_when(str_ends(strings, " ") ~ substr(strings, 1, nchar(strings) - 1),
            T ~ strings)
}

cleanLeadingTsheg = function(strings){
  case_when(str_starts(strings, "་") ~ substr(strings, 2, nchar(strings)),
            T ~ strings)
}
