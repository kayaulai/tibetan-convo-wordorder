library(tidyr)
library(dplyr)
library(here)
library(glue)
library(rezonateR)
library(beepr)
source(here("src", "utils", "botools.R"))

discoName = "dramatroupe-script-8421"

updateDF = function(discoName){
  origDF = rez_load(here("data", "01c_rezrDF", glue("{discoName}.Rdata")))

  #Import the change DFs
  changes = rez_read_csv(here("data", "01b_manual_tables", glue("{discoName}.csv")), origDF = origDF)

  #Implement changes
  colsToChange = c("local", "identifiable", "topic", "interrog", "argType", "animate", "self", "addressee", "pronom")
  if("haveBridges" %in% colnames(changes)) colsToChange = c(colsToChange, "haveBridges")
  newDF = origDF %>%
    updateFromDF(changes,
                changeCols = colsToChange,
                delRows = TRUE)
  newDF
}

saveUpdatedDF = function(discoName, newDF){
  rez_save(newDF, here("data", "02_rezrDF", glue("{discoName}.Rdata")))
  rez_write_csv(newDF, here("output", "02_final_data", glue("{discoName}.csv")),
                c("id", "name", "unitLastWord", "unitSeqLast", "word",
                  "argOrder", "noPrevMentions", "noPrevZero", "noNextMentions", "noNextZero",
                  "bridge", "justFirst", "justLast", "local", "identifiable", "topic",
                  "interrog", "argType",
                  "animate", "self", "addressee",
                  "length","pronom"))
}

checkOutput = function(discoName, newDF, debugging = TRUE){
  nas_present = newDF %>% filter(if_any(c("verbID", "argOrder",
                                    "noPrevMentions", "noPrevZero", "noNextMentions", "noNextZero",
                                    "bridge", "justFirst", "justLast", "local",
                                    "identifiable", "topic", "interrog", "animate",
                                    "self", "addressee", "length", "pronom"), is.na))
  singletons = newDF %>% group_by(verbID) %>% count %>% filter(n == 1)

  if(debugging & (nrow(nas_present) > 0 | nrow(singletons) > 0)){
    createDirIfNone(here("output", "debug", "01", discoName))
    message(glue("See the following path for debug output: {here('output', 'debug', '01', discoName)}"))
    if(nrow(nas_present) > 0){
      write_csv(nas_present, here("output", "debug", discoName, "02a_nas_present.csv"))
      View(nas_present)
    }
    if(nrow(singletons) > 0){
      write_csv(singletons, here("output", "debug", discoName, "02b_singletons.csv"))
      View(singletons)
    }
  }
}

main = function(discoName, debugging = TRUE, beepWhenDone = TRUE){
  message("Updating annotations ...\n")
  newDF = updateDF(discoName)
  saveUpdatedDF(discoName, newDF)
  if(debugging){
    checkOutput(discoName, newDF, debugging = debugging)
  }
  message("Done!")
  if(beepWhenDone) beepr::beep()
}

main(discoName)