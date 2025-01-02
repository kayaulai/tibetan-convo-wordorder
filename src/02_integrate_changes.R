# This script updates a rezrDF according to the manually corrected annotations
# from data/01b_manual_tables and saves the result in data and output folders.
# Some rough validation is done as well.

library(tidyr)
library(dplyr)
library(here)
library(glue)
library(rezonateR)
library(beepr)
library(optparse)
library(cli)
source(here("src", "utils", "botools.R"))

discoName = "dramatroupe-script-8421"

#' Update rezrDF according to manually corrected table in
#' 01b_manual_tables.
#'
#' @param discoName The name of the file, with no file extension.
#'
#' @return A rezrDF with the updated values.
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

#' Save two copies of an updated DF: One in data/02_rezrDF as R data,
#' one as a csv in output/02_rezrDF.
#'
#' @param discoName The name of the file, with no file extension.
#' @param newDF The updated rezrDF.
#'
#' @return A rezrDF with the updated values.
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

#' Check the corrected filed for singletons and NA lines.
#' If there are issues, they are saved in output/debug.
#'
#' @param discoName The name of the file, with no file extension.
#' @param newDF The updated rezrDF.
#' @param debugging Whether we are in debugging mode.
checkOutput = function(discoName, newDF, debugging = TRUE){
  nas_present = newDF %>% filter(if_any(c("verbID", "argOrder",
                                    "noPrevMentions", "noPrevZero", "noNextMentions", "noNextZero",
                                    "bridge", "justFirst", "justLast", "local",
                                    "identifiable", "topic", "interrog", "animate",
                                    "self", "addressee", "length", "pronom"), is.na))
  singletons = newDF %>% group_by(verbID) %>% count %>% filter(n == 1)

  if(debugging & (nrow(nas_present) > 0 | nrow(singletons) > 0)){
    createDirIfNone(here("output", "debug", "01", discoName))
    cli_alert_warning(glue("See the following path for debug output: {here('output', 'debug', '01', discoName)}"))
    if(nrow(nas_present) > 0){
      write_csv(nas_present, here("output", "debug", "02", discoName, "02a_nas_present.csv"))
      View(nas_present)
    }
    if(nrow(singletons) > 0){
      write_csv(singletons, here("output", "debug", "02", discoName, "02b_singletons.csv"))
      View(singletons)
    }
  }
}

main = function(discoName, debugging = TRUE, beepWhenDone = TRUE){
  cli_alert_info("Updating annotations ...\n")
  newDF = updateDF(discoName)
  saveUpdatedDF(discoName, newDF)
  if(debugging){
    checkOutput(discoName, newDF, debugging = debugging)
  }
  cli_alert_success("Done!")
  if(beepWhenDone) beepr::beep()
}

if(interactive()){
  main(discoName)
} else {
  option_list = list(
      make_option(c("-d", "--disco"), type = "character", default = NULL, 
                help = "document name (no file extension)", metavar = "character"),
      make_option(c("-p", "--debug"), type = "logical", default = FALSE, 
                help = "run in debug mode?", metavar = "logical")
  )

  # Add if I can figure out how to beep from command line:
  # make_option(c("-b", "--beep"), type = "logical", default = TRUE, 
  # help = "beep when done?", metavar = "logical")

  opt_parser = OptionParser(option_list = option_list)
  opt_values = parse_args(opt_parser)
  cli_alert_info(glue("Current doc processed: {opt_values[['disco']]}"))
  main(opt_values[["disco"]], opt_values[["debug"]])
}