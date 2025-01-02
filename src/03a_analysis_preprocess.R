
library(tidyverse)
library(tidyr)
library(rlang)
library(rezonateR)
library(here)
library(stringr)
library(glue)
source(here("src", "utils", "files.R"))

#' Import all discourse data from data/02_rezrdf.
#' 
#' @return A data frame with the data from all documents.
importAllDiscoData = function(){
    discoNames = getCurrDocNames()
    discoNames %>%
        map(function(x) suppressMessages(rez_load(here("data", "02_rezrDF", glue("{x}.Rdata"))))) %>%
        reduce(rez_bind_rows)
}

#' Encode the features of the data frame for use in ROLLing.
#' 
#' @param df An input data frame.
#' 
#' @return A data frame with features encoded for use in ROLLing.
encodeFeatures = function(df){
    featsChosen = c("argTypeNew",
            "noPrevMentions", "noPrevZero", "noNextMentions", "noNextZero",
            "haveBridges", "identifiable", "local", "justFirst", "justLast", 
                "interrog", "animate",
            "self", "addressee", "length", "pronom", "noPrevMentionsFar", "noNextMentionsFar")
    featsQuant = c("noPrevMentions", "noPrevZero", "noNextMentions", "noNextZero",
                "justFirst", "justLast", "length", "noPrevMentionsFar", "noNextMentionsFar")
    featsQual = setdiff(featsChosen, featsQuant) %>% c("topic")
    featsBinary = setdiff(featsQual, "argTypeNew")

    log_p1 = function(x) log(x + 1)
    df %>% mutate(identifiable = factor(identifiable, levels = c("n", "i"))) %>%
            mutate(across(all_of(featsBinary), as.integer)) %>%
            mutate(argType = as.factor(argType)) %>%
            mutate(argTypeNew = as.factor(case_when(argType == "T" ~ "P",
                                                    argType == "A" ~ "Base",
                                                    argType == "S" ~ "Base",
                                                    argType == "COPS" ~ "Base",
                                                    argType == "COPLOC" ~ "Base",
                                        T ~ argType))) %>%
            mutate(across(all_of(featsQuant), log_p1)) 
}

#' Check the encoded data frame for issues that may need
#' annotator attention, including singletons (arguments without
#' clausemates) and NA values. If issues are present, user will
#' be alerted and the faulty rows will be written in outputs/debug.
#' 
#' @param df The data frame to be checked.
checkEncodedDF = function(df){
    clauses = df %>% group_by(verbID) %>% count
    message(glue("Number of clauses: {nrow(clauses)}"))

    singletons = clauses %>% filter(n == 1)
    if(nrow(singletons) > 0){
        View(singletons)
        write_csv(singletons, here("output", "debug", "03_analysis", "singletons.csv"))
        message(glue("You have some singleton clauses; please check the output at {here(\"output\", \"debug\", \"analysis\", \"singletons.csv\")}."))
    }

    cols_na_present = df %>% sapply(function(x) any(is.na(x)))
    na_cols = intersect(featsChosen, names(cols_na_present)[which(cols_na_present)])
    if(length(na_cols) > 0){
        na_rows = df %>% filter(if_any(all_of(featsChosen), is.na))
        View(na_rows)
        write_csv(na_rows, here("output", "debug", "03_analysis", "na_rows.csv"))
        message(glue("The following columns have NA values: {paste0(na_cols, collapse = ", ")}"))
        message(glue("Please check the output at {here(\"output\", \"debug\", \"analysis\", \"na_rows\")}."))
    }
}

#' Save the encoded data frame.
#' 
#' @param df The data frame to be saved.
saveEncodedDF = function(df){
    saveRDS(df, here("output", "03a_coded_data", "wodata.rds"))
    write_csv(df, here("output", "03a_coded_data", "wodata.csv"))
}

main = function(){
    df = importAllDiscoData()
    df = encodeFeatures(df)
    checkEncodedDF(df)
    saveEncodedDF(df)
}

main()