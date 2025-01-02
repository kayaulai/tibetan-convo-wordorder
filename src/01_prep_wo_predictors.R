#This is for doing word order stuff assuming that the ref expression bit is done.
library(tidyr)
library(dplyr)
library(rlang)
library(stringr)
library(here)
library(rezonateR)
library(glue)
library(beepr)
source(here("src", "utils", "files.R"))

discoName = "three-flirt-2501"

getRezrObj = function(discoName){
  obj = rez_load(here("data", "00_rezrObj", glue("{discoName}.Rdata")))
}

getRefexprDF = function(obj){
  obj = obj %>%
    addFieldForeign("track", "refexpr", "chunk", "verb",
                    "verbID", "verbTokenSeqFirst", "docTokenSeqFirst")
  df = obj$trackDF$refexpr
  df = df %>% rez_bind_rows(obj$trackDF$clauseArg, type = "union")
  df
}

getWOPredictorGuesses = function(currDF, currObj, discoName, debugging = FALSE){
  currDF = currDF %>%
    dplyr::arrange(docTokenSeqFirst) %>%
    filter(!(roleType %in% c("LVO")))
  #Relationship of referent to context

  #If there is no combinedChunk feature then we need to add it for the code to run
  if(!("combinedChunk" %in% colnames(currDF))){
    currDF = currDF %>% mutate(combinedChunk = "")
  }

  #First do the ones that depend on expressions not appearing in multi-argument clauses
  currDF = currDF %>%
    rez_mutate(noPrevMentions = countPrevMentionsIf(windowSize = 10, cond = (word != "<0>") & !str_detect(combinedChunk, "member")),
          noPrevZero = countPrevMentionsIf(windowSize = 10, cond = (word == "<0>")),
          noNextMentions = countNextMentionsIf(windowSize = 10, cond = (word != "<0>") & !str_detect(combinedChunk, "member")),
          noNextZero = countPrevMentionsIf(windowSize = 10, cond = (word == "<0>")),
          allPrevMentions = countPrevMentionsIf(windowSize = Inf, cond = (word != "<0>") & !str_detect(combinedChunk, "member")),
          allPrevZero = countPrevMentionsIf(windowSize = Inf, cond = (word == "<0>")),
          allNextMentions = countNextMentionsIf(windowSize = Inf, cond = (word != "<0>") & !str_detect(combinedChunk, "member")),
          allNextZero = countPrevMentionsIf(windowSize = Inf, cond = (word == "<0>")),
          noPrevMentionsFar = allPrevMentions- noPrevMentions,
          noPrevZeroFar = allPrevZero- noPrevZero,
          noNextMentionsFar = allNextMentions- noNextMentions,
          noNextZeroFar = allNextZero- noNextZero,
          bridge = countPrevBridges(windowSize = 10, attr(currObj$trailDF, "frameMatrix"),
                                    inclRelations = c("part-whole", "whole-part",
                                                      "group-individual", "individual-group",
                                                      "superset-subset", "subset-superset")),
          haveBridges = bridge > 0,
          local = case_when(person != 3 ~ T,
                            str_detect(word, "འདི ར") ~ T,
                            T ~ F))

  #Now remove the stuff that isn't relevant
  #Start with stuff that are zeroes or have the wrong type
  currDF = currDF %>%
    filter(word != "<0>",
          roleType %in% c("A", "S", "P", "OBL", "COPS", "COPLOC",
                          "PRED", "EXIST",
                          "LVS", "STIM", "PBEN", "LVP", "LVA", "ABEN",
                          "EXP", "BEN", "G", "R", "T"))
  
  if(debugging){
    createDirIfNone(here("output", "debug", discoName))
    message(glue("See the following path for debug output: {here('output', 'debug', discoName)}"))
    write_csv(currDF, here("output", "debug", discoName, "01a_removed_roles.csv"))
  }
  
  #Then remove the stuff that don't have corresponding verbs or are postverbal
  argDF = currDF %>%
    filter(docTokenSeqFirst < verbTokenSeqFirst,
          !is.na(verbTokenSeqFirst))
  
  if(debugging){
    write_csv(argDF, here("output", "debug", discoName, "01b_removed_noverb.csv"))
  }

  #Finally remove the arguments that have not preverbal clause-mates  
  argDF = argDF %>%
    rez_group_by(doc, verbID, verbWord, lemma, suffix, force, evidMod, TA, subord) %>%
    rez_mutate(argOrder = 1:n(), noArgs = n(), argBack = n():1)  %>%
    rez_ungroup() %>%
    arrange(docTokenSeqFirst) %>%
    filter(noArgs > 1)

  if(debugging){
    write_csv(argDF, here("output", "debug", discoName, "01c_removed_singletons.csv"))
  }

  #Now we can work on properties that depend only on expressions appearing in multi-argument clauses
  argDF = argDF %>%
    rez_mutate(justFirst = countPrevMentionsIf(windowSize = 10, cond = (word != "<0>" & argOrder == 1 & noArgs > 1)),
          justLast = countPrevMentionsIf(windowSize = 10, cond = (word != "<0>" & argBack == 1 & noArgs > 1)))

  #Semantic properties of the referent
  interrogWordsA = c("ག་པ ར|ག་རེ|ག་ཚོད|ག་ནས|ག་པ ར|ག་འདྲས|ག་པ ར་")
  interrogWordsB = c("སུ", "གང")
  interrogWordsBCollapsed = paste0(interrogWordsB, collapse = "|")

  ditransitiveVs = c("སྤྲོད|བསྐུར|སྤྲད|གཏོང")

  argDF = argDF %>%
    rez_mutate(interrog = case_when(
      TextNoCase %in% interrogWordsB ~ T,
      str_detect(TextNoCase, interrogWordsA) ~ T,
      str_detect(TextNoCase, interrogWordsBCollapsed) ~ NA,
      T ~ F
    ),
    argType = case_when(
      roleType %in% c("A", "ABEN", "LVA", "EXP") ~ "A",
      roleType == "P" & str_detect(verbWord, ditransitiveVs) ~ "T",
      roleType %in% c("P", "LVP", "PBEN", "STIM") ~ "P",
      roleType %in% c("S", "LVS") ~ "S",
      roleType %in% c("BEN", "R") ~ "R",
      roleType %in% c("G", "OBL") ~ "E",
      T ~ roleType
    ),
    topic = case_when(
      roleType == "COPS" ~ T,
      interrog == T ~ F,
      roleType == "PRED" ~ F,
      identifiable == F ~ F,
      T ~ NA
    )
  )

  argDF = argDF %>%
    rez_mutate(animate = entityType %in% c("person", "animal"),
          self = (person == 1),
          addressee = (person == 2))

  #Formal properties of the argument
  tracksContainingVerbs = findContainingChunk(currObj$chunkDF$verb, argDF)
  tracksContainingVerbs = tracksContainingVerbs[!is.na(tracksContainingVerbs)]
  argDF = argDF %>%
    rez_mutate(length = str_count(wordWylie, " +"),
          pronom = lexicality == "p",
          containsVerb = id %in% tracksContainingVerbs)

  if(debugging){
    View(argDF %>% select(unitSeqFirst, wordWylie, word, verbWord, roleType, verbID))
  }
  argDF
  }


saveArgDF = function(argDF){
  #Write the table
  rez_write_csv(argDF, here("data", "01a_r_tables", glue("{discoName}.csv")),
  c("id", "name", "unitLastWord", "unitSeqLast", "word", "docTokenSeqFirst",
    "docTokenSeqLast","local", "identifiable", "topic", "interrog",
    "argType", "animate", "self", "addressee", "length", "pronom", "haveBridges"))

  #Save the argDF too
  rez_save(argDF, here("data", "01c_rezrDF", glue("{discoName}.Rdata")))

  message("After this point you should:
    -fix local variable
    -fix topic variable
    -do argtype P > T
    -fix interrog if necessary
  ")

  #If the manual table exists, its IDs need to be updated
  if(file.exists(here("data", "01b_manual_tables", glue("{discoName}.csv")))){
    old_ref_table = read_csv(initPath %+% "wo1_2_manual_tables/" %+% discoName %+% ".csv")
    missing_in_new = setdiff(old_ref_table$id, argDF$id)
    message("For the following, make sure to delete them from the old manual table if they're no longer needed,\n or check if the new file inadverdently deleted sth")
    for(missingID in missing_in_new){
      lastUnit = old_ref_table$unitSeqLast[old_ref_table$id == missingID]
      lastDTSF = old_ref_table$docTokenSeqFirst[old_ref_table$id == missingID]
      lastDTSL = old_ref_table$docTokenSeqLast[old_ref_table$id == missingID]
      word = old_ref_table$word[old_ref_table$id == missingID]
      newID = argDF %>% filter(unitSeqLast == lastUnit,
                                docTokenSeqFirst == lastDTSF,
                                docTokenSeqLast == lastDTSL,
                                word == word) %>% pull(id)
      if(length(newID) == 1){
        old_ref_table$id[old_ref_table$id == missingID] = newID
      } else {
        if(length(newID) > 1){
          message(paste0("Missing from new table with multiple candidate correpondences in old table: ",missingID))
        } else {
          message(paste0("Missing from new table: ", missingID))
        }
      }
    }
    message("Make sure to add these IDs to the manual table:", paste0(setdiff(argDF$id, old_ref_table$id), collapse = ", "))
    rez_write_csv(old_ref_table, initPath %+% "wo1_2_manual_tables/" %+% discoName %+% ".csv")
  }
}

main = function(discoName, debugging = TRUE, beepWhenDone = TRUE){
  message("Extracting referential expression DF from rezrObj ...")
  currObj = getRezrObj(discoName)
  refexprDF = getRefexprDF(currObj)
  message("Getting arguments DF ...")
  argDF = getWOPredictorGuesses(refexprDF, currObj, discoName, debugging)
  message("Saving ...")
  saveArgDF(argDF)
  if(beepWhenDone) beepr::beep()
}

main(discoName)
