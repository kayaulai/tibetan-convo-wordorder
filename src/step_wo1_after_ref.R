#This is for doing word order stuff assuming that the ref expression bit is done.
library(tidyr)
library(dplyr)
library(rlang)
library(stringr)
#library(rezonateR)

#Need redoing:  "three-flirt-2501", "dramatroupe-interview-2719"

discoNames = c("three-caught-8261",
               "three-friction-2883", "three-luckydreamalt-8296",
               "three-scold-21496", "virginia-altar-2563",
               "virginia-library-20766", "virginia-neighbour-2565",
               "three-chupa-8346", "three-counsel-8176",
               "lhasa-interview-2561", "three-caught-8261",
               "three-butter-8511", "three-dadronpudron-8256",
               "three-vegetables-9201", "three-wrap-8561",
               "three-parting-2569",
               "three-cheese-8521", "other-karmakunsang-2536",
               "three-marry-2275", "young-fight1-11786",
              "island-rollerstaff-9331", "three-upset-2535",
              "island-sangdadorje-8501", "dramatroupe-script-8421",
            "virginia-america-2564")
discoName = "virginia-library-20766"

#initPath = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/"
initPath = "C:/Users/kayau/Documents/GitHub/lhasa-reference-tracking/shanti/"
setwd(initPath)

currObj = rez_load(initPath %+% "5_1_rezrObj/" %+% discoName %+% ".Rdata")
currObj = currObj %>%
  addFieldForeign("track", "refexpr", "chunk", "verb",
                  "verbID", "verbTokenSeqFirst", "docTokenSeqFirst")
currDF = currObj$trackDF$refexpr
currDF = currDF %>% rez_bind_rows(currObj$trackDF$clauseArg, type = "union")
argProps = setdiff(colnames(currDF), c("doc", "verbID", "verbWord", "lemma", "suffix", "force", "evidMod", "TA", "subord"))

# firstOfEach = function(roleType){
#   if(length(unique(roleType)) < 2){
#     character(length(roleType))
#   } else {
#     result = character(length(roleType))
#     if(any(roleType == "A")) result[which(roleType == "A")[1]] = "A"
#     if(any(roleType == "P")) result[which(roleType == "P")[1]] = "P"
#     if(any(roleType %in% c("G", "OBL"))) result[which(roleType  %in% c("G", "OBL"))[1]] = "OBL"
#     result
#   }
# }
#
# firstOfEach = function(roleType){
#   if(length(unique(roleType)) < 2){
#     character(length(roleType))
#   } else {
#     result = character(length(roleType))
#     if(any(roleType == "COPLOC")) result[which(roleType == "A")[1]] = "COPLOC"
#     if(any(roleType == "EXIST")) result[which(roleType == "EXIST")[1]] = "EXIST"
#     if(any(roleType == "PRED")) result[which(roleType == "A")[1]] = "PRED"
#     if(any(roleType == "COPS")) result[which(roleType == "COPS")[1]] = "COPS"
#
#     result
#   }
# }

currDF = currDF %>%
  arrange(docTokenSeqFirst) %>%
  filter(!(roleType %in% c("LVO")))
#Relationship of referent to context

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

currDF = currDF %>%
  filter(word != "<0>",
         roleType %in% c("A", "S", "P", "OBL", "COPS", "COPLOC",
                         "PRED", "EXIST",
                         "LVS", "STIM", "PBEN", "LVP", "LVA", "ABEN",
                         "EXP", "BEN", "G", "R", "T"),
         docTokenSeqFirst < verbTokenSeqFirst,
         !is.na(verbTokenSeqFirst))  %>%
  rez_group_by(doc, verbID, verbWord, lemma, suffix, force, evidMod, TA, subord) %>%
  rez_mutate(argOrder = 1:n(), noArgs = n(), argBack = n():1)  %>%
  rez_ungroup() %>%
  arrange(docTokenSeqFirst) %>%
  filter(noArgs > 1)

#Then do the ones that depend only on expressions appearing in multi-argument clauses
currDF = currDF %>%
  rez_mutate(justFirst = countPrevMentionsIf(windowSize = 10, cond = (word != "<0>" & argOrder == 1 & noArgs > 1)),
        justLast = countPrevMentionsIf(windowSize = 10, cond = (word != "<0>" & argBack == 1 & noArgs > 1)))

#Semantic properties of the referent
interrogWordsA = c("ག་པ ར|ག་རེ|ག་ཚོད|ག་ནས|ག་པ ར|ག་འདྲས|ག་པ ར་")
interrogWordsB = c("སུ", "གང")
interrogWordsBCollapsed = paste0(interrogWordsB, collapse = "|")

ditransitiveVs = c("སྤྲོད|བསྐུར|སྤྲད|གཏོང")

currDF = currDF %>%
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

#Semantic properties of the referent
currDF = currDF %>%
  rez_mutate(animate = entityType %in% c("person", "animal"),
         self = (person == 1),
         addressee = (person == 2))

#Formal properties of the argument
tracksContainingVerbs = findContainingChunk(currObj$chunkDF$verb, currDF)
tracksContainingVerbs = tracksContainingVerbs[!is.na(tracksContainingVerbs)]
currDF = currDF %>%
  rez_mutate(length = str_count(wordWylie, " +"),
         pronom = lexicality == "p",
         containsVerb = id %in% tracksContainingVerbs)



rez_write_csv(currDF, "wo1_1_r_tables/" %+% discoName %+% ".csv",
              c("id", "name", "unitLastWord", "unitSeqLast", "word", "docTokenSeqFirst",
                "docTokenSeqLast","local", "identifiable", "topic", "interrog",
                "argType", "animate", "self", "addressee", "length", "pronom", "haveBridges"))

rez_save(currDF, initPath %+% "wo1_3_rezrDF/" %+% discoName %+% ".Rdata")

#List of things to do:
#fix local
#fix topic
#do argtype P > T
#fix interrog if necessary

View(currDF %>% select(unitSeqFirst, wordWylie, word, verbWord, roleType, verbID))

if(file.exists(initPath %+% "wo1_2_manual_tables/" %+% discoName %+% ".csv")){
  old_ref_table = read_csv(initPath %+% "wo1_2_manual_tables/" %+% discoName %+% ".csv")
  missing_in_new = setdiff(old_ref_table$id, currDF$id)
  print("For the following, make sure to delete them from the old manual table if they're no longer needed,\n or check if the new file inadverdently deleted sth")
  for(missingID in missing_in_new){
    lastUnit = old_ref_table$unitSeqLast[old_ref_table$id == missingID]
    lastDTSF = old_ref_table$docTokenSeqFirst[old_ref_table$id == missingID]
    lastDTSL = old_ref_table$docTokenSeqLast[old_ref_table$id == missingID]
    word = old_ref_table$word[old_ref_table$id == missingID]
    newID = currDF %>% filter(unitSeqLast == lastUnit,
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
  message("Make sure to add these IDs to the manual table:", paste0(setdiff(currDF$id, old_ref_table$id), collapse = ", "))
  rez_write_csv(old_ref_table, initPath %+% "wo1_2_manual_tables/" %+% discoName %+% ".csv")
}

library(beepr)
beep()
