#This is for doing word order stuff assuming that the ref expression bit is done.
library(tidyr)
library(dplyr)
library(rezonateR)

#Annos that need fixing: pfirst_right_ids[10]

discoNames = c("three-caught-8261", "three-parting-2569",
               "three-friction-2883", "three-luckydreamalt-8296",
               "three-scold-21496", "virginia-altar-2563",
               "virginia-library-20766", "virginia-neighbour-2565",
               "three-chupa-8346", "three-counsel-8176",
               "lhasa-interview-2561", "three-caught-8261",
               "three-butter-8511", "three-dadronpudron-8256",
               "three-vegetables-9201", "three-wrap-8561",
               "three-dadronmum-2537", "three-cheese-8521",
               "other-karmakunsang-2536", "three-marry-2275",
               "young-fight1-11786",
              "island-rollerstaff-9331", "three-upset-2535",
              "island-sangdadorje-8501", "dramatroupe-script-8421",
              "virginia-america-2564"
)
discoName = c("dramatroupe-script-8421")
#initPath = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/"
initPath = "C:/Users/kayau/Documents/GitHub/lhasa-reference-tracking/shanti/"
setwd(initPath)
source("botools.R")
corpus = rez_load(initPath  %+% "4_1_rezrObj/" %+% discoName %+% ".Rdata")
currDF = rez_load(initPath  %+% "wo1_3_rezrDF/" %+% discoName %+% ".Rdata")

#Import the change DFs and implement changes
changes = rez_read_csv(initPath  %+% "wo1_2_manual_tables/" %+% discoName %+% ".csv", origDF = corpus$trackDF$refexpr)

colsToChange = c("local", "identifiable", "topic", "interrog", "argType", "animate", "self", "addressee", "pronom")
if("haveBridges" %in% colnames(changes)) colsToChange = c(colsToChange, "haveBridges")
currDF = currDF %>%
  updateFromDF(changes,
               changeCols = colsToChange,
               delRows = T)

rez_save(currDF, initPath  %+% "wo1_3_rezrDF/" %+% discoName %+% ".Rdata")
rez_write_csv(currDF, "Texts presented/anno_csling/" %+% discoName %+% ".csv",
              c("id", "name", "unitLastWord", "unitSeqLast", "word",
                "argOrder", "noPrevMentions", "noPrevZero", "noNextMentions", "noNextZero",
                "bridge", "justFirst", "justLast", "local", "identifiable", "topic",
                "interrog", "argType",
                "animate", "self", "addressee",
                "length","pronom"))

#Error checks
currDF %>% filter(if_any(c("verbID", "argOrder",
                                  "noPrevMentions", "noPrevZero", "noNextMentions", "noNextZero",
                                  "bridge", "justFirst", "justLast", "local",
                                  "identifiable", "topic", "interrog", "animate",
                                  "self", "addressee", "length", "pronom"), is.na))
currDF %>% group_by(verbID) %>% count %>% filter(n == 1)
