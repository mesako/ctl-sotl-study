rm(list = ls())

library(tidyverse)
library(stringr)
library(tidytext)

my.folder <- "/Users/mesako/Desktop/Research/CTL_SOTL"
setwd(my.folder)

### Load Taxonomy Table
sotl.taxonomy <- read.csv(file = "data/sotl-taxonomy-final.csv")
colnames(sotl.taxonomy) <- gsub(colnames(sotl.taxonomy), pattern = "\\.", replacement = "")

### Standardize Text
sotl.taxonomy <- as.data.frame(sapply(sotl.taxonomy, str_to_lower))
sotl.taxonomy <- as.data.frame(sapply(sotl.taxonomy, str_replace_all, 
                                      pattern = "[:punct:]", replacement = " "))
sotl.taxonomy <- as.data.frame(sapply(sotl.taxonomy, str_replace_all, 
                                      pattern = "[:symbol:]", replacement = " "))
sotl.taxonomy <- as.data.frame(sapply(sotl.taxonomy, str_replace_all, 
                                      pattern = "[:digit:]", replacement = " "))
sotl.taxonomy <- as.data.frame(sapply(sotl.taxonomy, str_squish))

### Remove Stopwords and Repeats
sotl.taxonomy <- sotl.taxonomy %>% 
  unite(col = "Definition", BriefDescription:Definition, sep = " ")

sotl.taxonomy <- sotl.taxonomy %>%
  unnest_tokens(Definition, Definition) %>%
  anti_join(stop_words, by = c("Definition" = "word")) %>%
  group_by(SoTLProgramCategory, SoTLProgramModels) %>%
  summarise(Definition = paste0(Definition, collapse = " "))

remove.these <- c("ctl", "sotl", "university", "sotlers",
                  "sotler", "people", "participants",
                  "gra", "faculty")

for (x in 1:nrow(sotl.taxonomy)) {
  my.def <- sotl.taxonomy$Definition[x]
  my.def <- unlist(str_split(my.def, pattern = " "))
  my.def <- my.def[!(my.def %in% remove.these)]
  my.def <- unique(trimws(my.def))
  my.def <- paste(my.def, collapse = " ")
  sotl.taxonomy$Definition[x] <- my.def
  print(my.def)
}

### Fix Word Stems
fix.these <- list("providing|provided|provides" = "provide",
                  "discussions|discussion|discusses|discusss" = "discuss",
                  "informally" = "informal", "methods" = "method",
                  "sharing|shared|shares" = "share",
                  "attending|attended|attends" = "attend",
                  "designed|designs|designing" = "design",
                  "offered|offers" = "offer", "venues" = "venue",
                  "participates|participated|participating|participation" = "participate",
                  "includes|including" = "include", "constructing" = "construct",
                  "publishes|publishing" = "publish",
                  "informed" = "inform", "collecting" = "collect",
                  "analyzing|analysis" = "analyze", "serving" = "serve",
                  "questions" = "question", "collaborators" = "collaborator",
                  "facilitating|facilitates|facilitation" = "facilitate",
                  "disseminating|dissemination" = "disseminate",
                  "engagement|engaging" = "engage",
                  "events" = "event", "experts" = "expert",
                  "goals" = "goal", "graduate" = "grad",
                  "grants" = "grant", "incentives" = "incentive",
                  "individuals" = "individual", "instructors" = "instructor",
                  "involves" = "involve", "learning" = "learn",
                  "meetings|meeting" = "meet", "periods" = "period",
                  "producing" = "produce", "projects" = "project",
                  "programs|programming" = "program",
                  "reducing" = "reduce", "students" = "student",
                  "supporting|supports" = "support",
                  "teams" = "team", "terms" = "term",
                  "trainees" = "trainee", "training|trains" = "train",
                  "varies" = "vary")

for (x in 1:length(fix.these)) {
  sotl.taxonomy$Definition <- gsub(sotl.taxonomy$Definition,
                                   pattern = names(fix.these)[x],
                                   replacement = fix.these[[x]])
}

for (x in 1:nrow(sotl.taxonomy)) {
  my.def <- sotl.taxonomy$Definition[x]
  my.def <- unlist(str_split(my.def, pattern = " "))
  my.def <- unique(trimws(my.def))
  my.def <- paste(my.def, collapse = " ")
  sotl.taxonomy$Definition[x] <- my.def
  print(my.def)
}

write.csv(sotl.taxonomy, file = "data/sotl_model_keywords.csv", row.names = FALSE)
