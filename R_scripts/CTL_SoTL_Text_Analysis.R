rm(list = ls())

library(tidyverse)
library(stringr)
library(quanteda)
library(quanteda.textplots)

my.folder <- "/Users/mesako/Desktop/Research/CTL_SOTL"
setwd(my.folder)

### Load Text Data
ctl.corpus <- readRDS(file = "data/all_CTL_website_text.rds")

### Convert and Examine Text Corpus
my.corpus <- ctl.corpus[names(ctl.corpus) != ""]
my.corpus <- lapply(my.corpus, function(x) x[!is.na(x)])
my.corpus <- sapply(my.corpus, paste, collapse = " ")
my.corpus <- Filter(Negate(anyNA), my.corpus)
my.corpus <- unlist(my.corpus)

my.corpus <- tokens(my.corpus, remove_symbols = TRUE, remove_numbers = TRUE,
                    remove_punct = TRUE, remove_separators = TRUE,
                    remove_url = TRUE)

# Plot Top 100 Terms
ctl.dfm <- dfm(my.corpus, remove_punct = TRUE) %>% 
  dfm_remove(pattern = stopwords("english")) %>% 
  dfm_wordstem() %>%
  dfm_trim(min_termfreq = 100, min_docfreq = 0.5, 
           termfreq_type = "rank", docfreq_type = "prop")

set.seed(10)
textplot_wordcloud(ctl.dfm)

### Set up SoTL Mention Keyword Dictionary
# Define SoTL Search Terms
sotl.mention <- list("SoTL" = "scholarship of teaching and learning, \\(SoTL\\), SoTL",
                     "DBER" = "discipline-based education research, discipline-based educational research, \\(DBER\\), DBER",
                     "TAR" = "teaching-as-research, teaching as research, \\(TAR\\), TAR",
                     "EdRes" = "education research, educational research")
sotl.mention <- sapply(sotl.mention, strsplit, split = "\\, ")
sotl.mention <- dictionary(sotl.mention)

### Match Text Corpus to Dictionary
mention.search <- dfm(my.corpus, dictionary = sotl.mention)
which.mention <- convert(mention.search, to = "data.frame")
rownames(which.mention) <- which.mention$doc_id
which.mention$doc_id <- NULL
which.mention <- which.mention > 0

### Align with CTL Data
institution.sample <- read.csv(file = "data/full_institution_sample_shortened.csv",
                               na.strings = c("", "NA"))
institution.sample <- institution.sample %>% mutate(HasCTL = !is.na(CTL_likely))
institution.sample <- institution.sample %>% select(-(CTL_url1:CTL_url5))

institution.sample$url_school <- NULL
institution.sample$url_system <- NULL
institution.sample$title_iv_indicator <- NULL
institution.sample$inst_alias <- NULL
institution.sample$state_abbr <- NULL
institution.sample$city <- NULL
institution.sample$county_name <- NULL
institution.sample$region <- NULL
final.data <- institution.sample

final.data$inst_size <- as.character(final.data$inst_size)
final.data$inst_size <- factor(final.data$inst_size,
                               levels = c("Under 1", "1", "5", "10", "20"))

final.data$cc_class <- gsub(final.data$cc_class, pattern = "Associate s", replacement = "Associates")
final.data$cc_class <- gsub(final.data$cc_class, pattern = "Master s", replacement = "Masters")
final.data$cc_class <- gsub(final.data$cc_class, pattern = "associate s", replacement = "associates")
final.data$cc_class <- gsub(final.data$cc_class, pattern = "master s", replacement = "masters")

final.data$cc_class <- factor(final.data$cc_class)

add.in.data <- match(rownames(which.mention), final.data$inst_name)
missing.data <- setdiff(1:nrow(final.data), add.in.data)

temp.data1 <- final.data[add.in.data, ]
temp.data1 <- cbind(temp.data1, which.mention)

blank.data <- data.frame(matrix(nrow = length(missing.data),
                                ncol = ncol(which.mention)))
colnames(blank.data) <- colnames(which.mention)
temp.data2 <- final.data[missing.data, ]
temp.data2 <- cbind(temp.data2, blank.data)

final.data <- rbind(temp.data1, temp.data2)
final.data <- final.data[order(final.data$ID), ]
saveRDS(final.data, file = "data/final_institution_data.rds")

### Set up SoTL Model Keyword Dictionary
my.keywords <- read.csv(file = "data/sotl_model_keywords-final.csv")
sotl.dict <- my.keywords$Keywords
names(sotl.dict) <- my.keywords$SoTLProgramModels
sotl.dict <- as.list(sotl.dict)
sotl.dict <- sapply(sotl.dict, strsplit, split = " ")
sotl.dict <- dictionary(sotl.dict)

### Match Text Corpus to Dictionary
# first filter for only SoTL/DBER/Education Research institutions
keep.ind <- final.data[match(names(my.corpus), final.data$inst_name), ]
keep.ind1 <- keep.ind$SoTL | keep.ind$DBER | keep.ind$TAR | keep.ind$EdRes
my.corpus <- my.corpus[keep.ind1, ]

# Keyword-in-context
# grab 50 words before and after each mention of SoTL-like key terms
all.sotl.mentions <- kwic(my.corpus, pattern = sotl.mention, window = 50)
all.sotl.mentions <- as.data.frame(all.sotl.mentions)
all.sotl.mentions <- all.sotl.mentions %>% 
  mutate(Start = from - 50) %>% 
  mutate(End = to + 50)
all.sotl.mentions$from <- NULL
all.sotl.mentions$to <- NULL
all.sotl.mentions <- all.sotl.mentions %>% relocate(Start, .after = docname)
all.sotl.mentions <- all.sotl.mentions %>% relocate(End, .after = Start)

CheckOverlap <- function(all.ranges) {
  start1 <- all.ranges[1]
  end1 <- all.ranges[2]
  start2 <- all.ranges[3]
  end2 <- all.ranges[4]
  check1 <- start1 <= end2
  check2 <- end1 <= start2
  final.check <- check1 && check2
  return(final.check)
}

CheckOverlapDF <- function(my.df) {
  overlap.check <- c(FALSE)
  for (i in 2:nrow(my.df)) {
    if (my.df$docname[i] != my.df$docname[i - 1]) {
      overlap.check <- c(overlap.check, FALSE)
      next
    } else {
      check.these <- my.df[(i - 1):i, c("Start", "End")]
      check.these <- unname(unlist(check.these))
      overlap.check <- c(overlap.check, CheckOverlap(check.these))
    }
  }
  modified.df <- my.df %>% mutate(Overlap = overlap.check)
  return(modified.df)
}

all.sotl.mentions <- CheckOverlapDF(all.sotl.mentions)
all.sotl.mentions <- all.sotl.mentions %>% relocate(Overlap, .after = End)
fix.min <- all.sotl.mentions$Start <= 0
all.sotl.mentions$Start[fix.min] <- 1

DefineTextWindows <- function(my.df) {
  text.windows <- c(my.df$Start[1], my.df$End[1])
  names(text.windows)[1:2] <- my.df$docname[1:2]
  for (i in 2:nrow(my.df)) {
    if (my.df$docname[i] == my.df$docname[i - 1]) {
      if (my.df$Overlap[i]) {
        text.windows[length(text.windows)] <- my.df$End[i]
      } else {
        text.windows <- c(text.windows, my.df$Start[i], my.df$End[i])
        names(text.windows)[(length(text.windows) - 1):length(text.windows)] <- my.df$docname[i]
      }
    } else {
      text.windows <- c(text.windows, my.df$Start[i], my.df$End[i])
      names(text.windows)[(length(text.windows) - 1):length(text.windows)] <- my.df$docname[i]
    }
  }
  return(text.windows)
}

new.text.windows <- DefineTextWindows(all.sotl.mentions)
text.df <- matrix(new.text.windows, ncol = 2, byrow = TRUE)
text.df <- as.data.frame(text.df)
df.names <- names(new.text.windows)[seq(from = 1, to = length(names(new.text.windows)), by = 2)]
colnames(text.df) <- c("Start", "End")
text.df <- cbind(docname = df.names, text.df)
text.df <- text.df %>% mutate(Text = NA)

for (i in 1:nrow(text.df)) {
  get.document <- tokens_subset(my.corpus, docname_ == text.df$docname[i])
  check.boundaries <- ntoken(get.document)
  fixed.end <- min(text.df$End[i], check.boundaries)
  temp.tokens <- tokens_select(get.document, startpos = text.df$Start[i], endpos = fixed.end)
  text.df$Text[i] <- paste(temp.tokens, collapse = " ")
}

# create new mini corpus only of the surrounding text
# merge all mentions for a single institution together
text.df <- text.df %>% group_by(docname) %>% 
  summarise(Text = paste(Text, collapse = " "))

new.corpus <- text.df$Text
names(new.corpus) <- text.df$docname
new.corpus <- tokens(new.corpus)

sotl.search <- dfm(new.corpus, dictionary = sotl.dict)
which.sotl <- apply(sotl.search, MARGIN = 1, order, decreasing = TRUE)
which.sotl <- as.data.frame(which.sotl)
sotl.number <- colnames(sotl.search)
names(sotl.number) <- 1:ncol(sotl.search)
names(sotl.number) <- paste("^", names(sotl.number), "$", sep = "")
which.sotl <- as.data.frame(sapply(which.sotl, str_replace_all, sotl.number))

top.guess <- match(names(which.sotl[1, ]), text.df$docname)
top.guess <- unlist(unname(which.sotl[1, top.guess]))
text.df <- text.df %>% mutate(Model1 = top.guess)

second.guess <- match(names(which.sotl[2, ]), text.df$docname)
second.guess <- unlist(unname(which.sotl[2, second.guess]))
text.df <- text.df %>% mutate(Model2 = second.guess)

third.guess <- match(names(which.sotl[3, ]), text.df$docname)
third.guess <- unlist(unname(which.sotl[3, third.guess]))
text.df <- text.df %>% mutate(Model3 = third.guess)

text.df <- text.df %>% relocate(Text, .after = Model3)

write.csv(text.df, file = "results/ctl_mentions_of_sotl.csv",
          row.names = FALSE)
