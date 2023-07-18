rm(list = ls())

library(tidyverse)
library(stringr)
library(rvest)
library(httr)

my.folder <- "/Users/mesako/Desktop/Research/CTL_SOTL"
setwd(my.folder)

institution.sample <- readRDS(file = "institution_sample.rds")

### Standardize URLs
institution.sample$url_school <- gsub(institution.sample$url_school, pattern = "^www", replacement = "http://www")
fix.these <- !grepl(pattern = "^http", x = institution.sample$url_school)
institution.sample$url_school[fix.these] <- paste("http://www.", institution.sample$url_school[fix.these], sep = "")

### Add Possible CTL URLs
institution.sample <- institution.sample %>% 
  mutate(CTL_url1 = NA, CTL_url2 = NA,
         CTL_url3 = NA, CTL_url4 = NA,
         CTL_url5 = NA)

last.cols <- (ncol(institution.sample) - 4):ncol(institution.sample)
key.phrase <- paste("center", "for", "teaching", "and", "learning", "faculty", "development", sep = "+")

for (x in 298:nrow(institution.sample)) {
  Sys.sleep(60)
  print(institution.sample$inst_name[x])
  temp <- institution.sample$url_school[x]
  search.term <- paste(temp, key.phrase, sep = "+")
  search.term <- paste("https://www.google.co.in/search?q=", search.term, sep = "")
  my.search <- read_html(search.term)
  my.links <- my.search %>% html_nodes(xpath = "//a") %>% html_attr("href")
  my.links <- strsplit(my.links[as.vector(grep("url", my.links))], split = "&")
  my.links <- sapply(my.links, "[", 1)
  my.links <- gsub("/url\\?q=", "", my.links)
  my.links <- head(my.links, n = 5)
  institution.sample[x, last.cols] <- my.links
  closeAllConnections()
  if (x %% 20 == 0) {
    my.file <- paste("institution_sample_up_to", x, ".rds", sep = "")
    saveRDS(institution.sample, file = my.file)
  }
}

institution.sample <- readRDS("institution_sample_up_to312.rds")
write.csv(institution.sample, file = "full_institution_sample.csv",
          row.names = FALSE)
