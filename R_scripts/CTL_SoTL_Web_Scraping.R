rm(list = ls())

library(tidyverse)
library(stringr)
library(see)
library(rvest)
library(httr)

my.folder <- "/Users/mesako/Desktop/Research/CTL_SOTL"
setwd(my.folder)

### Load and Clean Data
institution.sample <- read.csv(file = "data/full_institution_sample_shortened.csv",
                               na.strings = c("", "NA"))
institution.sample <- institution.sample %>% mutate(HasCTL = !is.na(CTL_likely))

drop.cols <- c("CTL_url1", "CTL_url2", "CTL_url3", "CTL_url4", "CTL_url5")
institution.sample <- institution.sample %>% select(-all_of(drop.cols))
institution.sample <- institution.sample %>% relocate(CTL_likely, .after = inst_name)
institution.sample <- institution.sample %>% relocate(HasCTL, .after = CTL_likely)

### Check Final Distributions
# geo_region
table(institution.sample$geo_region)

# cc_class
table(institution.sample$cc_class)

### Web Scrape CTL Page Content
ctls.only <- institution.sample %>% filter(HasCTL == TRUE)
remove.elements <- c("footer", "Footer", "Navigat",
                     "navigat", "nav", "search",
                     "Nav", "Search")

ctl.labels <- c("")

key.words <- c("scholarship of teaching and learning", " SoTL ",
               "education research", "educational research",
               "teaching as research", "teaching-as-research",
               " TAR ", " DBER ", "discipline-based education research",
               "\\(SoTL\\)", "\\(TAR\\)", "\\(DBER\\)")
key.words <- paste(key.words, collapse = "|")

RemoveClassElements <- function(this.page.remove, html) {
  this.page.remove <- gsub(this.page.remove, pattern = "\\.",
                           replacement = "\\\\.")
  this.page.remove <- this.page.remove[!grepl(this.page.remove, pattern = "\\=")]
  this.page.remove <- this.page.remove[!grepl(this.page.remove, pattern = "\\\\")]
  this.page.remove <- this.page.remove[!grepl(this.page.remove, pattern = "\\/")]
  this.page.remove <- this.page.remove[!grepl(this.page.remove, pattern = "\\&")]
  this.page.remove <- this.page.remove[!grepl(this.page.remove, pattern = "^\\\n ")]
  this.page.remove <- gsub(this.page.remove, pattern = "\\#",
                           replacement = "\\\\#")
  this.page.remove <- gsub(this.page.remove, pattern = "\\*",
                           replacement = "\\\\*")
  this.page.remove <- gsub(this.page.remove, pattern = "\\/",
                           replacement = "\\\\/")
  this.page.remove <- gsub(this.page.remove, pattern = "\\=",
                           replacement = "\\\\=")
  this.page.remove <- gsub(this.page.remove, pattern = "\\(",
                           replacement = "\\\\(")
  this.page.remove <- gsub(this.page.remove, pattern = "\\)",
                           replacement = "\\\\)")
  this.page.remove <- gsub(this.page.remove, pattern = "\\:",
                           replacement = "\\\\:")
  this.page.remove <- paste(".", this.page.remove, sep = "")
  this.page.remove <- paste0(this.page.remove, collapse = ", ")
  this.page.remove <- gsub(this.page.remove, pattern = "\\.[ ]+",
                           replacement = ".")
  this.page.remove <- this.page.remove[!grepl(this.page.remove, pattern = "^\\.$")]
  if (length(this.page.remove) > 0) {
    toRemove <- html %>% html_elements(css = this.page.remove)
    xml2::xml_remove(toRemove)
  }
}

RemoveIDElements <- function(this.page.remove, html) {
  this.page.remove <- paste("div[id='", this.page.remove, sep = "")
  this.page.remove <- paste(this.page.remove, "']", sep = "")
  this.page.remove <- paste0(this.page.remove, collapse = ", ")
  toRemove <- html %>% html_elements(css = this.page.remove)
  xml2::xml_remove(toRemove)
}

CleanExtractedText <- function(my.text) {
  my.text <- unlist(strsplit(my.text, split = "\r"))
  my.text <- unlist(strsplit(my.text, split = "\n"))
  my.text <- my.text[nzchar(my.text)]
  my.text <- my.text[my.text != " "]
  my.text <- unique(my.text)
  return(my.text)
}

MergeOnlyOneSlash <- function(text1, vector.text2) {
  if (grepl(text1, pattern = "\\/$")) {
    vector.text2 <- gsub(vector.text2, pattern = "^\\/", replacement = "") 
    merge.text <- paste(text1, vector.text2, sep = "")
  } else {
    has.slash <- grepl(vector.text2, pattern = "^\\/")
    vector.text2[!has.slash] <- paste("/", vector.text2[!has.slash], sep = "")
    merge.text <- paste(text1, vector.text2, sep = "")
  }
  return(merge.text)
}

FindAllLinks <- function(my.url) {
  short.url <- gsub(my.url, pattern = "home\\.html", replacement = "")
  short.url <- gsub(short.url, pattern = "index\\.html", replacement = "")
  short.url <- gsub(short.url, pattern = "index\\.shmtl", replacement = "")
  short.url <- gsub(short.url, pattern = "index\\.php", replacement = "")
  short.url <- gsub(short.url, pattern = "index\\.htm", replacement = "")
  short.url <- gsub(short.url, pattern = "index\\.cfm", replacement = "")
  short.url <- gsub(short.url, pattern = "index\\.aspx", replacement = "")
  short.url <- gsub(short.url, pattern = "default\\.aspx", replacement = "")
  short.url <- gsub(short.url, pattern = "main\\.html", replacement = "")
  
  keep.url <- short.url
  add.url <- gsub(keep.url, pattern = "(edu).*", replacement = "\\1")
  short.url <- basename(short.url)
  
  html <- xml2::read_html(my.url)
  all.links <- html %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
  all.links <- all.links[!is.na(all.links)]
  all.links <- all.links[nchar(all.links) > 1]
  all.links <- trimws(unique(all.links))
  
  remove.these <- c("\\.css\\?", "\\.png", "\\?delta\\=", "mailto\\:", "tel\\:",
                    "^\\#", "\\.css$", "\\.pcf$", "\\.pdf$", "sharepoint\\.com",
                    "\\.ico", "docs\\.google\\.com", "zoom\\.", "qualtrics\\.com",
                    "javascript\\:void\\(0\\)", "json", "\\.xml$",
                    "twitter\\.com", "facebook\\.com", "instagram\\.com",
                    "linkedin\\.com", "youtube\\.com", "\\.mp4", "\\.mp3",
                    "\\.jpg", "\\.tif", "\\.doc", "\\.xls", "\\.ppt", "\\.txt",
                    "\\.mpeg", "\\.mpg", "\\.mov", "\\.m4a", "pinterest\\.com",
                    "snapchat\\.com", "itunes\\.apple\\.com", "tiktok\\.com",
                    "www\\.google\\.com\\/maps", "attachment", "formstack\\.com")
  remove.these <- paste(remove.these, collapse = "|")
  all.links <- all.links[str_detect(all.links, pattern = remove.these, negate = TRUE)]
  
  my.links1 <- all.links[str_which(all.links, pattern = short.url)]
  fix.these <- !grepl(my.links1, pattern = "^http|^www\\.")
  fix.links <- my.links1[fix.these]
  my.links1[fix.these] <- MergeOnlyOneSlash(add.url, fix.links)
  my.links1 <- c(my.links1, MergeOnlyOneSlash(keep.url, fix.links))
  my.links2 <- all.links[str_which(all.links, pattern = keep.url)]
  my.links <- c(my.links1, my.links2)
  
  test.this <- str_count(all.links, pattern = "\\/") == 1
  test.these <- grepl(all.links, pattern = "^\\/") & test.this
  if (grepl(short.url, pattern = "\\.edu$")) {
    if (sum(test.these) > 3) {
      my.links3 <- all.links[test.these]
      my.links3 <- c(MergeOnlyOneSlash(add.url, my.links3),
                     MergeOnlyOneSlash(keep.url, my.links3))
      my.links <- c(my.links, my.links3)
    }
  } else {
    test.this <- str_count(all.links, pattern = "\\/") == 0
    test.these <- str_count(all.links, pattern = "\\.") == 1 & test.this
    if (sum(test.these) > 3) {
      my.links3 <- all.links[test.these]
      my.links3 <- MergeOnlyOneSlash(keep.url, my.links3)
      my.links <- c(my.links, my.links3)
    }
  }
  my.links <- trimws(unique(my.links))
  fix.these <- grepl(my.links, pattern = "^www\\.")
  my.links[fix.these] <- paste("https://", my.links[fix.these], sep = "")
  fix.these <- !grepl(my.links, pattern = "^http")
  my.links[fix.these] <- paste("https://www.", my.links[fix.these], sep = "")
  
  my.links <- my.links[!grepl(my.links, pattern = "\\s")]
  my.links <- gsub(my.links, pattern = "\\/Academics\\/ATLE\\/Academics\\/ATLE\\/", replacement = "/Academics/ATLE/")
  my.links <- gsub(my.links, pattern = "\\/ctfd\\/ctfd\\/", replacement = "/ctfd/")
  my.links <- gsub(my.links, pattern = "\\/CETL\\/CETL\\/", replacement = "/CETL/")
  my.links <- trimws(unique(my.links))
  
  my.links <- my.links[!sapply(my.links, httr::http_error)]
  print("working links")
  print(my.links)
  return(my.links)
}

ScrapeHTMLBody <- function(my.url, remove.elements) {
  Sys.sleep(5)
  remove.elements <- paste0(remove.elements, collapse = "|")
  html <- xml2::read_html(my.url)
  toRemove <- html %>% rvest::html_nodes(css = "code")
  xml2::xml_remove(toRemove)
  
  this.page.classes <- html %>% html_nodes("*") %>% 
    html_attr("class") %>% unique()
  this.page.ids <- html %>% html_nodes("*") %>% 
    html_attr("id") %>% unique()
  
  this.page.remove <- this.page.classes[grep(this.page.classes, pattern = remove.elements)]
  if (length(this.page.remove) > 0) {
    RemoveClassElements(this.page.remove, html)
  }
  
  this.page.remove <- this.page.ids[grep(this.page.ids, pattern = remove.elements)]
  if (length(this.page.remove) > 0) {
    RemoveIDElements(this.page.remove, html)
  }
  
  my.text <- html %>% 
    html_elements(css = c("p, p > ul, ul > li")) %>% 
    html_text2()
  
  if (length(my.text) == 0) {
    html <- xml2::read_html(my.url)
    toRemove <- html %>% rvest::html_nodes(css = "code")
    xml2::xml_remove(toRemove)
    my.text <- html %>% 
      html_elements(css = c("p, p > ul, ul > li")) %>% 
      html_text2()
  }
  
  if (length(my.text) == 0) {
    my.text <- NA
  } else {
    my.text <- CleanExtractedText(my.text)
  }
  closeAllConnections()
  return(my.text)
}

ctl.corpus <- list()
# skip row 143
# skip row 167

all.ctls <- 1:nrow(ctls.only)
all.ctls <- setdiff(all.ctls, c(143, 167))
temp.ctls <- all.ctls
temp.ctls <- setdiff(all.ctls, 1:169)
for (i in temp.ctls) {
  Sys.sleep(5)
  print(i)
  if (!httr::http_error(ctls.only$CTL_likely[i])) {
    my.text <- ScrapeHTMLBody(ctls.only$CTL_likely[i], remove.elements)
    my.links <- FindAllLinks(ctls.only$CTL_likely[i])
  } else {
    cat(ctls.only$CTL_likely[i], "not available\n")
    closeAllConnections()
    if (!is.na(ctls.only$CTL_alt[i])) {
      if (!httr::http_error(ctls.only$CTL_alt[i])) {
        my.text <- ScrapeHTMLBody(ctls.only$CTL_alt[i], remove.elements)
        my.links <- FindAllLinks(ctls.only$CTL_alt[i])
      } else {
        cat(ctls.only$CTL_alt[i], "not available\n")
        closeAllConnections()
        next
      }
    } else {
      closeAllConnections()
      next
    }
  }
  if (length(my.links) > 0) {
    for (j in 1:length(my.links)) {
      more.text <- ScrapeHTMLBody(my.links[j], remove.elements)
      my.text <- c(my.text, more.text)
    }
  }
  my.text <- unique(my.text)
  ctl.corpus[[i]] <- my.text
  names(ctl.corpus)[[i]] <- ctls.only$inst_name[i]
  closeAllConnections()
  if (i %% 5 == 0) {
    my.file <- paste("data/all_CTL_website_text_up_to_", i, ".rds", sep = "")
    saveRDS(ctl.corpus, file = my.file)
  }
}

i <- 172
# get 172 website manually
my.text <- ScrapeHTMLBody(ctls.only$CTL_likely[i], remove.elements)
html <- xml2::read_html(ctls.only$CTL_likely[i])
my.links <- html %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
my.links <- my.links[!is.na(my.links)]
my.links <- my.links[nchar(my.links) > 1]
my.links <- trimws(unique(my.links))
my.links <- my.links[grepl(my.links, pattern = basename(ctls.only$CTL_likely[i]))]
my.links <- my.links[!grepl(my.links, pattern = "\\.jpg")]

for (j in 1:length(my.links)) {
  more.text <- ScrapeHTMLBody(my.links[j], remove.elements)
  my.text <- c(my.text, more.text)
}
my.text <- unique(my.text)
ctl.corpus[[i]] <- my.text
names(ctl.corpus)[[i]] <- ctls.only$inst_name[i]
closeAllConnections()

ctl.corpus2 <- lapply(ctl.corpus, unique)
ctl.corpus2 <- lapply(ctl.corpus2, str_squish)
ctl.corpus2 <- lapply(ctl.corpus2, trimws)

saveRDS(ctl.corpus2, file = "data/all_CTL_website_text.rds")
