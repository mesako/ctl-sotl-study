rm(list = ls())

library(tidyverse)
library(educationdata)
library(stringr)

my.folder <- "/Users/mesako/Desktop/Research/CTL_SOTL"
setwd(my.folder)

institution.list <- get_education_data(level = "college-university",
                                       source = "ipeds", topic = "directory",
                                       filters = list(year = 2020, postsec_public_active = 1,
                                                      inst_control = c(1, 2), offering_undergrad = 1,
                                                      degree_granting = 1, sector = c(1:2, 3:4, 7:8)),
                                       add_labels = TRUE, csv = FALSE)
saveRDS(institution.list, file = "ipeds_2020_institution_list.rds")
institution.list <- readRDS(file = "data/ipeds_2020_institution_list.rds")

### Remove Columns from Institution List
remove.these <- c("year", "address", "zip", "phone_number", "fips", "chief_admin_name",
                  "chief_admin_title", "ein", "opeid", "date_closed", "duns",
                  "inst_status", "newid", "year_deleted", "degree_granting", "open_public",
                  "reporting_method", "url_fin_aid", "url_application", "cbsa",
                  "primarily_postsecondary", "currently_active_ipeds", "offering_undergrad",
                  "postsec_public_active", "csa", "necta", "url_veterans", "url_athletes",
                  "url_netprice", "url_disability_services", "longitude", "latitude",
                  "comparison_group", "comparison_group_custom", "cbsa_type",
                  "county_fips", "congress_district_id", "postsec_public_active_title_iv",
                  "unitid")
institution.list <- institution.list %>% select(-all_of(remove.these))

remove.flag <- c("*_2000")
remove.these <- grep(remove.flag, colnames(institution.list))
institution.list <- institution.list %>% select(-all_of(remove.these))

remove.flag <- c("*_2010")
remove.these <- grep(remove.flag, colnames(institution.list))
institution.list <- institution.list %>% select(-all_of(remove.these))

remove.flag <- c("*_2015")
remove.these <- grep(remove.flag, colnames(institution.list))
institution.list <- institution.list %>% select(-all_of(remove.these))

remove.flag <- c("*_2021")
remove.these <- grep(remove.flag, colnames(institution.list))
institution.list <- institution.list %>% select(-all_of(remove.these))

dim(institution.list)

### Recode Logical Variables in Institution List
# hbcu, hospital, medical_degree, tribal_college, inst_system_flag, 
# offering_grad, land_grant
logical.cols <- c("hbcu", "hospital", "medical_degree", "tribal_college",
                  "inst_system_flag", "offering_grad", "land_grant")
for (x in logical.cols) {
  na.vals <- !grepl(institution.list[, x], pattern = "Yes$|No$")
  if (sum(na.vals) > 0) {
    institution.list[, x][na.vals] <- NA
  }
  temp.vals <- institution.list[, x]
  temp.vals <- as.character(temp.vals)
  temp.vals <- ifelse(temp.vals == "Yes", TRUE, FALSE)
  institution.list[, x] <- temp.vals
}

### Recode Factor Variables in Institution List
# state_abbr, city, county_name, inst_system_name
factor.cols <- c("state_abbr", "city", "county_name", "inst_system_name")
for (x in factor.cols) {
  institution.list[, x] <- as.factor(institution.list[, x])
}

### Clean Factor Variables in Institution List
# inst_system_name, region
institution.list$inst_system_name[institution.list$inst_system_name == -1] <- NA
institution.list$inst_system_name[institution.list$inst_system_name == -2] <- NA

institution.list$region <- gsub(":.*", "", institution.list$region)
institution.list$region <- as.factor(institution.list$region)

saveRDS(institution.list, file = "ipeds_2020_institution_list_cleaned.rds")
write.csv(institution.list, file = "ipeds_2020_institution_list_cleaned.csv",
          row.names = FALSE)
institution.list <- readRDS(file = "data/ipeds_2020_institution_list_cleaned.rds")

### Sample Institutions for Analysis
# randomize list order and add ID numbers
set.seed(10)
institution.list <- institution.list[sample(nrow(institution.list)), ]
institution.list <- institution.list[sample(nrow(institution.list)), ]
institution.list <- institution.list[sample(nrow(institution.list)), ]
institution.list$ID <- 1:nrow(institution.list)
rownames(institution.list) <- NULL
institution.list <- institution.list %>% relocate(ID)

### Randomly Sample Institutions from Institution List
# cc_basic_2018
summary(institution.list$cc_basic_2018)
levels(institution.list$cc_basic_2018)
cc_class <- as.character(institution.list$cc_basic_2018)
cc_class <- gsub(cc_class, pattern = "[[:punct:]]", replacement = " ")
cc_class <- str_squish(trimws(cc_class))
sort(table(cc_class))

cc_class[grep(cc_class, pattern = "Not applicable")] <- NA
cc_class[grep(cc_class, pattern = "Special focus")] <- "Special focus institution"
cc_class[grep(cc_class, pattern = "Tribal colleges")] <- "Special focus institution"

cc_class[grep(cc_class, pattern = "Associate s colleges Mixed transfer vocational focus")] <- "Associate s colleges Mixed transfer vocational focus"
cc_class[grep(cc_class, pattern = "Associate s colleges Transfer focused")] <- "Associate s colleges Transfer focused"
cc_class[grep(cc_class, pattern = "Associate s colleges Vocational focused")] <- "Associate s colleges Vocational focused"

cc_class <- as.factor(cc_class)

institution.list <- institution.list %>% mutate(cc_class)

institution.sample <- institution.list %>% filter(!is.na(cc_class))
institution.sample <- institution.sample %>%
  group_by(cc_class) %>%
  sample_n(size = min(table(institution.sample$cc_class)))
institution.sample <- institution.sample %>% ungroup()

geo_region <- as.character(institution.sample$region)
geo_region[grep(geo_region, pattern = "US service schools")] <- NA
geo_region[grep(geo_region, pattern = "Outlying areas")] <- NA
geo_region <- as.factor(geo_region)

institution.sample <- institution.sample %>% mutate(geo_region)

institution.sample <- institution.sample %>% filter(!is.na(geo_region))
institution.sample <- institution.sample %>%
  group_by(geo_region) %>%
  sample_n(size = min(table(institution.sample$geo_region)))
institution.sample <- institution.sample %>% ungroup()

institution.sample <- as.data.frame(institution.sample)

### Clean up Factor Columns
factor.cols <- unname(unlist(lapply(institution.sample, is.factor)))
factor.cols <- colnames(institution.sample)[factor.cols]

for (x in factor.cols) {
  temp.col <- as.character(institution.sample[, x])
  temp.col <- as.factor(temp.col)
  institution.sample[, x] <- temp.col
}

dim(institution.sample)
summary(institution.sample)

saveRDS(institution.sample, file = "institution_sample.rds")
write.csv(institution.sample, file = "institution_sample.csv",
          row.names = FALSE)
