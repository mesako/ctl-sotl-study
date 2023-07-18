rm(list = ls())

library(tidyverse)
library(ggplot2)
library(see)
library(reshape2)
library(forcats)

my.folder <- "/Users/mesako/Desktop/Research/CTL_SOTL"
setwd(my.folder)

### Load Final Data
final.data <- readRDS(file = "data/final_institution_data.rds")

# removed closed university
final.data <- final.data[!grepl(final.data$inst_name, pattern = "Cardinal"), ]

final.data$inst_size <- final.data$inst_size %>% 
  dplyr::recode("Under 1" = "Under 1K",
                "1" = "1K", "5" = "5K",
                "10" = "10K", "20" = "20K")

my.plot <- ggplot(final.data, aes(x = inst_size, fill = inst_size)) + geom_bar() + 
  facet_wrap(~ cc_class, ncol = 5, labeller = labeller(cc_class = label_wrap_gen(25)))
my.plot <- my.plot + guides(fill = "none") + ylab("Count") + 
  xlab("Size of Institution") + scale_fill_okabeito()
print(my.plot)
ggsave(filename = "results/CTL_inst_size_by_cc_class.png", dpi = 600,
       width = 10, height = 6, units = "in")

my.plot <- ggplot(final.data, aes(x = inst_size, fill = inst_size)) + geom_bar() + 
  facet_wrap(~ geo_region, ncol = 5, labeller = labeller(cc_class = label_wrap_gen(25)))
my.plot <- my.plot + guides(fill = "none") + ylab("Count") + 
  xlab("Size of Institution") + scale_fill_okabeito()
print(my.plot)
ggsave(filename = "results/CTL_inst_size_by_geo_region.png", dpi = 600,
       width = 10, height = 4, units = "in")

final.data2 <- final.data
final.data2$geo_region <- str_wrap(final.data2$geo_region, width = 5)

my.plot <- ggplot(final.data2, aes(x = geo_region, fill = geo_region)) + geom_bar() + 
  facet_wrap(~ cc_class, ncol = 3, labeller = labeller(cc_class = label_wrap_gen(70)))
my.plot <- my.plot + guides(fill = "none") + ylab("Count") + 
  xlab("Geographic Region of Institution") + scale_fill_okabeito()
print(my.plot)
ggsave(filename = "results/CTL_geo_region_by_cc_class.png", dpi = 600,
       width = 15, height = 10, units = "in")

### Process and Visualize CTL Presence
ctl.vs.sector <- as.data.frame(table(final.data$HasCTL, final.data$sector))
ctl.vs.geo.region <- as.data.frame(table(final.data$HasCTL, final.data$geo_region))
ctl.vs.inst.size <- as.data.frame(table(final.data$HasCTL, final.data$inst_size))
ctl.vs.cc.class <- as.data.frame(table(final.data$HasCTL, final.data$cc_class))

ctl.vs.sector <- ctl.vs.sector %>% group_by(Var2) %>% mutate(Percent = Freq / sum(Freq))
ctl.vs.geo.region <- ctl.vs.geo.region %>% group_by(Var2) %>% mutate(Percent = Freq / sum(Freq))
ctl.vs.inst.size <- ctl.vs.inst.size %>% group_by(Var2) %>% mutate(Percent = Freq / sum(Freq))
ctl.vs.cc.class <- ctl.vs.cc.class %>% group_by(Var2) %>% mutate(Percent = Freq / sum(Freq))

ctl.vs.sector$Var2 <- str_wrap(ctl.vs.sector$Var2, width = 30)
ggplot(ctl.vs.sector, aes(x = Var2, y = Percent, fill = Var1)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Institution Sector") + ylab("Percent of Institutions") +
  labs(fill = "Has a CTL") + scale_y_continuous(labels = scales::percent) +
  scale_fill_okabeito()
ggsave(filename = "results/CTL_presence_by_sector.png", dpi = 600,
       width = 6, height = 4, units = "in")

ctl.vs.geo.region$Var2 <- str_wrap(ctl.vs.geo.region$Var2, width = 10)
ggplot(ctl.vs.geo.region, aes(x = Var2, y = Percent, fill = Var1)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Institution Geographic Region") + ylab("Percent of Institutions") +
  labs(fill = "Has a CTL") + scale_y_continuous(labels = scales::percent) +
  scale_fill_okabeito()
ggsave(filename = "results/CTL_presence_by_geo_region.png", dpi = 600,
       width = 8, height = 4, units = "in")

set.seed(10)
ctl.vs.geo.region <- ctl.vs.geo.region %>% filter(Var1 == TRUE)
ggplot(ctl.vs.geo.region, aes(x = "", y = Percent)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter(aes(color = Var2), 
                                                 height = 0.01, width = 0.05) +
  labs(color = "Institution\nGeographic Region") + 
  ylab("Percent of Institutions Having a CTL") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_okabeito() + xlab("")
ggsave(filename = "results/CTL_presence_by_geo_region_boxplot.png", dpi = 600,
       width = 4, height = 4, units = "in")

ggplot(ctl.vs.inst.size, aes(x = Var2, y = Percent, fill = Var1)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Institution Size") + ylab("Percent of Institutions") +
  labs(fill = "Has a CTL") + scale_y_continuous(labels = scales::percent) +
  scale_fill_okabeito()
ggsave(filename = "results/CTL_presence_by_inst_size.png", dpi = 600,
       width = 6, height = 4, units = "in")

ctl.vs.cc.class$Var2 <- str_wrap(ctl.vs.cc.class$Var2, width = 30)
ggplot(ctl.vs.cc.class, aes(x = Var2, y = Percent, fill = Var1)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Institution Carnegie Classification") + ylab("Percent of Institutions") +
  labs(fill = "Has a CTL") + scale_y_continuous(labels = scales::percent) +
  coord_flip() + scale_fill_okabeito()
ggsave(filename = "results/CTL_presence_by_cc_class.png", dpi = 600,
       width = 7, height = 6, units = "in")

### Process and Visualize SoTL Mentions at CTLs
CTLs.only <- final.data %>% filter(HasCTL == TRUE)
CTLs.only <- CTLs.only %>% mutate(SoTLMention = SoTL | DBER | TAR | EdRes)

by.sector <- as.data.frame(table(CTLs.only$SoTLMention, CTLs.only$sector))
by.geo.region <- as.data.frame(table(CTLs.only$SoTLMention, CTLs.only$geo_region))
by.inst.size <- as.data.frame(table(CTLs.only$SoTLMention, CTLs.only$inst_size))
by.cc.class <- as.data.frame(table(CTLs.only$SoTLMention, CTLs.only$cc_class))

by.sector <- by.sector %>% group_by(Var2) %>% mutate(Percent = Freq / sum(Freq))
by.geo.region <- by.geo.region %>% group_by(Var2) %>% mutate(Percent = Freq / sum(Freq))
by.inst.size <- by.inst.size %>% group_by(Var2) %>% mutate(Percent = Freq / sum(Freq))
by.cc.class <- by.cc.class %>% group_by(Var2) %>% mutate(Percent = Freq / sum(Freq))

by.sector$Var2 <- str_wrap(by.sector$Var2, width = 30)
ggplot(by.sector, aes(x = Var2, y = Percent, fill = Var1)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Institution Sector") + ylab("Percent of Institutions") +
  labs(fill = "Mentions\nSoTL at CTL") + scale_y_continuous(labels = scales::percent) +
  scale_fill_okabeito()
ggsave(filename = "results/CTL_SoTL_presence_by_sector.png", dpi = 600,
       width = 6, height = 4, units = "in")

by.geo.region$Var2 <- str_wrap(by.geo.region$Var2, width = 30)
ggplot(by.geo.region, aes(x = Var2, y = Percent, fill = Var1)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Institution Geographic Region") + ylab("Percent of Institutions") +
  labs(fill = "Mentions\nSoTL at CTL") + scale_y_continuous(labels = scales::percent) +
  scale_fill_okabeito()
ggsave(filename = "results/CTL_SoTL_presence_by_geo_region.png", dpi = 600,
       width = 8, height = 4, units = "in")

set.seed(5)
by.geo.region2 <- by.geo.region %>% filter(Var1 == TRUE)
ggplot(by.geo.region2, aes(x = "", y = Percent)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter(aes(color = Var2), 
                                                 height = 0.01, width = 0.05) +
  labs(color = "Institution\nGeographic Region") + 
  ylab("Percent of Institutions Mentioning SoTL at CTL") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_okabeito() + xlab("")
ggsave(filename = "results/CTL_SoTL_presence_by_geo_region_boxplot.png", dpi = 600,
       width = 4, height = 4, units = "in")

ggplot(by.inst.size, aes(x = Var2, y = Percent, fill = Var1)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Institution Size") + ylab("Percent of Institutions") +
  labs(fill = "Mentions\nSoTL at CTL") + scale_y_continuous(labels = scales::percent) +
  scale_fill_okabeito()
ggsave(filename = "results/CTL_SoTL_presence_by_inst_size.png", dpi = 600,
       width = 6, height = 4, units = "in")

by.cc.class$Var2 <- str_wrap(by.cc.class$Var2, width = 30)
ggplot(by.cc.class, aes(x = Var2, y = Percent, fill = Var1)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Institution Carnegie Classification") + ylab("Percent of Institutions") +
  labs(fill = "Mentions\nSoTL at CTL") + scale_y_continuous(labels = scales::percent) +
  coord_flip() + scale_fill_okabeito()
ggsave(filename = "results/CTL_SoTL_presence_by_cc_class.png", dpi = 600,
       width = 7, height = 6, units = "in")

### Process and Visualize SoTL Models at CTLs
### Load Data
sotl.data <- read.csv(file = "data/ctl_sotl_models_data_final.csv")
sotl.data$SoTL.Model <- NULL
sotl.data$Notes <- NULL
sotl.data$Model.Number <- NULL
sotl.data$Blanks <- NULL

summary.data <- colSums(sotl.data[, sapply(sotl.data, is.logical)], na.rm = TRUE)
save.this <- summary.data
summary.data <- summary.data / nrow(sotl.data)
summary.data <- cbind(Model = names(summary.data), Freq = unname(summary.data))
summary.data <- as.data.frame(summary.data)
summary.data$Freq <- as.numeric(summary.data$Freq)
summary.data$Model <- gsub(summary.data$Model,
                           pattern = "\\.",
                           replacement = " ")

summary.data$Model <- gsub(summary.data$Model,
                           pattern = "One on One",
                           replacement = "One-on-One")

summary.data$Model <- gsub(summary.data$Model,
                           pattern = "Train the Trainer",
                           replacement = "Train-the-Trainer")

summary.data$Model <- gsub(summary.data$Model,
                           pattern = "Reference",
                           replacement = "Reference (no actual program offering)")

ggplot(summary.data, aes(x = reorder(Model, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "#F5C710") + xlab("SoTL Program Model") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent of CTLs with this Model") + coord_flip()
ggsave(filename = "results/CTL_SoTL_models_freq.png", dpi = 600,
       width = 6, height = 3, units = "in")

# Merge SoTL Data and Institutional Data
present.ind <- match(sotl.data$docname, final.data$inst_name)
missing.ind <- setdiff(1:nrow(final.data), present.ind)
sotl.data2 <- sotl.data
colnames(sotl.data2) <- gsub(colnames(sotl.data2),
                             pattern = "\\.", replacement = "")
sotl.data2 <- sotl.data2[, 3:ncol(sotl.data2)]

final.data2 <- final.data[missing.ind, ]
add.in <- matrix(NA, nrow = length(missing.ind), ncol(sotl.data2))
add.in <- data.frame(add.in)
colnames(add.in) <- colnames(sotl.data2)
final.data2 <- cbind(final.data2, add.in)

sotl.data2 <- cbind(final.data[present.ind, ], sotl.data2)

final.sotl.data <- rbind(final.data2, sotl.data2)
final.sotl.data <- final.sotl.data[order(final.sotl.data$ID), ]
saveRDS(final.sotl.data, file = "data/final_institution_sotl_data.rds")

# annonymize data and save
deid.data <- final.sotl.data
deid.data$ID <- 1:nrow(deid.data)
deid.data$inst_name <- NULL
deid.data$inst_system_name <- NULL
deid.data$CTL_likely <- NULL
deid.data$CTL_alt <- NULL
write.csv(deid.data, file = "data/de-identified_institutional_data.csv", row.names = FALSE)

ctls.by.sector.sotl.summary <- final.sotl.data %>% 
  filter(HasCTL == TRUE) %>% 
  group_by(sector) %>% 
  summarise(across(where(is.logical), ~ (sum(., na.rm = TRUE) / n()))) %>% 
  select(c(sector, Reference:LocalSoTLShowcases))

ctls.by.control.sotl.summary <- final.sotl.data %>% 
  filter(HasCTL == TRUE) %>% 
  group_by(inst_control) %>% 
  summarise(across(where(is.logical), ~ (sum(., na.rm = TRUE) / n()))) %>% 
  select(c(inst_control, Reference:LocalSoTLShowcases))

ctls.by.geo.region.sotl.summary <- final.sotl.data %>% 
  filter(HasCTL == TRUE) %>% 
  group_by(geo_region) %>% 
  summarise(across(where(is.logical), ~ (sum(., na.rm = TRUE) / n()))) %>% 
  select(c(geo_region, Reference:LocalSoTLShowcases))

ctls.by.cc.class.sotl.summary <- final.sotl.data %>% 
  filter(HasCTL == TRUE) %>% 
  group_by(cc_class) %>% 
  summarise(across(where(is.logical), ~ (sum(., na.rm = TRUE) / n()))) %>% 
  select(c(cc_class, Reference:LocalSoTLShowcases))

temp <- reshape2::melt(ctls.by.sector.sotl.summary)
save.names <- levels(temp$variable)
orig.names <- save.names
save.names <- gsub(save.names, pattern = "([[:lower:]])([[:upper:]][[:lower:]])", replacement = "\\1 \\2")
save.names <- gsub(save.names, pattern = "SoTL", replacement = " SoTL ")
save.names <- gsub(save.names, pattern = "CTL", replacement = " CTL ")
save.names <- str_squish(trimws(save.names))
save.names <- gsub(save.names, pattern = "Incentivesfor ", replacement = "Incentives for ")
save.names <- gsub(save.names, pattern = "Howto", replacement = "How to")
save.names <- gsub(save.names, pattern = "Awardsor ", replacement = "Awards or ")
save.names <- gsub(save.names, pattern = "Oneon One", replacement = "One-on-One")
save.names <- gsub(save.names, pattern = "Trainthe Trainer", replacement = "Train-the-Trainer")
save.names <- gsub(save.names, pattern = "Partnersin", replacement = "Partners in")
save.names <- gsub(save.names, pattern = "Studentsas", replacement = "Students as")
convert.names <- as.list(save.names)
names(convert.names) <- orig.names
temp$variable <- recode(temp$variable, !!!convert.names)

keep.these <- temp %>% group_by(variable) %>% summarize(check = sum(value)) %>% 
  filter(check > 0) %>% select(variable)
temp <- temp %>% filter(variable %in% as.character(keep.these$variable))
temp$sector <- str_wrap(temp$sector, width = 30)
my.plot <- ggplot(temp, aes(x = sector, y = value, fill = sector)) + 
  geom_bar(stat = "identity") + facet_wrap(. ~ variable) + xlab("Institution Sector") +
  guides(fill = "none") + scale_y_continuous(labels = scales::percent) +
  ylab("Frequency of Model at CTLs within Sector") + scale_fill_okabeito() + coord_flip()
print(my.plot)
ggsave(filename = "results/CTL_sotl_model_by_sector.png", dpi = 600,
       width = 10, height = 6, units = "in")

temp <- reshape2::melt(ctls.by.control.sotl.summary)
temp$variable <- recode(temp$variable, !!!convert.names)
keep.these <- temp %>% group_by(variable) %>% summarize(check = sum(value)) %>% 
  filter(check > 0) %>% select(variable)
temp <- temp %>% filter(variable %in% as.character(keep.these$variable))
temp$inst_control <- factor(temp$inst_control)
temp$inst_control <- recode(temp$inst_control, "Private not-for-profit" = "Private")
temp$inst_control <- relevel(temp$inst_control, ref = "Public")
my.plot <- ggplot(temp, aes(x = inst_control, y = value, fill = inst_control)) + 
  geom_bar(stat = "identity") + facet_wrap(. ~ variable) + xlab("Institution Control") +
  guides(fill = "none") + scale_y_continuous(labels = scales::percent) +
  ylab("Frequency of Model at CTLs within Institution Control") + scale_fill_okabeito() + coord_flip()
print(my.plot)
ggsave(filename = "results/CTL_sotl_model_by_control.png", dpi = 600,
       width = 11, height = 6, units = "in")

temp <- reshape2::melt(ctls.by.geo.region.sotl.summary)
temp$variable <- recode(temp$variable, !!!convert.names)
keep.these <- temp %>% group_by(variable) %>% summarize(check = sum(value)) %>% 
  filter(check > 0) %>% select(variable)
temp <- temp %>% filter(variable %in% as.character(keep.these$variable))
temp$geo_region <- str_wrap(temp$geo_region, width = 30)
my.plot <- ggplot(temp, aes(x = geo_region, y = value, fill = geo_region)) + 
  geom_bar(stat = "identity") + facet_wrap(. ~ variable) + xlab("Institution Geographic Region") +
  guides(fill = "none") + scale_y_continuous(labels = scales::percent) +
  ylab("Frequency of Model at CTLs within Geographic Region") + scale_fill_okabeito() + coord_flip()
print(my.plot)
ggsave(filename = "results/CTL_sotl_model_by_geo_region.png", dpi = 600,
       width = 11, height = 6, units = "in")

temp <- reshape2::melt(ctls.by.cc.class.sotl.summary)
keep.these <- temp %>% group_by(variable) %>% summarize(check = sum(value)) %>% 
  filter(check > 0) %>% select(variable)
temp <- temp %>% filter(variable %in% as.character(keep.these$variable))
temp$variable <- recode(temp$variable, !!!convert.names)
my.plot <- ggplot(temp, aes(x = cc_class, y = value)) + 
  geom_bar(stat = "identity", fill = "#F5C710") + facet_wrap(. ~ variable) + xlab("Institution Carnegie Classification") +
  guides(fill = "none") + scale_y_continuous(labels = scales::percent) +
  ylab("Frequency of Model at CTLs within Carnegie Classification") + coord_flip()
print(my.plot)
ggsave(filename = "results/CTL_sotl_model_by_cc_class.png", dpi = 600,
       width = 11, height = 6, units = "in")

# Add CC Simplified
final.sotl.data <- cbind(final.sotl.data, cc_simplified = NA)
final.sotl.data <- final.sotl.data %>% relocate(cc_simplified, .after = cc_class)
final.sotl.data$cc_simplified[grepl(final.sotl.data$cc_class, pattern = "^Masters")] <- "Masters colleges"
final.sotl.data$cc_simplified[grepl(final.sotl.data$cc_class, pattern = "^Doctoral")] <- "Doctoral universities"
final.sotl.data$cc_simplified[grepl(final.sotl.data$cc_class, pattern = "^Associates")] <- "Associates colleges"
final.sotl.data$cc_simplified[grepl(final.sotl.data$cc_class, pattern = "^Baccalaureate")] <- "Baccalaureate colleges"
final.sotl.data$cc_simplified[grepl(final.sotl.data$cc_class, pattern = "^Special")] <- "Special focus institution"
final.sotl.data$cc_simplified <- factor(final.sotl.data$cc_simplified,
                                        levels = c("Associates colleges", "Baccalaureate colleges", "Masters colleges",
                                                   "Doctoral universities", "Special focus institution"))

ctls.by.cc.simple.sotl.summary <- final.sotl.data %>% 
  filter(HasCTL == TRUE) %>% 
  group_by(cc_simplified) %>% 
  summarise(across(where(is.logical), ~ (sum(., na.rm = TRUE) / n()))) %>% 
  select(c(cc_simplified, Reference:LocalSoTLShowcases))

temp <- reshape2::melt(ctls.by.cc.simple.sotl.summary)
keep.these <- temp %>% group_by(variable) %>% summarize(check = sum(value)) %>% 
  filter(check > 0) %>% select(variable)
temp <- temp %>% filter(variable %in% as.character(keep.these$variable))
temp$variable <- recode(temp$variable, !!!convert.names)
my.plot <- ggplot(temp, aes(x = cc_simplified, y = value, fill = cc_simplified)) + 
  geom_bar(stat = "identity") + facet_wrap(. ~ variable, ncol = 3) + 
  xlab("Institution Simplified Carnegie Classification") +
  guides(fill = "none") + scale_y_continuous(labels = scales::percent) +
  ylab("Frequency of Model at CTLs within Simplified Carnegie Classification") + scale_fill_okabeito() + coord_flip()
print(my.plot)
ggsave(filename = "results/CTL_sotl_model_by_cc_simple.png", dpi = 600,
       width = 11, height = 8, units = "in")

### Classification Model Predicting SoTL Engagement? Predicting CTL Presence?
final.sotl.data$HasCTL
any.model <- final.sotl.data %>% select(CuratedResourceCollections:LocalSoTLShowcases)
any.model <- unname(rowSums(any.model, na.rm = TRUE))
any.model <- any.model > 0
final.sotl.data <- cbind(final.sotl.data, HasSoTL = any.model)
final.sotl.data <- final.sotl.data %>% relocate(HasSoTL, .after = HasCTL)

temp.glm.data <- final.sotl.data
temp.glm.data$sector <- factor(temp.glm.data$sector, 
                               levels = c("Public two-year", "Public four-year or above",
                                          "Private not-for-profit four-year or above"))
temp.glm.data$geo_region <- factor(temp.glm.data$geo_region)
temp.glm.data$inst_control <- factor(temp.glm.data$inst_control)
temp.glm.data$inst_control <- recode(temp.glm.data$inst_control, "Private not-for-profit" = "Private")
temp.glm.data$inst_control <- relevel(temp.glm.data$inst_control, ref = "Public")

temp.glm.data$locale <- NA
temp.glm.data$locale[grepl(temp.glm.data$urban_centric_locale, pattern = "^Town")] <- "Town"
temp.glm.data$locale[grepl(temp.glm.data$urban_centric_locale, pattern = "^Rural")] <- "Rural"
temp.glm.data$locale[grepl(temp.glm.data$urban_centric_locale, pattern = "^City")] <- "City"
temp.glm.data$locale[grepl(temp.glm.data$urban_centric_locale, pattern = "^Suburb")] <- "Suburb"
temp.glm.data$locale <- factor(temp.glm.data$locale)
temp.glm.data$locale <- relevel(temp.glm.data$locale, ref = "Rural")

ctl.glm <- glm(HasCTL ~ inst_control + cc_simplified + geo_region + inst_system_flag + inst_size + locale, data = temp.glm.data)
summary(ctl.glm)
write.csv(summary(ctl.glm)[[12]], file = "results/ctl_glm_model_summary.csv")
table(ctl.glm$fitted.values >= 0.5, ctl.glm$y)
# (73 + 145) / 298 correct = 73.2% accurate

sotl.glm <- glm(HasSoTL ~ inst_control + cc_simplified + geo_region + inst_system_flag + inst_size + locale, data = temp.glm.data)
summary(sotl.glm)
write.csv(summary(sotl.glm)[[12]], file = "results/sotl_glm_model_summary.csv")
table(sotl.glm$fitted.values >= 0.5, sotl.glm$y)
# (239 + 8) / 298 correct = 82.9% accurate

model.counts <- final.sotl.data %>% filter(HasSoTL == TRUE) %>% select(CuratedResourceCollections:LocalSoTLShowcases)
model.counts <- unname(rowSums(model.counts, na.rm = TRUE))
model.count.freq <- table(model.counts) / length(model.counts)

scales::percent(as.vector(unname(model.count.freq)))
