setwd("/Users/zisvan/insight_project/")

library(dplyr)
library(ggplot2)

df <- read.csv("keep_schools.csv")

admissions <- c("ADM_RATE", "ADM_RATE_ALL",
                "SATVR25","SATVR75", "SATMT25", "SATMT75","SATWR25", "SATWR75",
                "SATVRMID", "SATMTMID", "SATWRMID",
                "ACTCM25", "ACTCM75", "ACTEN25", "ACTEN75",
                "ACTMT25", "ACTMT75", "ACTWR25","ACTWR75", "ACTCMMID",
                "ACTENMID", "ACTMTMID", "ACTWRMID", "SAT_AVG",
                "earnings", "fam_bracket_f")

colAdm <- match(admissions, names(data))

by_admission <- df %>%
  select(colAdm) %>%
  #filter(complete.cases(.)) %>%
  group_by(earnings, fam_bracket_f) %>%
  summarize(count = n())


p <- ggplot(data = keep_schools, aes(y = SAT_AVG, x = fam_bracket_f, color = earnings)) +
  geom_boxplot(na.rm = TRUE)

p2 <- ggplot(data = keep_schools, aes(y = SATMTMID, x = fam_bracket_f, color = earnings)) 
+ geom_boxplot(na.rm = TRUE)
