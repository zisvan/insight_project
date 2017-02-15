setwd("/Users/zisvan/insight_project/")

library(dplyr)
library(ggplot2)

keep_schools <- read.csv("../college_data/keep_schools.csv")

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

###########################################################################
## Below this line is play - not working entirely, and calls objects from 
## workspace that are not explicitly included here

p <- ggplot(data = keep_schools, aes(y = SAT_AVG, x = fam_bracket_f, color = earnings)) +
  geom_boxplot(na.rm = TRUE)

p2 <- ggplot(data = subset(keep_schools, PCIP14 > 0), aes(y = PCIP14, x = fam_bracket_f, color = earnings)) 
p2 + geom_boxplot()

##
ethnicity <- c("UGDS_WHITE","UGDS_BLACK", "UGDS_HISP",  "UGDS_ASIAN", "UGDS_AIAN",
               "UGDS_NHPI", "UGDS_2MOR", "UGDS_NRA","UGDS_UNKN")

by_ethnicity <- keep_schools %>%
  rowwise() %>%
  mutate(non_white = sum(UGDS_BLACK, UGDS_HISP, UGDS_ASIAN, UGDS_AIAN, UGDS_NHPI),
         minority_ratio = non_white/(UGDS_WHITE + non_white)) 
