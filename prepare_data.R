setwd("/Users/zisvan/insight_project/")

library(dplyr)
library(ggplot2)
library(MASS)
library(leaps)
library(relaimpo)

# files <- list.files(path = "../college_data/", pattern = "*.csv")

# get one file for now from 11-12
data <- read.csv("../college_data/MERGED2011_12_PP.csv", na.strings = "NULL")

gradVars <- c("C150_4", #completion rate
              "COSTT4_A", #average cost of attendance academic year
              "TUITFTE", #Net tuition revenue per full-time equivalent student
              "INEXPFTE", #Instructional expenditures per full-time equivalent student
              "ADM_RATE", #Admission rate overall
              "SAT_AVG", #Average SAT equivalent score of students admitted
              "UGDS", #Enrollment of all undergraduate students
              "AVGFACSAL", #Average faculty salary
              "PFTFAC", #Ratio of full-time faculty
              "DEBT_MDN", #Median debt
              "PAR_ED_PCT_1STGEN", #percentage of first-gen students
              "RET_FT4" #retention rate after one year
)

colGradVars <- match(gradVars, names(data))

sample <- data %>%
  filter(ICLEVEL == 1, # four year
         DISTANCEONLY == 0 #not online-only
         ) %>%
  select(colGradVars) %>%
  filter(complete.cases(.))

# convert factors to numeric
sample$DEBT_MDN <- as.numeric(as.character(sample$DEBT_MDN))
sample$PAR_ED_PCT_1STGEN <- as.numeric(as.character(sample$PAR_ED_PCT_1STGEN))

#remove NA's
sample <- sample[complete.cases(sample),]

# Determine what constitutes a good graduation rate
meanRate <- mean(sample$C150_4)
stdRate <- sqrt(var(sample$C150_4))
# medianRate <- median(sample$C150_4)
sample <- mutate(sample, gradRate = ifelse(C150_4 > (meanRate - stdRate), 
                                           (ifelse(C150_4 > (meanRate + stdRate), 
                                                   "Good", "OK")), "Bad"))


# Determine what variables are strongly related

lin <- lm(C150_4 ~ COSTT4_A + TUITFTE + INEXPFTE + ADM_RATE + SAT_AVG +
            UGDS + AVGFACSAL + PFTFAC + DEBT_MDN + PAR_ED_PCT_1STGEN + RET_FT4,
          data = sample)
step <- stepAIC(lin, direction="both")


# ALternatively
attach(sample)
leaps <- regsubsets(C150_4 ~ COSTT4_A + TUITFTE + INEXPFTE + ADM_RATE + SAT_AVG +
            UGDS + AVGFACSAL + PFTFAC + DEBT_MDN + PAR_ED_PCT_1STGEN + RET_FT4,
          data = sample, nbest = 5)

# Relative importance
# Calculate Relative Importance for Each Predictor
calc.relimp(lin, type="lmg", rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(lin, b = 100, type = "lmg", rank = TRUE, 
                    diff = FALSE, rela = TRUE)
# booteval.relimp(boot) # print result
# plot(booteval.relimp(boot,sort=TRUE)) # plot result
