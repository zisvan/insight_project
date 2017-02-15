setwd("/Users/zisvan/insight_project/")

library(dplyr)
library(ggplot2)
library(class)


data <- read.csv("../college_data/MERGED2011_12_PP.csv", na.strings = "NULL")

gradVars <- c("C150_4", #completion rate
              "COSTT4_A", #average cost of attendance academic year
              #"COSTT4_P", #average cost of attendance program year
              "TUITFTE", #Net tuition revenue per full-time equivalent student
              "INEXPFTE", #Instructional expenditures per full-time equivalent student
              "ADM_RATE", #Admission rate overall
              "SAT_AVG", #Average SAT equivalent score of students admitted
              "UGDS", #Enrollment of all undergraduate students
              "AVGFACSAL", #Average faculty salary
              "PFTFAC", #Ratio of full-time faculty
              "DEBT_MDN", #Median debt
              "PAR_ED_PCT_1STGEN" #percentage of first-gen students
)

colGradVars <- match(gradVars, names(data))

sample <- data %>%
  filter(ICLEVEL == 1) %>%
  select(colGradVars)

# convert factors to numeric
sample$DEBT_MDN <- as.numeric(as.character(sample$DEBT_MDN))
sample$PAR_ED_PCT_1STGEN <- as.numeric(as.character(sample$PAR_ED_PCT_1STGEN))

#remove NA's
sample <- sample[complete.cases(sample),]

# Determine what constitutes a good graduation rate
meanRate <- mean(sample$C150_4)
stdRate <- sqrt(var(sample$C150_4))

sample <- mutate(sample, gradRate = ifelse(C150_4 > (meanRate - stdRate), 
                                           (ifelse(C150_4 > (meanRate + stdRate), 
                                                   "Good", "OK")), "Bad"))

# Sample into training and test sets
set.seed(123)
trn <- sample(seq_len(nrow(sample)), size = floor(.75 * nrow(sample)))

train <- sample[trn, ]
test <- sample[-trn, ]

# Run knn
kn <- floor(sqrt(nrow(train)))
pred.knn <- knn(train = train[, 2:11], test = test[, 2:11], cl = train[, 12],
                   k = kn)

