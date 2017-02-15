setwd("/Users/zisvan/insight_project/")

library(dplyr)
library(ggplot2)
library(class)

keep_schools <- read.csv("../college_data/keep_schools.csv", na.strings = "NULL", 
                         stringsAsFactors =  FALSE)

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
              "earnings" #Earnings Above or Below
)

colGradVars <- match(gradVars, names(keep_schools))

knnSample <- select(keep_schools, colGradVars)

#convert all but earnings to numeric (they ae already chr)
numeric.num <- match(gradVars[1:length(gradVars)-1], names(knnSample))
knnSample[numeric.num] <- sapply(knnSample[numeric.num], as.numeric)

#remove NAs from converting
knnSample <- knnSample[complete.cases(knnSample),]

#normalize columns to same scale
normalize <- function(x){
  if(!is.numeric(x)){
    return(x)
  }else{
    norm_x <- (x-min(x))/(max(x) - min(x))
    return(norm_x)    
  }
}

knnNorm <- as.data.frame(sapply(knnSample, normalize))


#training and test samples
# Sample into training and test sets
set.seed(123)
trn <- sample(seq_len(nrow(knnNorm)), size = floor(.75 * nrow(knnNorm)))

train <- knnNorm[trn, ]
test <- knnNorm[-trn, ]

#run knn
kn <- floor(sqrt(nrow(train)))
pred.knn <- knn(train = train[, 1:11], test = test[, 1:11], cl = train[, 12],
                   k = kn)


table(test[,12],pred.knn)
prop.table(table(test[,12],pred.knn))

#

