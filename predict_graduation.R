setwd("/Users/zisvan/insight_project/")

library(dplyr)
library(ggplot2)
library(class)
library(caret)

# Get the data
if (!(exists("sample") && is.data.frame(get("sample")))){
  source('./prepare_data.R')
}


# Make knn sample; normalize variables (distance 0-1)
normalize <- function(x){
  if(!is.numeric(x)){
    return(x)
  }else{
    norm_x <- (x-min(x))/(max(x) - min(x))
    return(norm_x)  
  }
}


knnNorm <- as.data.frame(sapply(sample, normalize))

numeric.num <- match(gradVars[1:length(gradVars)-1], names(knnNorm))
knnNorm[numeric.num] <- sapply(knnNorm[numeric.num], as.character)
knnNorm[numeric.num] <- sapply(knnNorm[numeric.num], as.numeric)



# knn with all variables
# Sample into training and test sets
set.seed(123)
trn <- sample(seq_len(nrow(knnNorm)), size = floor(.75 * nrow(knnNorm)))

train <- knnNorm[trn, ]
test <- knnNorm[-trn, ]

# Run knn
kn <- floor(sqrt(nrow(train)))
pred.knn <- knn(train = train[, 2:12], test = test[, 2:12], cl = train[, 13],
                   k = kn)

# table(test[,13],pred.knn)
# cm <- confusionMatrix(pred.knn, test[,13])
# acc <- cm$overall['Accuracy']

# Simpler for accuracy only
acc <- mean(pred.knn == test[,13])


# knn with top 4 important variables
top4 <- c("SAT_AVG", "RET_FT4", "PAR_ED_PCT_1STGEN", "DEBT_MDN")
colTop4 <- match(top4, names(knnNorm))
knn4 <- dplyr::select(knnNorm, colTop4)

train4 <- knn4[trn, ]
test4 <- knn4[-trn, ]

pred.knn4 <- knn(train = train4, test = test4, cl = train[, 13], k = kn)
acc4 <- mean(pred.knn4 == test[,13])


# Vary k
predictions <- list()
accuracies <- list()
index <- 1

kns <- seq(3, kn+10, 2)
for (i in kns){
  predictions[[index]] <- knn(train = train4, test = test4, cl = train[, 13], k=i)
  accuracies[[index]] <- mean(predictions[[index]] == test[, 13])
  index <- index + 1
}

names(predictions) <- as.character(kns)
names(accuracies) <- as.character(kns)

# sort
accuracies[order(unlist(accuracies))]


# Vary k and variables together

ordered <- c("SAT_AVG", "RET_FT4", "PAR_ED_PCT_1STGEN", "DEBT_MDN", "COSTT4_A",
             "AVGFACSAL", "TUITFTE", "INEXPFTE", "ADM_RATE",
             "UGDS", "PFTFAC")

# loop over k with these; refill
predictions <- list()
accuracies <- list()


# loop over variables
pred.knnN <- list()
accN <- list()
topN <- list()
colTopN <- list()
knnN <- list()
trainN <- list()
testN <- list()
m = 1
for (i in 2:length(ordered)){
  topN[[m]] <- ordered[1:i]
  colTopN[[m]] <- match(topN[[m]], names(knnNorm))
  knnN[[m]] <- dplyr::select(knnNorm, colTopN[[m]]) #knn sample with N columns
  
  trainN[[m]] <- knnN[[m]][trn, ]
  testN[[m]] <- knnN[[m]][-trn, ]
  
  n=1
  for (j in kns){
    predictions[[n]] <- knn(train = trainN[[m]], test = testN[[m]], cl = train[, 13], k = j)
    accuracies[[n]] <- mean(predictions[[n]] == test[,13])
    n <- n + 1
  }
  pred.knnN[[m]] <- predictions
  accN[[m]] <- accuracies
  m <- m + 1  

}

acc_df <- data.frame(do.call(rbind, accN))

