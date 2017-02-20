## Improve kNN

library(dplyr)
library(class)
library(caret)

#function
#get a vector of k
# for each set of train+test data, run knn with each k
# rank
# return list ranked by performance

#function - performance of knn

#make combinations of 11 variables 3-10 and make a vector with each set of variables
varsList <- list()
n <- 1
for(i in 3:11){
  temp <- combn(gradVars[1:11], i)
  print(i)
  for(j in 1:ncol(temp)){
    varsList[[n]] <- temp[,j]
    n <- n + 1
  }
}

# add "earnings" to each 
for(i in 1:length(varsList)){
  varsList[[i]] <- c(varsList[[i]], "earnings")
}

#randomly separate 25/75
set.seed(123)
trn <- sample(seq_len(nrow(knnNorm)), size = floor(.75 * nrow(knnNorm)))

# lists of data frames with each combination
knnList <- list()
trainList <- list()
testList <- list()

for(i in 1:length(varsList)){
  colNums <- match(varsList[[i]], names(knnNorm))
  knnList[[i]] <- select(knnNorm, colNums)
  trainList[[i]] <- knnList[[i]][trn,]
  testList[[i]] <- knnList[[i]][-trn,]
}


predList <- list()
accuracy <- c()
for(i in 1:length(trainList)){
  cols <- ncol(trainList[[i]])
  predList[[i]] <- knn(train = trainList[[i]][,1:(cols-1)], 
                       test = testList[[i]][,1:(cols-1)],
                       cl = trainList[[i]][,cols],
                       k = 27)
  cm <- confusionMatrix(predList[[i]], testList[[i]][,cols])
  #add accuracy to varsList
  accuracy[i] <- cm$overall['Accuracy']
}

#Get best few results
accsort <- sort(accuracy, index.return = TRUE, decreasing = TRUE)

varsListSorted <- list()
count <- 1
for(index in accsort[[2]]){
  varsListSorted[[count]] <- varsList[[index]]
  count <- count + 1
}

# 
# top20 <- varsListSorted[1:20]
# bottom20 <-varsListSorted[1961:1981]

