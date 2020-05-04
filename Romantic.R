
rm(list = ls());

#summary(lmfit)

#summary(lmfit2)

mydata <- read.csv("student-por.csv",header=TRUE)
lmfit <- lm(G1 ~ studytime  + Medu + failures + goout
            + Mjob + Fjob + higher + Dalc + absences, mydata)
#ggpairs(mydata, aes(alpha = 0.4))
summary(lmfit)

glmfit = glm(G1_avg ~ studytime  + factor(Medu) + factor(failures) + goout
             + Mjob + Fjob + higher + Dalc + absences,
             mydata,family="binomial")
summary(glmfit)


dataSet = read.csv("student-por.csv")

if (!require("rpart")) { install.packages("rpart")
  require("rpart") }

if (!require("rpart.plot")) { install.packages("rpart.plot")
  require("rpart.plot") }
set.seed(123) 
trainIndex  <- sample(1:nrow(dataSet), size = round(0.5*nrow(dataSet)), 
                      replace = F)

trainingSet <- dataSet[ trainIndex,]

# Put everything that wasn't in that subset (1-p) in validationSet
validationSet  <- dataSet[-trainIndex,]

MyTree <- rpart(G1_avg ~ studytime  + Medu + failures + goout
                + Mjob + Fjob + higher + Dalc + absences, data=trainingSet, method="class", 
                control=rpart.control(minsplit=1, cp=0.0005))

cat("\n###### Display the text output from each decision tree: ######\n")
printcp(MyTree)
plotcp(MyTree, minline = FALSE)

prunedTree <- prune(MyTree, cp=MyTree$cptable[which.min(MyTree$cptable[,"xerror"]),"CP"])

predTraining <- predict(MyTree, trainingSet, type="class") 
predValidation <- predict(MyTree, validationSet, type="class")

# Generating Confusion Matrices for the traing and validation sets:
cat("\n###### Confusion Matrix for the training set ######\n")
table(Predicted=predTraining,Observed=trainingSet[, 32] )
cat("\n###### Confusion Matrix for the validation set ######\n")
table(Predicted=predValidation,Observed=validationSet[, 32] )
predRateTraining <- mean(predTraining == trainingSet[, 32])
predRateValidation <- mean(predValidation == validationSet[, 32])

# This stops R from writing any more to the text output file.

prp(prunedTree, main=paste("Decision Tree\n(Correct classification rate ",
                           round(predRateTraining,4)*100,
                           "% for the training set\n ",
                           round(predRateValidation,4)*100,
                           "% for the validation set)"), 
    type=4, extra=6, faclen=0, under=TRUE)




