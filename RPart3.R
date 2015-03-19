##########  QUESTION 4  ###########

accuracy <- function(table) {
  (table[1,1]+table[2,2])/(table[1,1]+table[2,2]+table[1,2]+table[2,1])
}

accuracy2 <- function(tab, initial){
  acc <- 100-(sum(tab)-sum(diag(tab)))/dim(initial)[1]*100
  acc
}

# 1)
##### Estimate a Support Vector Machine on the DIGITS training set to predict the class label.
##### Use in particular a classifier learned with svm from the e1071 package.  #####
x <- subset(DigitsTrain, select = -Class)
y <- subset(DigitsTrain, select = Class)

model <- svm(Class ~ .,DigitsTrain ,cost = 2, gamma = 0.0004)
pre.test <- predict(model, DigitsValid)

table(pred = pre.test , true = t(DigitsValid[1]))
print(model)


# 2)
##### Analyze the classification performance on the DIGITS validation set
##### as a function of the choice of the meta-parameters you estimate important.####

accuracy <- function(table) {
  (table[1,1]+table[2,2])/(table[1,1]+table[2,2]+table[1,2]+table[2,1])
}


G = seq(0.2, 2, by = 0.2)
C = seq(1, 7, by = 1)

parameters <- function() {
  df <- data.frame()
  for ( i in C) {
    for( j in 1:length(G)){
      model <- svm(Class ~ .,DigitsTrain ,cost = C[i], gamma = G[j]/dim(DigitsTrain)[2])#0.0004
      pre.test <- predict(model, DigitsValid)
      tab <- table(pre.test , DigitsValid$Class)
      acc <- accuracy(tab)
      df[(i-1)*length(G)+j, 1] <- acc
      df[(i-1)*length(G)+j, 2] <- C[i]
      df[(i-1)*length(G)+j, 3] <- G[j]/dim(DigitsTrain)[2]
      df[(i-1)*length(G)+j, 4] <- G[j]
      
      #print(paste("Accuracy = ", acc, "| cost = ", i, " | gamma = ", G[j]/dim(DigitsTrain)[2]))
    }
  }
  df
}

results <- parameters()
bestsParam <- which(results[1]== max(results[1]),arr.ind = TRUE)
best <- results[bestsParam[1],]
print(paste("Accuracy = ", best[1], "| cost = ", best[2], " | gamma = ", best[3]))

precision
plot(precision)


# 3)
##### Report learning curves by measuring classification performance
##### with an increasing number of training examples. #####

samplingTest <- function() {

  vector <- c(2, 5, 10, 20, 50, 99)/100
  acc = c()
  num = 10
  df <- data.frame()
  for ( i in 1:length(vector)) {
    for ( j in 1:num) {
      dim <- dim(DigitsTrain)
      randomvector <- sample(dim[1])
      len = (dim[1]*vector[i])
      trainingvector <- randomvector[1:len]
      mysample <- DigitsTrain[trainingvector,]
      
      model <- svm(Class ~ .,mysample ,cost = 4, gamma = 0.0004)#0.0004###TODO
      pre.test <- predict(model, DigitsTest)
      acc[i] = acc[i] + accuracy(table(pred = pre.test , true = t(DigitsTest[1])))/num
            
      df[(i-1)*num+j,1] = accuracy(table(pred = pre.test , true = t(DigitsTest[1])))
    }
  }
  plot(vector, acc, xlab="size", ylab="accuracy", type="l", col="blue")
  #lines(vector, size, xlab="size", ylab="number of nodes", type="l", col="blue")
  df
}

df = samplingTest()
df


vector <- c(2, 5, 10, 20, 50, 99)/100
acc = c()
num = 10
df <- data.frame()
for ( i in 1:length(vector)) {
  for ( j in 1:num) {
    dim <- dim(DigitsTrain)
    randomvector <- sample(dim[1])
    len = (dim[1]*vector[i])
    trainingvector <- randomvector[1:len]
    mysample <- DigitsTrain[trainingvector,]
    
    model <- svm(Class ~ .,DigitsTrain ,cost = 7, gamma = 0.000479616306954437)#0.0004###TODO
    pre.test <- predict(model, DigitsTest)
    acc[i] = acc[i] + accuracy(table(pred = pre.test , true = t(DigitsTest[1])))/num
    accuracy(table(pred = pre.test , true = t(DigitsTest[1])))
    df[(i-1)*num+j,1] = accuracy(table(pred = pre.test , true = t(DigitsTest[1])))
  }
}
plot(vector, acc, xlab="size", ylab="accuracy", type="l", col="blue")
#lines(vector, size, xlab="size", ylab="number of nodes", type="l", col="blue")
df

# 4)
##### Choose the training examples uniformly at random
##### from the global training set and report median performances 
##### (boxplots may be useful to illustrate the variability across different selections of training examples).

# 5) en gros c'est la question 2...
##### Are the default meta-parameter definitions of the svm method appropriate for your ex- periments
##### on the DIGITS dataset? Use the validation set to select the best choices of meta-parameters. #####TODOTODOTODOTODO

# 6)
##### Can you conclude that the results obtained on the validation set,
##### after selecting the optimal meta-parameters, predict accurately test set classification results?
##### Turn in one or several plot(s) to sustain your claims. #####


### Estimate a SVM on the whole training set according to the best choices of meta-parameters 
### (as determined previously on the validation set) and report performances on the whole test set.

x <- subset(DigitsTrain, select = -Class)
y <- subset(DigitsTrain, select = Class)

model <- svm(Class ~ .,DigitsTrain ,cost = 7, gamma = 0.000479616306954437)
pre.test <- predict(model, DigitsValid)

table(pred = pre.test , true = t(DigitsValid[1]))
print(model)

### Choose a significantly different meta-parameters setting (based on the validation
### results computed in question 4), report the corresponding number of support vectors,
### training and test set classification results.

x <- subset(DigitsTrain, select = -Class)
y <- subset(DigitsTrain, select = Class)


model <- svm(Class ~ .,x,y ,cost = 7, gamma = 0.000479616306954437)
model <- svm(Class ~ .,DigitsTrain ,cost = 7, gamma = 0.000479616306954437)
pre.test <- predict(model, DigitsValid)

newsample <- DigitsValid

## last sous question 5.2

model <- svm(Class ~ .,DigitsTrain ,cost = 7, gamma = 0.000479616306954437)
pre.test <- predict(model, DigitsTrain)
accuracy(table(pred = pre.test , true = t(DigitsTrain[1])))

pre.test <- predict(model, DigitsValid)
accuracy(table(pred = pre.test , true = t(DigitsValid[1])))

pre.test <- predict(model, DigitsTest)
accuracy(table(pred = pre.test , true = t(DigitsTest[1])))

## résultat sur 