##########  QUESTION 4  ###########

accuracy <- function(table) {
  (table[1,1]+table[2,2])/(table[1,1]+table[2,2]+table[1,2]+table[2,1])
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



# 3)
##### Report learning curves by measuring classification performance
##### with an increasing number of training examples. #####

# 4)
##### Choose the training examples uniformly at random
##### from the global training set and report median performances 
##### (boxplots may be useful to illustrate the variability across different selec- tions of training examples).

# 5) en gros c'est la question 2...
##### Are the default meta-parameter definitions of the svm method appropriate for your ex- periments
##### on the DIGITS dataset? Use the validation set to select the best choices of meta-parameters. #####

# 6)
##### Can you conclude that the results obtained on the validation set,
##### after selecting the optimal meta-parameters, predict accurately test set classification results?
##### Turn in one or several plot(s) to sustain your claims. #####