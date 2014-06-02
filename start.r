#!/usr/bin/Rscript

source("tictoc.r")

FILE = "10percent.data"

set.seed(1)

tic()
cat("Loading R libraries... ")
source("mow.r")
cat("Done in ", toc(), "s.\n", sep = "")

tic()
cat("Loading ", FILE, "... ", sep = "")
data = prepareData(FILE, 0.1)
cat("Done in ", toc(), "s.\n", sep = "")
cat(nrow(data$train), "training samples,", sum(data$trainClasses == "normal"), "have 'normal' class\n")
cat(nrow(data$test), "test samples,", sum(data$testClasses == "normal"), "have 'normal' class\n")

tic()
cat("KNN... ")
classes = KNN(data$train, data$test, data$trainClasses)
cat("Done in ", toc(), "s.\n", sep = "")
cat("Error rate: ", error(classes, data$testClasses), "\n")

tic()
cat("Bayes... ")
classes = Bayes(data$train, data$test, data$trainClasses)
cat("Done in ", toc(), "s.\n", sep = "")
cat("Error rate: ", error(classes, data$testClasses), "\n")

tic()
cat("SVM... ")
classes = SVM(data$train, data$test, data$trainClasses)
cat("Done in ", toc(), "s.\n", sep = "")
cat("Error rate: ", error(classes, data$testClasses), "\n")