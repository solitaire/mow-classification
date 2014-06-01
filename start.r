#!/usr/bin/Rscript

FILE = "10percent.data"

set.seed(1)

startTime = Sys.time()
cat("Loading libraries... ")
source("mow.r")
cat("Done in ", Sys.time() - startTime, "s.\n", sep = "")

startTime = Sys.time()
cat("Loading ", FILE, "... ", sep = "")
data = prepareData(FILE, 0.1)
cat("Done in ", Sys.time() - startTime, "s.\n", sep = "")
cat(nrow(data$train), "training samples,", sum(data$trainClasses == "normal"), "have 'normal' class\n")
cat(nrow(data$test), "test samples,", sum(data$testClasses == "normal"), "have 'normal' class\n")

startTime = Sys.time()
cat("KNN... ")
classes = KNN(data$train, data$test, data$trainClasses)
cat("Done in ", Sys.time() - startTime, "s.\n", sep = "")
cat(error(classes, data$testClasses), "\n")
