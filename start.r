#!/usr/bin/Rscript

FILE = "1percent.data"

set.seed(1)

cat("Loading libraries... ")
source("mow.r")
cat("Done.\n")

cat("Loading ", FILE, "... ", sep = "")
data = prepareData(FILE);
cat("Done.\n")

cat("KNN... ")
classes = KNN(data$train, data$test, data$trainClasses)
cat("Done.\n")
cat(error(classes, data$testClasses), "\n")
