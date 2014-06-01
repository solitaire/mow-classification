#!/usr/bin/Rscript

set.seed(1)

cat("Loading libraries... ");
source("mow.r")
cat("Done.\n");
data = prepareData("1percent.data");
KNN(data$train, data$test, data$trainClasses)
