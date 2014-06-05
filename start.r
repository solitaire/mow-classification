#!/usr/bin/Rscript

args = commandArgs(TRUE)
if (length(args) == 0) {
	stop("Usage: ./start 10percent.data.processed")
}
PROCESSED_FILE = args[1]

source("mow.r")
source("tictoc.r")
set.seed(1)

tic()
cat("Loading ", PROCESSED_FILE, "... ", sep = "")
load(PROCESSED_FILE)
cat("Done in ",  toc(), "s.\n", sep = "")
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
