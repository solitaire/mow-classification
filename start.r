#!/usr/bin/Rscript

args = commandArgs(TRUE)
if (length(args) < 2) {
	stop("Usage: ./start 10percent.data.processed knn")
}
PROCESSED_FILE = args[1]
ALGORITHM = args[2]

source("mow.r")
source("tictoc.r")
set.seed(1)

tic()
cat("Loading ", PROCESSED_FILE, "... ", sep = "")
load(PROCESSED_FILE)
cat("Done in ",  toc(), "s.\n", sep = "")
cat(nrow(data$train), "training samples,", sum(data$trainClasses == "normal"), "have 'normal' class\n")
cat(nrow(data$test), "test samples,", sum(data$testClasses == "normal"), "have 'normal' class\n")

if (ALGORITHM == "knn") {
	tic()
	K = 1
	cat(paste(K, "NN... ", sep = ""))
	classes = KNN(data$train, data$test, data$trainClasses, K)
	cat("Done in ", toc(), "s.\n", sep = "")
	cat("Error rate: ", error(classes, data$testClasses), "\n")
	print(confusionMatrix(data$testClasses, classes))
}

if (ALGORITHM == "bayes") {
	tic()
	cat("Bayes... ")
	classes = Bayes(data$train, data$test, data$trainClasses)
	cat("Done in ", toc(), "s.\n", sep = "")
	cat("Error rate: ", error(classes, data$testClasses), "\n")
	print(confusionMatrix(data$testClasses, classes))
}

if (ALGORITHM == "svm") {
	tic()
	cat("SVM... ")
	w = c(0.5, 5, 0.5, 10, 10)
	names(w) = c("normal", "probe", "dos", "u2r", "r2l")
	classes = SVM(data$train, data$test, data$trainClasses, kernelFunc = "radial", classWeights=w)
	cat("Done in ", toc(), "s.\n", sep = "")
	cat("Error rate: ", error(classes, data$testClasses), "\n")
	print(confusionMatrix(data$testClasses, classes))
}
