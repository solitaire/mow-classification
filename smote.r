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
cat(nrow(data$train), "training samples,", sum(data$trainClasses == "dos"), "have 'dos' class\n")
cat(nrow(data$train), "training samples,", sum(data$trainClasses == "u2r"), "have 'u2r' class\n")
cat(nrow(data$train), "training samples,", sum(data$trainClasses == "r2l"), "have 'r2l' class\n")
cat(nrow(data$train), "training samples,", sum(data$trainClasses == "probe"), "have 'probe' class\n")

tic()
cat("SMOTE...\n")
classes = factor(data$trainClasses)
fullData = cbind(data$train, classes)
resampled = resampleData(classes, fullData, 10000, 6000)
resampledClasses = resampled$classes
cat(nrow(resampled), "training samples,", sum(resampledClasses == "normal"), "have 'normal' class\n")
cat(nrow(resampled), "training samples,", sum(resampledClasses == "dos"), "have 'dos' class\n")
cat(nrow(resampled), "training samples,", sum(resampledClasses == "u2r"), "have 'u2r' class\n")
cat(nrow(resampled), "training samples,", sum(resampledClasses == "r2l"), "have 'r2l' class\n")
cat(nrow(resampled), "training samples,", sum(resampledClasses == "probe"), "have 'probe' class\n")

data$train = resampled[,1:14]
data$trainClasses = resampledClasses
save(cat(PROCESSED_FILE, ".smote"))

cat("Done in ", toc(), "s.\n", sep = "")

