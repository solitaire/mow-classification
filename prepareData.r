#!/usr/bin/Rscript

args = commandArgs(TRUE)
if (length(args) == 0) {
	stop("Usage: ./prepareData 10percent.data")
}
TRAIN_FILE = args[1]
TEST_FILE = "test.data"

PROCESSED_FILE = paste(TRAIN_FILE, ".processed", sep = "")

source("mow.r")
source("tictoc.r")
set.seed(1)

tic()
cat("Processing ", TRAIN_FILE, "... ", sep = "")
data = prepareData(TRAIN_FILE, TEST_FILE)
cat("Done in ", toc(), "s.\n", sep = "")

tic()
cat("Saving ", PROCESSED_FILE, "... ", sep = "")
save(data, file = PROCESSED_FILE)
cat("Done in ", toc(), "s.\n", sep = "")
