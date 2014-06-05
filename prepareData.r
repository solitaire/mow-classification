#!/usr/bin/Rscript

args = commandArgs(TRUE)
if (length(args) == 0) {
	stop("Usage: ./prepareData 10percent.data")
}
FILE = args[1]
PROCESSED_FILE = paste(FILE, ".processed", sep = "")

source("mow.r")
source("tictoc.r")

tic()
cat("Processing ", FILE, "... ", sep = "")
data = prepareData(FILE)
cat("Done in ", toc(), "s.\n", sep = "")

tic()
cat("Saving ", PROCESSED_FILE, "... ", sep = "")
save(data, file = PROCESSED_FILE)
cat("Done in ", toc(), "s.\n", sep = "")
