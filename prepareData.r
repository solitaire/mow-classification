#!/usr/bin/Rscript

FILE = "100percent.data"
PROCESSED_FILE = "100perent_processed.data"

startTime = Sys.time()
cat("Processing file ", FILE, "... ", sep = "")
data = prepareData(FILE, 1)
write.csv(data, file=PROCESSED_FILE)
cat("Done in ", Sys.time() - startTime, "s.\n", sep = "")
