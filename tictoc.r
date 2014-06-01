startTime = Sys.time()

tic = function() {
  startTime = Sys.time()
}

toc = function() {
  return (floor(10 * (Sys.time() - startTime)) / 10)
}
