tic = function() {
	tic = proc.time()["elapsed"]
	assign(".tic", tic, envir=baseenv())
	invisible(tic)
}

toc = function() {
   toc = proc.time()["elapsed"]
   tic = get(".tic", envir=baseenv())
   invisible(toc)
   return (toc - tic)
}
