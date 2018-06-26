quiet <-
function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
