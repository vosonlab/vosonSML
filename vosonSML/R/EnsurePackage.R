EnsurePackage <-
function(x) {
  # EnsurePackage(x) - Installs and loads a package if necessary
  # Args:
  #   x: name of package

  x <- as.character(x)
  if (!require(x, character.only=TRUE)) {
    install.packages(pkgs=x, repos="http://cran.r-project.org")
    require(x, character.only=TRUE)
  }
}
