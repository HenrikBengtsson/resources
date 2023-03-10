.package <- new.env()

## covr: skip=all
.onLoad <- function(libname, pkgname) {
  .package[["version"]] <- utils::packageVersion(pkgname)
  update_package_options()
}

