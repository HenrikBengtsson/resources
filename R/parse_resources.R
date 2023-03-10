#' Parse a Resource Specification
#'
#' @param spec Resource specification as a [base::formula].
#'
#' @return
#' A list of resource specifications.
#'
#' @examples
#' resources <- parse_resources(~ !fork)
#' print(resources)
#' resources <- parse_resources(~ !fork && ram(32*GiB))
#' print(resources)
#'
#' @export
parse_resources <- function(specs) {
  stopifnot(
    inherits(specs, "formula"),
    identical(specs[[1]], as.symbol("~")),
    length(specs) == 2L                     ## formula without LHS
  )

  specs <- specs[-1]
  mdebugf("Parse RHS specifications ...")
  mdebugf("- specification: %s", deparse(specs))

  res <- list()
  for (ii in seq_along(specs)) {
    mdebugf("RHS part %d of %d ...", ii, length(specs))
    spec <- specs[[ii]]
    value <- parse_resource_specification(spec)
    res <- c(res, list(value))
    mdebugf("RHS part %d of %d ... done", ii, length(specs))
  }
  
  mdebugf("Parse RHS specifications ... done")

  res
}


parse_resource_specification <- function(specs) {
  mdebugf("Parse specifications ...")
  mdebugf("- specification: %s", deparse(specs))

  if (is.symbol(specs)) {
    spec <- specs
    name <- as.character(spec)
    mdebugf("- symbol: '%s'", name)
    value <- list(name = name, type = "logical", should_be = TRUE)
    return(list(value))
  }
  
  res <- list()
  kk <- 1L
  while (kk <= length(specs)) {
    mdebugf("Part %d of %d ...", kk, length(specs))
    spec <- specs[[kk]]
    if (is.symbol(spec)) {
      name <- as.character(spec)
      mdebugf("- symbol: '%s'", name)
      type <- if (name == "!") {
        "operator"
      } else {
        "logical"
      }
      
      if (type == "operator") {
        if (! name %in% c("!")) {
          stop(sprintf("Syntax error. Unknown resource specification operator: %s", sQuote(name)))
        }
        stopifnot(kk < length(specs)) ## should always be true
        kk <- kk + 1L
        spec <- specs[[kk]]
        value <- parse_resource_specification(spec)
        value <- value[[1]]        
        if (value$type != "logical") {
          stop(sprintf("Syntax error: Do not know how to negate a resource specification of type %s: %s", sQuote(value$type), deparse(spec)))
        }
        value$should_be <- !value$should_be
      } else {
        value <- list(name = name, type = type, should_be = TRUE)
      }
      res <- c(res, list(value))
    } else if (is.language(spec)) {
      mdebugf("- expression: '%s'", deparse(spec))
      stop("Not yet implemented")
    }
    mdebugf("Part %d of %d ... done", kk, length(specs))
    kk <- kk + 1L
  }
  
  mdebugf("Parse specifications ... done")

  res
}
