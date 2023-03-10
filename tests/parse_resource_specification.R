library(resources)
library(utils)

parse_resource_specification <- resources:::parse_resource_specification

specs <- parse_resource_specification(quote(fork()))
str(specs)
stopifnot(
  is.list(specs),
  length(specs) == 1L
)
specs <- specs[[1]]
stopifnot(
  is.list(specs),
  specs[["name"]] == "fork",
  specs[["type"]] == "logical",
  identical(specs[["should_be"]], TRUE)
)

specs <- parse_resource_specification(quote(!fork()))
str(specs)
stopifnot(
  is.list(specs),
  length(specs) == 1L
)
specs <- specs[[1]]
stopifnot(
  is.list(specs),
  specs[["name"]] == "fork",
  specs[["type"]] == "logical",
  identical(specs[["should_be"]], FALSE)
)
