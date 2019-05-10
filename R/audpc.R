
# Adapted from cropsim package version 0.2.0-5 by Adam H. Sparks - USQ CCH

#' @noRd
audpc <- function(x) {
  if (length(x$severity) > 0) {
    return(sum(x$severity))
  } else if (length(x$incidence) > 0) {
    return(sum(x$incidence))
  } else {
    stop("\nCannot find incidence or severity.\n",
         call. = FALSE)
  }
}

raudpc <- function(x) {
  audpc(x) / length(x[, 1])
}
