#' Create a List of Breakpoints
#'
#' This function creates a list of numeric breakpoints for each field. If
#' \code{breaks} is a numeric vector, the same breakpoints are replicated for
#' each field. If \code{breaks} is a list, each element of the list is used
#' directly (with \code{-Inf} and \code{Inf} added).
#'
#' @param breaks Either a numeric vector or a list of numeric vectors. If numeric,
#'   the breakpoints will be \code{c(-Inf, breaks, Inf)}. If a list, each list
#'   element will be prepended by \code{-Inf} and appended by \code{Inf}.
#' @param types A character vector indicating the comparison type for each field
#'   (e.g., \code{"bi"}, \code{"lv"}, \code{"nu"}). Only used to replicate the
#'   breakpoints the correct number of times.
#'
#' @return A list of numeric vectors representing breakpoints for each field.
#'
#' @examples
#' # Replicates the same breakpoints for 3 fields:
#' create_breaklist(c(0, 0.25, 0.5), types = c("bi", "lv", "nu"))
#'
#' # Uses a list of numeric breakpoints for two different fields:
#' create_breaklist(list(c(0, 0.25), c(0, 0.1, 0.2)), types = c("lv", "nu"))
#'
#' @export
create_breaklist <- function(breaks, types) {
  # If 'breaks' is a numeric vector:
  if (is.numeric(breaks)) {
    return(rep(list(c(-Inf, breaks, Inf)), length(types)))
  }

  # If 'breaks' is a list:
  if (is.list(breaks)) {
    breaklist_out <- vector("list", length(types))
    for (f in seq_along(types)) {
      # Ensure -Inf and Inf are included
      breaklist_out[[f]] <- unique(c(-Inf, breaks[[f]], Inf))
    }
    return(breaklist_out)
  }

  stop("'breaks' must be either a numeric vector or a list.")
}
