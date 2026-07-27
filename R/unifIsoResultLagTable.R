#' Constructor for unifIsoResultLagTable objects
#'
#' @description
#' Creates an object of class \code{unifIsoResultLagTable}, which wraps a list
#' of lag family data frames for specialized formatting and printing.
#'
#' @param lag_list A list of data frames containing lag information and proportions.
#'
#' @return An object of class \code{unifIsoResultLagTable}.
#'
#' @name new_unifIsoResultLagTable
#' @keywords internal
#' @noRd
new_unifIsoResultLagTable <- function(lag_list) {
  structure(
    lag_list,
    class = "unifIsoResultLagTable"
  )
}


#' Print method for unifIsoResultLagTable objects
#'
#' @description
#' Custom print method that formats and outputs the lag table families,
#' applying ANSI color codes to highlight out-of-bounds empirical proportions.
#'
#' @param x An object of class \code{unifIsoResultLagTable}.
#' @param ... Additional arguments passed to \code{print}.
#'
#' @return The function is called for its side effects (printing).
#' @export
#'
#' @examples
#' \dontrun{
#' # Placeholder for print.unifIsoResultLagTable example
#' }
print.unifIsoResultLagTable <- function(x, ...) {

  for (i in seq_along(x)) {
    # Coerce to a standard data.frame immediately
    tbl <- as.data.frame(x[[i]])
    family_name <- names(x)[i]

    # Extract summary statistics for the family header
    avg_length <- mean(tbl$length, na.rm = TRUE)
    expected_val <- tbl$Expected[1]

    cat(sprintf("Lag %s: Length %g, Expected proportion: %g\n",
                family_name, avg_length, expected_val))

    # Identify the out-of-bounds rows before removing columns
    out_of_bounds <- tbl$Expected < tbl$CI_lower | tbl$Expected > tbl$CI_upper

    # Drop columns not needed for the final printed output
    cols_to_keep <- setdiff(names(tbl), c("Family", "Expected", "length"))
    tbl <- tbl[, cols_to_keep, drop = FALSE]

    # Manual string formatting to prevent ANSI escape character misalignment
    col_names_padded <- character(length(cols_to_keep))

    for (j in seq_along(cols_to_keep)) {
      col <- cols_to_keep[j]
      vals <- as.character(tbl[[col]])

      # Find the necessary width to align this column perfectly
      max_width <- max(nchar(vals), nchar(col))

      # Pad both the header name and the column values with spaces to match max_width
      col_names_padded[j] <- sprintf("%-*s", max_width, col)
      tbl[[col]] <- sprintf("%-*s", max_width, vals)
    }

    # Apply conditional coloring to the Proportion column
    # Coloring is applied after padding to maintain visual alignment
    tbl[["Proportion"]][out_of_bounds] <- cli::col_red(tbl[["Proportion"]][out_of_bounds])
    tbl[["Proportion"]][!out_of_bounds] <- cli::col_green(tbl[["Proportion"]][!out_of_bounds])


    # Print header names separated by two spaces
    cat(paste(col_names_padded, collapse = "  "), "\n")

    # Print each row separated by two spaces
    for (r in seq_len(nrow(tbl))) {
      row_vals <- unlist(tbl[r, cols_to_keep], use.names = FALSE)
      cat(paste(row_vals, collapse = "  "), "\n")
    }

    # Add a blank line between tables
    cat("\n")
  }

  invisible(x)
}
