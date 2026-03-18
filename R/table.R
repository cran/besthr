#' Generate a summary table from besthr results
#'
#' Creates a publication-ready summary table containing group statistics,
#' confidence intervals, and optionally effect sizes and significance.
#'
#' @param hrest An hrest object from \code{\link{estimate}}
#' @param format Output format: "tibble" (default), "markdown", "html", or "latex"
#' @param digits Number of decimal places for rounding (default 2)
#' @param include_significance Logical, whether to include significance stars (default FALSE)
#'
#' @return A tibble (if format = "tibble") or character string (other formats)
#'
#' @export
#'
#' @examples
#' d <- make_data()
#' hr <- estimate(d, score, group)
#' besthr_table(hr)
#' besthr_table(hr, format = "markdown")
#'
besthr_table <- function(hrest, format = "tibble", digits = 2,
                         include_significance = FALSE) {
  if (!inherits(hrest, "hrest")) {
    stop("hrest must be an hrest object from estimate()")
  }

  valid_formats <- c("tibble", "markdown", "html", "latex")
  if (!format %in% valid_formats) {
    stop("format must be one of: ", paste(valid_formats, collapse = ", "))
  }

  # Get group column name
group_col_name <- names(hrest$group_n)[names(hrest$group_n) != "n"][[1]]

  # Build base table from group_n and group_means
  tbl <- dplyr::left_join(
    hrest$group_n,
    hrest$group_means,
    by = group_col_name
  )

  # Add CI for non-control groups
  ci_data <- hrest$ci
  names(ci_data)[names(ci_data) == "low"] <- "ci_low"
  names(ci_data)[names(ci_data) == "high"] <- "ci_high"
  names(ci_data)[names(ci_data) == "mean"] <- "ci_mean"

  tbl <- dplyr::left_join(tbl, ci_data[, c(group_col_name, "ci_low", "ci_high")],
                          by = group_col_name)

  # Rename columns for clarity
  names(tbl)[names(tbl) == "mean"] <- "mean_rank"
  names(tbl)[names(tbl) == group_col_name] <- "group"

  # Compute effect size (difference from control)
  control_mean <- tbl$mean_rank[tbl$group == hrest$control]
  tbl$effect_size <- ifelse(tbl$group == hrest$control, NA_real_,
                            tbl$mean_rank - control_mean)

  # Add significance if requested
  if (include_significance) {
    sig <- compute_significance(hrest)
    tbl <- dplyr::left_join(tbl, sig[, c("group", "stars")], by = "group")
    names(tbl)[names(tbl) == "stars"] <- "significance"
  }

  # Round numeric columns
  numeric_cols <- c("mean_rank", "ci_low", "ci_high", "effect_size")
  for (col in numeric_cols) {
    if (col %in% names(tbl)) {
      tbl[[col]] <- round(tbl[[col]], digits)
    }
  }

  # Reorder columns
  col_order <- c("group", "n", "mean_rank", "ci_low", "ci_high", "effect_size")
  if (include_significance) col_order <- c(col_order, "significance")
  tbl <- tbl[, col_order[col_order %in% names(tbl)]]

  # Return in requested format
  if (format == "tibble") {
    return(tibble::as_tibble(tbl))
  }

  # Convert to formatted string
  format_table(tbl, format)
}

#' Format table as string
#'
#' Internal function to convert tibble to markdown, html, or latex string.
#'
#' @param tbl A tibble to format
#' @param format The output format
#'
#' @return A character string
#' @keywords internal
#'
format_table <- function(tbl, format) {
  if (format == "markdown") {
    # Build markdown table
    header <- paste("|", paste(names(tbl), collapse = " | "), "|")
    separator <- paste("|", paste(rep("---", ncol(tbl)), collapse = " | "), "|")

    rows <- apply(tbl, 1, function(row) {
      paste("|", paste(as.character(row), collapse = " | "), "|")
    })

    return(paste(c(header, separator, rows), collapse = "\n"))
  }

  if (format == "html") {
    # Build HTML table
    header_cells <- paste0("<th>", names(tbl), "</th>", collapse = "")
    header_row <- paste0("<tr>", header_cells, "</tr>")

    data_rows <- apply(tbl, 1, function(row) {
      cells <- paste0("<td>", as.character(row), "</td>", collapse = "")
      paste0("<tr>", cells, "</tr>")
    })

    return(paste0(
      "<table>\n<thead>\n", header_row, "\n</thead>\n<tbody>\n",
      paste(data_rows, collapse = "\n"),
      "\n</tbody>\n</table>"
    ))
  }

  if (format == "latex") {
    # Build LaTeX table
    col_spec <- paste(rep("l", ncol(tbl)), collapse = "")
    header <- paste(names(tbl), collapse = " & ")
    rows <- apply(tbl, 1, function(row) {
      paste(as.character(row), collapse = " & ")
    })

    return(paste0(
      "\\begin{tabular}{", col_spec, "}\n",
      "\\hline\n",
      header, " \\\\\n",
      "\\hline\n",
      paste(rows, collapse = " \\\\\n"), " \\\\\n",
      "\\hline\n",
      "\\end{tabular}"
    ))
  }
}
