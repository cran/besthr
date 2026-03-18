#' Compute significance from bootstrap distributions
#'
#' Determines statistical significance by checking if the bootstrap confidence
#' interval for each treatment group overlaps with the control group's mean rank.
#'
#' @param hrest An hrest object from \code{\link{estimate}}
#'
#' @return A data frame with columns: group, significant (logical), p_value, stars
#'
#' @export
#'
#' @examples
#' d <- make_data()
#' hr <- estimate(d, score, group, nits = 500)
#' compute_significance(hr)
#'
compute_significance <- function(hrest) {
  if (!inherits(hrest, "hrest")) {
    stop("hrest must be an hrest object from estimate()")
  }

  # Get group column name
  group_col_name <- names(hrest$group_n)[names(hrest$group_n) != "n"][[1]]

  # Get control mean
  control_mean <- hrest$group_means$mean[hrest$group_means[[group_col_name]] == hrest$control]

  # Get all groups
  all_groups <- unique(hrest$group_means[[group_col_name]])

  # Compute significance for each group
  results <- lapply(all_groups, function(g) {
    if (g == hrest$control) {
      return(data.frame(
        group = g,
        significant = NA,
        p_value = NA_real_,
        stars = "",
        stringsAsFactors = FALSE
      ))
    }

    # Get bootstrap samples for this group
    group_boots <- hrest$bootstraps$mean[hrest$bootstraps[[group_col_name]] == g]

    # Compute proportion of bootstrap samples that cross control mean
    # This gives a two-tailed p-value approximation
    n_boots <- length(group_boots)
    group_mean <- mean(group_boots)

    if (group_mean > control_mean) {
      # Treatment higher - count samples below control
      p_value <- sum(group_boots <= control_mean) / n_boots
    } else {
      # Treatment lower - count samples above control
      p_value <- sum(group_boots >= control_mean) / n_boots
    }

    # Two-tailed
    p_value <- min(p_value * 2, 1)

    # Determine significance
    significant <- p_value < 0.05

    # Assign stars
    stars <- ""
    if (p_value < 0.001) {
      stars <- "***"
    } else if (p_value < 0.01) {
      stars <- "**"
    } else if (p_value < 0.05) {
      stars <- "*"
    }

    data.frame(
      group = g,
      significant = significant,
      p_value = p_value,
      stars = stars,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

#' Compute effect sizes from bootstrap distributions
#'
#' Calculates the effect size (difference from control) for each treatment group
#' with bootstrap confidence intervals.
#'
#' @param hrest An hrest object from \code{\link{estimate}}
#'
#' @return A data frame with columns: group, effect, effect_ci_low, effect_ci_high
#'
#' @export
#'
#' @examples
#' d <- make_data()
#' hr <- estimate(d, score, group, nits = 500)
#' compute_effect_size(hr)
#'
compute_effect_size <- function(hrest) {
  if (!inherits(hrest, "hrest")) {
    stop("hrest must be an hrest object from estimate()")
  }

  # Get group column name
  group_col_name <- names(hrest$group_n)[names(hrest$group_n) != "n"][[1]]

  # Get control mean
  control_mean <- hrest$group_means$mean[hrest$group_means[[group_col_name]] == hrest$control]

  # Get all groups
  all_groups <- unique(hrest$group_means[[group_col_name]])

  # Compute effect size for each group
  results <- lapply(all_groups, function(g) {
    if (g == hrest$control) {
      return(data.frame(
        group = g,
        effect = NA_real_,
        effect_ci_low = NA_real_,
        effect_ci_high = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    # Get group mean (point estimate)
    group_mean <- hrest$group_means$mean[hrest$group_means[[group_col_name]] == g]
    effect <- group_mean - control_mean

    # Get bootstrap samples for this group and compute effect distribution
    group_boots <- hrest$bootstraps$mean[hrest$bootstraps[[group_col_name]] == g]

    # For proper effect CI, we'd need paired bootstrap of control and treatment
    # Approximation: use the CI from the group bootstrap
    effect_boots <- group_boots - control_mean
    effect_ci_low <- stats::quantile(effect_boots, hrest$low)
    effect_ci_high <- stats::quantile(effect_boots, hrest$high)

    data.frame(
      group = g,
      effect = effect,
      effect_ci_low = as.numeric(effect_ci_low),
      effect_ci_high = as.numeric(effect_ci_high),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
