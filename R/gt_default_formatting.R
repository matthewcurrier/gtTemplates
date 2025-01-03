#' Create a gt object with your preferential defaults
#'
#' @param gt_object a gt object
#' @param metric_cols columns from gt object that should be formatted
#' as numbers. Defaults to zero decimals.
#' @param pct_cols columns from gt object that should be formatted as percents.
#' Defaults to zero decimals.
#' @param ... other args to pass
#' @import gt
#' @import dplyr
#' @import tidyselect
#' @return returns a gt object with columns formatted
#' @export
#'
#' @examples
#' library(gt)
#' library(gtTemplates)
#' library(dplyr)
#' gt_default_formatting(gt::gt(us_migration))
#' gt_default_formatting(gt::gt(us_migration), metric_cols = c("m_births", "m_deaths"))
#' gt_default_formatting(gt::gt(mtcars), pct_cols = "dd")
gt_default_formatting <- function(gt_object, key_col, metric_cols, pct_cols, ...) {

  stopifnot(
    `'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
      class(gt_object)
  )

  # If no metric cols vector provided, just isolate columns which start with m_
  if(missing(metric_cols)) {
    m_cols <- gt_object$`_data` |>
      dplyr::select(matches("^m_")) |>
      names()

    # if(length(m_cols) == 0) {
    #   # just select numeric columns that aren't decimal based
    #   m_cols <- gt_object$`_data` |>
    #     dplyr::select(where(is.numeric)) |>
    #     names()
    #
    # }
    metric_cols <- m_cols
    # print message indicating you just selected cols that start with _m
    message("No metrics cols were supplied. Using these cols: ", paste0(m_cols, collapse = ", "))
  }

  if(missing(pct_cols)) {
    p_cols <- gt_object$`_data` |>
      dplyr::select(matches("^pct_")) |>
      names()

    # if(length(p_cols) == 0) {
    #   p_cols <- gt_object$`_data` |>
    #     dplyr::select(where(is.numeric)) |>
    #     names()
    # }
    pct_cols <- p_cols
    # print message indicating you just selected cols that start with _m
    message("No percent cols were supplied. Using these cols: ", paste0(p_cols, collapse = ", "))
  }

  if(missing(key_col)) {
    key_col <- gt_object$`_data`[,1] |>
      names()
    message("No key column was supplied. Using the first col: ", key_col)
  }

  gt_object |>
    fmt_number(
      columns = any_of(metric_cols),
      decimals = 0
    ) |>
    fmt_percent(
      columns = any_of(pct_cols),
      decimals = 0
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = any_of(key_col)
      )
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels()
    )
}
