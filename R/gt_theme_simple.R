#' A basic gt table theme that captures personal preferences
#'
#' @param gt_object a gt object created by the gt package
#' @param ... additional arguments to pass
#'
#' @return A 'gt_tbl' object with the specified theme applied
#' @export
#' @import gt
#'
#' @examples
#' mtcars |>
#' gt::gt() |>
#' gt_theme_simple()
gt_theme_simple <- function(gt_object, ...) {
  # Check if gt_object is a gt_tbl
  stopifnot(
    `'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
      class(gt_object)
  )

  gt_object |>
    tab_options(
      heading.align = 'left',
      heading.title.font.size = px(26),
      heading.subtitle.font.size = px(14),

      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.width = px(2),

      data_row.padding = px(6),

      table_body.hlines.width = px(0),
      table.border.top.width = px(0)

    ) |>
    tab_style(
      style = cell_text(
        weight = 600
      ),
      locations = cells_title(
        groups = c("title")
      )
    )
}
