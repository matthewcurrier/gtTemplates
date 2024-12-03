
# Load the gt package
library(gt)
library(gtExtras)
library(tidyverse)

# Make a gt table from us migration data
us_migration_table <- gt(us_migration)
us_migration_table

# Make a theme for the us_migration_table
us_migration_table |>
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.width = px(3),
    data_row.padding = px(6),
    heading.align = 'left',
    # column_labels.background.color = 'dodgerblue4',
    heading.title.font.size = px(26),
    heading.subtitle.font.size = px(14),
    table_body.hlines.width = px(0),
    table.border.top.width = px(0)

  ) |>
  tab_style(
    style = cell_text(
      color = "black",
      weight = "bold",
      transform = "uppercase"),
    locations = list(
      cells_column_labels(everything())
    )) |>
  # Format pct columns as a percentage
  fmt_percent(
    columns = starts_with("pct_"),
    decimals = 1
  ) |>
  # Make the name column text bold
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(name)
    )
  ) |>
  # Make all columns that start with m_ as numbers with commas
  fmt_number(
    columns = starts_with("m_"),
    decimals = 0
  ) |>
  # Format all column widths that start with pct_ or m_ as 90 pixels
  cols_width(
    starts_with("pct_") ~ px(90),
    starts_with("m_") ~ px(90),
    everything() ~ px(120)
  ) |>
  cols_align(
    columns = matches("^(pct_|m_)"),
    align = "right"
  ) |>
  # Style negative values
  tab_style(
    style = cell_text(color = '#8b1a1a', weight = 'bold'),
    locations = cells_body(
      columns = c(pct_births_change),
      rows =  (pct_births_change < 0)
    )
  ) |>
  # Style non-negative values
  tab_style(
    style = cell_text(color = '#2E8B57', weight = 'bold'),
    locations = cells_body(
      columns = c(pct_births_change),
      rows =  (pct_births_change > 0)
    )
  ) |>
  cols_label(
    name = "STATE",
    year = "YR",
    m_births = "BTHS",
    m_deaths = "DTHS",
    pct_births_change = "Δ BRTHS",
    pct_internationalmig_change = "Δ INTL",
    pct_domesticmig_change = "Δ DOM"
  ) |>
  opt_table_font(
    font = list(
      google_font(name = "Merriweather"),
      "Cochin", "serif"
    )
  ) |>
  tab_style(
    style = cell_text(font = google_font("Arial")),
    locations = cells_body(
      columns = where(is.numeric)
    )
  ) |>
  opt_table_font(font = "Arial") |>
  tab_header(
    title = "US Migration Data",
    subtitle = "Migration data for the United States from 2010-2019"
  ) |>
  tab_style(
    style = cell_text(
      weight = 600
      ),
    locations = cells_title(
      groups = c("title")
      )
  )


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
      column_labels.border.bottom.width = px(3),

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
