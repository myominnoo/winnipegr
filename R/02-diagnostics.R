# Module: Comprehensive column diagnostics for all datasets in a list ----

# Helper 1: Summarise a single column ----
# Helper 1: Summarise a single column ----
summarise_column <- function(col, col_name) {
  n_total     <- base::length(col)
  n_missing   <- base::sum(base::is.na(col) | (base::is.character(col) & stringr::str_squish(col) == ""))
  pct_missing <- base::round((n_missing / n_total) * 100, 1)
  n_valid     <- n_total - n_missing
  
  base_info <- tibble::tibble(
    column      = col_name,
    type        = base::class(col)[1],
    n_total     = n_total,
    n_valid     = n_valid,
    n_missing   = n_missing,
    pct_missing = pct_missing
  )
  
  # Numeric columns ----
  if (base::is.numeric(col)) {
    stats <- tibble::tibble(
      min        = base::as.character(base::round(base::min(col,      na.rm = TRUE), 2)),
      max        = base::as.character(base::round(base::max(col,      na.rm = TRUE), 2)),
      mean       = base::as.character(base::round(base::mean(col,     na.rm = TRUE), 2)),
      median     = base::as.character(base::round(stats::median(col,  na.rm = TRUE), 2)),
      sd         = base::as.character(base::round(stats::sd(col,      na.rm = TRUE), 2)),
      n_unique   = dplyr::n_distinct(col, na.rm = TRUE),
      categories = NA_character_
    )
    
    # Date / datetime columns ----
  } else if (base::inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
    stats <- tibble::tibble(
      min        = base::as.character(base::min(col, na.rm = TRUE)),
      max        = base::as.character(base::max(col, na.rm = TRUE)),
      mean       = NA_character_,
      median     = NA_character_,
      sd         = NA_character_,
      n_unique   = dplyr::n_distinct(col, na.rm = TRUE),
      categories = NA_character_
    )
    
    # Character / factor columns ----
  } else if (base::is.character(col) | base::is.factor(col)) {
    clean_col <- col[!base::is.na(col) & stringr::str_squish(col) != ""]
    n_unique  <- dplyr::n_distinct(clean_col)
    top_cats  <- base::sort(base::table(clean_col), decreasing = TRUE)
    
    # Show all categories if ≤ 10, otherwise top 10 ----
    show_cats  <- base::min(n_unique, 10)
    cat_string <- base::paste(
      base::names(top_cats)[1:show_cats],
      base::paste0("(n=", top_cats[1:show_cats], ")"),
      collapse = ", "
    )
    if (n_unique > 10) cat_string <- base::paste0(cat_string, " ... +", n_unique - 10, " more")
    
    stats <- tibble::tibble(
      min        = NA_character_,
      max        = NA_character_,
      mean       = NA_character_,
      median     = NA_character_,
      sd         = NA_character_,
      n_unique   = n_unique,
      categories = cat_string
    )
    
    # List columns ----
  } else {
    stats <- tibble::tibble(
      min        = NA_character_,
      max        = NA_character_,
      mean       = NA_character_,
      median     = NA_character_,
      sd         = NA_character_,
      n_unique   = dplyr::n_distinct(col, na.rm = TRUE),
      categories = "<list column>"
    )
  }
  
  dplyr::bind_cols(base_info, stats)
}

# Helper 2: Diagnose all columns in a single dataframe ----
diagnose_dataset <- function(df, dataset_name) {
  if (base::is.null(df)) {
    base::message("  ⚠ Skipping ", dataset_name, " — NULL (failed to load)")
    return(NULL)
  }
  
  purrr::imap_dfr(df, \(col, col_name) {
    tryCatch(
      summarise_column(col, col_name),
      error = \(e) {
        base::message("  ⚠ Could not summarise column: ", col_name, " → ", base::conditionMessage(e))
        NULL
      }
    )
  }) |>
    dplyr::mutate(dataset = dataset_name, .before = 1)
}

# Module 6: Run diagnostics across all datasets ----
check_all <- function(data_list, print_each = TRUE) {
  
  results <- purrr::imap(data_list, \(df, name) {
    
    if (print_each) {
      base::message("\n", base::strrep("─", 60))
      base::message("Checking: ", name,
                    " [", base::nrow(df), " rows × ", base::ncol(df), " cols]")
      base::message(base::strrep("─", 60))
    }
    
    diag <- diagnose_dataset(df, name)
    
    if (print_each && !base::is.null(diag)) {
      # Flag columns with any missing values ----
      flagged <- diag |> dplyr::filter(pct_missing > 0)
      if (base::nrow(flagged) > 0) {
        base::message("  Columns with missing values:")
        purrr::pwalk(flagged, \(column, pct_missing, n_missing, ...) {
          base::message("    ▸ ", column, ": ", pct_missing, "% missing (n=", n_missing, ")")
        })
      } else {
        base::message("  ✓ No missing values detected")
      }
    }
    
    diag
  })
  
  # Combine all into one master diagnostics table ----
  combined <- purrr::list_rbind(results)
  base::message("\n", base::strrep("─", 60))
  base::message("Diagnostics complete: ",
                base::nrow(combined), " columns across ",
                base::length(data_list), " datasets")
  base::message(base::strrep("─", 60))
  
  base::invisible(combined)
}

# Run ----
diagnostics <- check_all(data_raw_list)

# # Explore results ----
# 
# # All columns with > 20% missing across all datasets ----
# diagnostics |>
#   dplyr::filter(pct_missing > 20) |>
#   dplyr::select(dataset, column, type, n_total, n_missing, pct_missing) |>
#   dplyr::arrange(dplyr::desc(pct_missing))
# 
# # All categorical columns and their valid values ----
# diagnostics |>
#   dplyr::filter(type == "character", !base::is.na(categories)) |>
#   dplyr::select(dataset, column, n_unique, categories)
# 
# # Numeric summary for a specific dataset ----
# diagnostics |>
#   dplyr::filter(dataset == "assessment_parcels", type %in% c("numeric", "integer")) |>
#   dplyr::select(column, min, max, mean, median, sd, n_missing, pct_missing)
# 
# # Save diagnostics report ----
# readr::write_csv(diagnostics, "data/diagnostics_report.csv")