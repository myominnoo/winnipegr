

# API Calls functions -----------------------------------------------------

# Module 1: Build URL with optional limit ----
build_url <- function(url, limit = NULL) {
  if (!base::is.null(limit)) {
    base::paste0(url, "?$limit=", limit)
  } else {
    url  # no limit → RSocrata auto-paginates all rows
  }
}

# Module 2: Fetch a single dataset with error handling ----
fetch_socrata <- function(url, limit = NULL) {
  final_url <- build_url(url, limit)
  base::message("Fetching: ", final_url)
  tryCatch(
    expr    = RSocrata::read.socrata(final_url),
    error   = \(e) {
      base::message("ERROR fetching ", final_url, "\n  → ", base::conditionMessage(e))
      NULL
    },
    warning = \(w) {
      base::message("WARNING fetching ", final_url, "\n  → ", base::conditionMessage(w))
      RSocrata::read.socrata(final_url)  # retry on warning
    }
  )
}

# Module 3: Report failed datasets after loading ----
report_failures <- function(data_list) {
  failed <- base::names(purrr::keep(data_list, base::is.null))
  if (base::length(failed) > 0) {
    base::message("\nFailed to load: ", base::paste(failed, collapse = ", "))
  } else {
    base::message("\nAll datasets loaded successfully.")
  }
  base::invisible(failed)
}

# Module 4: Fetch all datasets with per-dataset limits ----
fetch_all_socrata <- function(api_list, api_limits = NULL) {
  
  # if no limits provided, fetch all rows for every dataset
  if (base::is.null(api_limits)) {
    data_list <- purrr::map(api_list, fetch_socrata)
    
    # if limits provided, apply per-dataset limits via map2  
  } else {
    data_list <- purrr::map2(
      api_list,
      api_limits,
      \(url, lim) fetch_socrata(url, limit = lim)
    )
  }
  
  report_failures(data_list)
  data_list
}



# Module 5: Glimpse all datasets in a list ----
glimpse_all <- function(data_list) {
  purrr::iwalk(data_list, \(df, name) {
    base::message("\n", base::strrep("─", 60))
    base::message("Dataset: ", name)
    base::message(base::strrep("─", 60))
    if (base::is.null(df)) {
      base::message("  ⚠ NULL — dataset failed to load")
    } else {
      dplyr::glimpse(df)
    }
  })
  base::invisible(data_list)  # return list silently for piping
}




# DT helpers ------------------------------------------------------------

# Render a searchable, filterable DT table ----
make_dt <- function(df,
                    page_length = 15,
                    filter      = "top",
                    rownames    = FALSE,
                    ...) {
  DT::datatable(
    df,
    filter   = filter,
    rownames = rownames,
    options  = list(
      pageLength = page_length,
      autoWidth  = TRUE,
      scrollX    = TRUE        # handles wide tables like diagnostics
    ),
    ...                        # pass any extra DT::datatable() args
  )
}



