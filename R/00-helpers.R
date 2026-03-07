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
# httr2_keys: character vector of api_list names to route through fetch_all_pages
#             instead of RSocrata. Limits are ignored for these — fetch_all_pages
#             handles pagination internally via its own page_size argument.
fetch_all_socrata <- function(api_list, api_limits = NULL, httr2_keys = NULL) {
  
  # Normalise limits to a named list of NULLs when not supplied
  limits <- if (base::is.null(api_limits)) {
    stats::setNames(vector("list", base::length(api_list)), base::names(api_list))
  } else {
    api_limits
  }
  
  # Per-dataset dispatcher ----
  fetch_one <- function(name, url, lim) {
    if (!base::is.null(httr2_keys) && name %in% httr2_keys) {
      base::message("Fetching (httr2 paginated): ", url)
      tryCatch(
        expr  = fetch_all_pages(url),
        error = \(e) {
          base::message("ERROR fetching ", url, "\n  → ", base::conditionMessage(e))
          NULL
        }
      )
    } else {
      fetch_socrata(url, limit = lim)
    }
  }
  
  # Dispatch over every dataset by name so httr2_keys matching works
  data_list <- purrr::imap(
    api_list,
    \(url, name) fetch_one(name, url, limits[[name]])
  )
  
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

fetch_all_pages <- function(base_url, page_size = 1000L) {
  require(httr2)
  library(tidyverse)
  offset  <- 0L
  pages   <- list()

  repeat {
    resp <- request(base_url) |>
      req_url_query(
        `$limit`  = page_size,
        `$offset` = offset,
        `$order`  = ":id"
      ) |>
      req_perform()

    page <- resp |> resp_body_json(simplifyVector = TRUE) |> as_tibble()

    if (nrow(page) == 0L) break

    pages  <- c(pages, list(page))
    offset <- offset + page_size

    message(sprintf("Fetched %d rows so far...", offset))

    if (nrow(page) < page_size) break
  }

  bind_rows(pages)
}

fetch_one_page <- function(base_url) {
  require(httr2)
  library(tidyverse)

  resp <- request(base_url) |>
    req_perform()

  page <- resp |> resp_body_json(simplifyVector = TRUE) |> as_tibble()

}

# 03-data-process helpers ---------------------------------------------------------

clean_neighbourhood <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_squish()
}

