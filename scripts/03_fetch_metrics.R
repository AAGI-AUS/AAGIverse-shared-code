suppressPackageStartupMessages({
    library(here)
    library(readr)
    library(dplyr)
    library(jsonlite)
})

source(here("scripts/00_helpers.R"))

IN_CSV <- here("data/processed/submissions_cleaned.csv")
OUT_CSV <- here("data/processed/package_metrics.csv")

# Unauthenticated GitHub requests are capped at 60/hour. The workflow passes
# GITHUB_TOKEN, which lifts that to 1000/hour.
GH_TOKEN <- Sys.getenv("GITHUB_TOKEN", Sys.getenv("GITHUB_PAT", ""))
MAX_PAGES <- 50 # 5000 commits/contributors is plenty for these packages

# ------------------------------------------------------------
# HTTP helpers. Base download.file() + jsonlite keep us off new
# dependencies (httr2/curl are not in renv.lock).
# ------------------------------------------------------------

get_json <- function(url, github = FALSE) {
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp))
    headers <- c(Accept = "application/json", `User-Agent` = "AAGIverse-shared-code")
    if (github) {
        headers <- c(headers, `X-GitHub-Api-Version` = "2022-11-28")
        if (nzchar(GH_TOKEN)) headers <- c(headers, Authorization = paste("Bearer", GH_TOKEN))
    }
    res <- tryCatch(
        suppressWarnings(download.file(url, tmp, mode = "wb", quiet = TRUE, headers = headers)),
        error = function(e) 1L
    )
    if (!identical(res, 0L) || !file.exists(tmp)) return(NULL)
    tryCatch(fromJSON(tmp, simplifyVector = FALSE), error = function(e) NULL)
}

# Walk a paginated GitHub list endpoint and return every item.
gh_paginate <- function(path) {
    items <- list()
    for (page in seq_len(MAX_PAGES)) {
        sep <- if (grepl("?", path, fixed = TRUE)) "&" else "?"
        res <- get_json(sprintf("https://api.github.com/%s%sper_page=100&page=%d", path, sep, page), github = TRUE)
        if (is.null(res)) return(if (length(items)) items else NULL)
        items <- c(items, res)
        if (length(res) < 100) break
    }
    items
}

as_date_chr <- function(x) substr(x, 1, 10)

# ------------------------------------------------------------
# Per-source fetchers; each returns NA fields when data is unavailable
# ------------------------------------------------------------

fetch_github <- function(owner, repo) {
    out <- list(github_stars = NA_integer_, github_followers = NA_integer_,
                first_commit_date = NA_character_, last_commit_date = NA_character_,
                contributors = NA_integer_, github_repo = NA_character_)
    info <- get_json(sprintf("https://api.github.com/repos/%s/%s", owner, repo), github = TRUE)
    if (is.null(info)) {
        warning(sprintf("GitHub repo %s/%s not found or API unavailable", owner, repo), call. = FALSE)
        return(out)
    }
    out$github_repo <- info$full_name
    out$github_stars <- info$stargazers_count

    # Repos have no followers; this is the follower count of the owning
    # user/organisation.
    owner_info <- get_json(sprintf("https://api.github.com/users/%s", owner), github = TRUE)
    if (!is.null(owner_info)) out$github_followers <- owner_info$followers

    # Commits come newest-first, so first/last are the ends of the list.
    commits <- gh_paginate(sprintf("repos/%s/commits", info$full_name))
    if (length(commits)) {
        dates <- vapply(commits, function(x) x$commit$committer$date %||% NA_character_, character(1))
        out$last_commit_date <- as_date_chr(dates[1])
        out$first_commit_date <- as_date_chr(dates[length(dates)])
    }

    contribs <- gh_paginate(sprintf("repos/%s/contributors", info$full_name))
    if (!is.null(contribs)) out$contributors <- length(contribs)

    out
}

# The CRAN package name can differ from the repo name, so prefer the Package
# field in DESCRIPTION and fall back to the repo name.
cran_package_name <- function(repo_full, repo) {
    desc <- tryCatch(
        readLines(sprintf("https://raw.githubusercontent.com/%s/HEAD/DESCRIPTION", repo_full), warn = FALSE),
        error = function(e) NULL, warning = function(w) NULL
    )
    pkg <- if (length(desc)) sub("^Package:\\s*", "", grep("^Package:", desc, value = TRUE)[1]) else NA_character_
    if (is.na(pkg) || !nzchar(pkg)) repo else trimws(pkg)
}

fetch_cran <- function(pkg) {
    out <- list(cran_package = NA_character_, cran_downloads_last_month = NA_integer_,
                cran_downloads_total = NA_integer_)
    # cranlogs reports 0 for unknown packages, so check CRAN itself to tell
    # "not on CRAN" (NA) from "on CRAN with no downloads" (0).
    if (is.null(get_json(sprintf("https://crandb.r-pkg.org/%s", pkg))$Package)) return(out)
    out$cran_package <- pkg

    month <- get_json(sprintf("https://cranlogs.r-pkg.org/downloads/total/last-month/%s", pkg))
    if (length(month)) out$cran_downloads_last_month <- month[[1]]$downloads
    # cranlogs data begins 2012-10-01
    total <- get_json(sprintf("https://cranlogs.r-pkg.org/downloads/total/2012-10-01:%s/%s", Sys.Date(), pkg))
    if (length(total)) out$cran_downloads_total <- total[[1]]$downloads
    out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------
# Build metrics table, keyed on the software website
# ------------------------------------------------------------

df <- read_csv(IN_CSV, show_col_types = FALSE)
urls <- unique(df$`Software website`[!is.na(df$`Software website`)])

rows <- lapply(urls, function(url) {
    base <- list(`Software website` = url)
    gh <- parse_github_repo(url)
    if (is.null(gh)) {
        message("Skipping non-GitHub site: ", url)
        return(as_tibble(base))
    }
    message("Fetching metrics for ", gh$owner, "/", gh$repo)
    g <- fetch_github(gh$owner, gh$repo)
    pkg <- cran_package_name(g$github_repo %||% paste(gh$owner, gh$repo, sep = "/"), gh$repo)
    as_tibble(c(base, g, fetch_cran(pkg)))
})

metrics <- bind_rows(rows) |>
    mutate(metrics_updated = format(Sys.Date()))

write_csv(metrics, OUT_CSV)

cat("✓ Package metrics fetched for", nrow(metrics), "entries\n")
cat("  Output:", OUT_CSV, "\n")
