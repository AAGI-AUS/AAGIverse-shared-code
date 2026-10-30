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
# GITHUB_TOKEN, which lifts that to 1000/hour. GitLab and Codeberg work
# without a token; GITLAB_TOKEN is only needed for user follower counts.
TOKENS <- list(
    github = Sys.getenv("GITHUB_TOKEN", Sys.getenv("GITHUB_PAT", "")),
    gitlab = Sys.getenv("GITLAB_TOKEN", ""),
    codeberg = Sys.getenv("CODEBERG_TOKEN", "")
)
MAX_PAGES <- 50 # 2500-5000 commits/contributors is plenty for these packages

# ------------------------------------------------------------
# HTTP helpers. Base download.file() + jsonlite keep us off new
# dependencies (httr2/curl are not in renv.lock).
# ------------------------------------------------------------

forge_headers <- function(forge) {
    token <- TOKENS[[forge]]
    switch(forge,
        github = c(`X-GitHub-Api-Version` = "2022-11-28",
                   if (nzchar(token)) c(Authorization = paste("Bearer", token))),
        gitlab = if (nzchar(token)) c(`PRIVATE-TOKEN` = token),
        codeberg = if (nzchar(token)) c(Authorization = paste("token", token))
    )
}

# Forge requests that have failed so far. A repo whose fetch adds to this
# keeps its previous metrics rather than being written out with gaps.
failed_requests <- 0L

get_json <- function(url, forge = NULL) {
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp))
    headers <- c(Accept = "application/json", `User-Agent` = "AAGIverse-shared-code",
                 if (!is.null(forge)) forge_headers(forge))
    res <- tryCatch(
        suppressWarnings(download.file(url, tmp, mode = "wb", quiet = TRUE, headers = headers)),
        error = function(e) 1L
    )
    out <- if (identical(res, 0L) && file.exists(tmp)) {
        tryCatch(fromJSON(tmp, simplifyVector = FALSE), error = function(e) NULL)
    }
    if (is.null(out) && !is.null(forge)) failed_requests <<- failed_requests + 1L
    out
}

# The quota endpoint doesn't count against the quota. When the token is
# rejected or the hourly limit is spent, every GitHub request fails the same
# way, so say why once up front.
check_github_quota <- function() {
    core <- get_json("https://api.github.com/rate_limit", "github")$resources$core
    if (is.null(core)) {
        if (nzchar(TOKENS$github)) {
            warning("GitHub didn't accept the token in GITHUB_PAT/GITHUB_TOKEN. Check it hasn't expired.", call. = FALSE)
        }
    } else if (core$remaining == 0) {
        warning(sprintf("The GitHub API limit of %d requests/hour is used up until %s.%s",
                        core$limit, format(as.POSIXct(core$reset, origin = "1970-01-01"), "%H:%M"),
                        if (nzchar(TOKENS$github)) "" else " Set GITHUB_PAT to raise it to 5000."),
                call. = FALSE)
    }
}

# Walk a paginated list endpoint and return every item. GitHub and GitLab
# take per_page (up to 100); Forgejo, which runs Codeberg, takes limit and
# quietly caps it at 50.
paginate <- function(url, forge) {
    size <- if (forge == "codeberg") 50 else 100
    size_param <- if (forge == "codeberg") "limit" else "per_page"
    items <- list()
    for (page in seq_len(MAX_PAGES)) {
        sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
        res <- get_json(sprintf("%s%s%s=%d&page=%d", url, sep, size_param, size, page), forge)
        if (is.null(res)) return(if (length(items)) items else NULL)
        items <- c(items, res)
        if (length(res) < size) break
    }
    items
}

as_date_chr <- function(x) substr(x, 1, 10)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------
# Per-forge fetchers. Each takes a parse_repo() result and returns the same
# fields, NA where the data is unavailable. description_url is where the
# package's DESCRIPTION file lives; it's used for the CRAN lookup and is not
# written out.
# ------------------------------------------------------------

empty_metrics <- function(r) {
    list(forge = FORGES[[r$forge]]$label, repo = NA_character_,
         stars = NA_integer_, owner_followers = NA_integer_,
         first_commit_date = NA_character_, last_commit_date = NA_character_,
         contributors = NA_integer_, description_url = NA_character_)
}

with_commit_dates <- function(out, dates) {
    dates <- as_date_chr(dates[!is.na(dates)])
    if (length(dates)) {
        out$first_commit_date <- min(dates)
        out$last_commit_date <- max(dates)
    }
    out
}

fetch_github <- function(r) {
    out <- empty_metrics(r)
    api <- sprintf("https://api.github.com/repos/%s", r$path)
    info <- get_json(api, "github")
    if (is.null(info)) return(out)
    out$repo <- info$full_name
    out$stars <- info$stargazers_count

    # Repos have no followers; this is the follower count of the owning
    # user/organisation.
    owner_info <- get_json(sprintf("https://api.github.com/users/%s", r$owner), "github")
    if (!is.null(owner_info)) out$owner_followers <- owner_info$followers

    commits <- paginate(paste0(api, "/commits"), "github")
    out <- with_commit_dates(out, vapply(commits, function(x) x$commit$committer$date %||% NA_character_, character(1)))

    contribs <- paginate(paste0(api, "/contributors"), "github")
    if (!is.null(contribs)) out$contributors <- length(contribs)

    out$description_url <- sprintf("https://raw.githubusercontent.com/%s/HEAD/DESCRIPTION", info$full_name)
    out
}

fetch_gitlab <- function(r) {
    out <- empty_metrics(r)
    api <- sprintf("https://gitlab.com/api/v4/projects/%s", URLencode(r$path, reserved = TRUE))
    info <- get_json(api, "gitlab")
    if (is.null(info)) return(out)
    out$repo <- info$path_with_namespace
    out$stars <- info$star_count

    # Groups can't be followed, and gitlab.com only lists a user's followers
    # to signed-in requests, so this needs GITLAB_TOKEN.
    if (identical(info$namespace$kind, "user") && nzchar(TOKENS$gitlab)) {
        user <- get_json(sprintf("https://gitlab.com/api/v4/users?username=%s", info$namespace$path), "gitlab")
        if (length(user)) {
            followers <- paginate(sprintf("https://gitlab.com/api/v4/users/%s/followers", user[[1]]$id), "gitlab")
            if (!is.null(followers)) out$owner_followers <- length(followers)
        }
    }

    commits <- paginate(paste0(api, "/repository/commits"), "gitlab")
    out <- with_commit_dates(out, vapply(commits, function(x) x$committed_date %||% NA_character_, character(1)))

    # GitLab counts contributors by commit email rather than by account.
    contribs <- paginate(paste0(api, "/repository/contributors"), "gitlab")
    if (!is.null(contribs)) out$contributors <- length(contribs)

    out$description_url <- sprintf("https://gitlab.com/%s/-/raw/HEAD/DESCRIPTION", info$path_with_namespace)
    out
}

fetch_codeberg <- function(r) {
    out <- empty_metrics(r)
    api <- sprintf("https://codeberg.org/api/v1/repos/%s", r$path)
    info <- get_json(api, "codeberg")
    if (is.null(info)) return(out)
    out$repo <- info$full_name
    out$stars <- info$stars_count
    # The repo response carries the owner's follower count, for users and
    # organisations alike.
    out$owner_followers <- info$owner$followers_count

    # Diff stats, signature checks and file lists make each page slow to
    # build, and only the dates and authors are needed.
    commits <- paginate(paste0(api, "/commits?stat=false&verification=false&files=false"), "codeberg")
    out <- with_commit_dates(out, vapply(commits, function(x) x$commit$committer$date %||% NA_character_, character(1)))

    # Forgejo has no contributors endpoint, so count distinct commit authors:
    # their Codeberg account where the commit is linked to one, otherwise
    # their email.
    if (!is.null(commits)) {
        authors <- vapply(commits, function(x) x$author$login %||% tolower(x$commit$author$email %||% NA_character_), character(1))
        out$contributors <- length(unique(na.omit(authors)))
    }

    out$description_url <- paste0(api, "/raw/DESCRIPTION")
    out
}

# The CRAN package name can differ from the repo name, so prefer the Package
# field in DESCRIPTION and fall back to the repo name.
cran_package_name <- function(description_url, repo) {
    desc <- if (is.na(description_url)) NULL else tryCatch(
        readLines(description_url, warn = FALSE),
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

fetch_repo <- function(r) {
    switch(r$forge,
        github = fetch_github(r),
        gitlab = fetch_gitlab(r),
        codeberg = fetch_codeberg(r)
    )
}

# ------------------------------------------------------------
# Build metrics table, keyed on the software website
# ------------------------------------------------------------

df <- read_csv(IN_CSV, show_col_types = FALSE)
urls <- unique(df$`Software website`[!is.na(df$`Software website`)])
repos <- lapply(urls, parse_repo)

# Last run's metrics, so a repo that can't be fetched in full this time keeps
# its earlier values. Everything is handled as text so kept and fresh rows
# bind together; the pages' read_csv() restores the types.
previous <- if (file.exists(OUT_CSV)) read_csv(OUT_CSV, col_types = cols(.default = col_character()))

previous_row <- function(url) {
    if (is.null(previous) || !all(c("repo", "metrics_updated") %in% names(previous))) return(NULL)
    row <- previous[previous$`Software website` %in% url & !is.na(previous$repo), ]
    if (nrow(row)) row[1, ]
}

as_text <- function(x) if (is.na(x)) NA_character_ else format(x, scientific = FALSE, trim = TRUE)

if (any(vapply(repos, function(r) identical(r$forge, "github"), logical(1)))) check_github_quota()

rows <- Map(function(url, r) {
    base <- list(`Software website` = url)
    if (is.null(r)) {
        message("Skipping site not on GitHub, GitLab or Codeberg: ", url)
        return(as_tibble(base))
    }
    label <- FORGES[[r$forge]]$label
    message("Fetching ", label, " metrics for ", r$path)
    failed_before <- failed_requests
    m <- fetch_repo(r)
    if (failed_requests > failed_before) {
        kept <- previous_row(url)
        if (!is.null(kept)) {
            warning(sprintf("Couldn't fetch all %s metrics for %s; kept the values from %s",
                            label, r$path, kept$metrics_updated), call. = FALSE)
            return(kept)
        }
        warning(sprintf("Couldn't fetch all %s metrics for %s, and there are no earlier values to keep",
                        label, r$path), call. = FALSE)
    }
    pkg <- cran_package_name(m$description_url, r$repo)
    m$description_url <- NULL
    as_tibble(lapply(c(base, m, fetch_cran(pkg), metrics_updated = format(Sys.Date())), as_text))
}, urls, repos)

metrics <- bind_rows(rows)

write_csv(metrics, OUT_CSV)

cat("✓ Package metrics fetched for", nrow(metrics), "entries\n")
cat("  Output:", OUT_CSV, "\n")
