#!/usr/bin/env Rscript

# Pull the latest submissions and package metrics into data/, the same steps
# the daily workflow runs before it renders. To preview the site with
# today's data:
#
#   Rscript scripts/refresh_data.R && quarto preview

library(here)

# 01_fetch_data.R writes to paths relative to the project root.
setwd(here())

# Unauthenticated GitHub requests are capped at 60/hour, which a few
# refreshes in a row will use up. Borrow the GitHub CLI's token if there is one.
if (!nzchar(Sys.getenv("GITHUB_TOKEN")) && !nzchar(Sys.getenv("GITHUB_PAT")) && nzchar(Sys.which("gh"))) {
    token <- tryCatch(
        suppressWarnings(system2("gh", c("auth", "token"), stdout = TRUE, stderr = FALSE)),
        error = function(e) character(0)
    )
    if (length(token) && nzchar(token[1])) Sys.setenv(GITHUB_PAT = token[1])
}
if (!nzchar(Sys.getenv("GITHUB_TOKEN")) && !nzchar(Sys.getenv("GITHUB_PAT"))) {
    message("No GitHub token found, so GitHub metrics are limited to 60 requests/hour.\n",
            "Add GITHUB_PAT=<token> to ~/.Renviron (a token with no scopes is enough).")
}

for (script in c("01_fetch_data.R", "02_clean_validate.R", "03_fetch_metrics.R")) {
    message("\n── ", script)
    source(here("scripts", script), local = new.env())
}

# quarto preview reuses each page's last R output from this cache for as
# long as the page's .qmd is unchanged, whatever the page's freeze setting.
# New data doesn't change the .qmd, so clear the cache or preview keeps
# showing the old data.
unlink(here(".quarto", "_freeze"), recursive = TRUE)
