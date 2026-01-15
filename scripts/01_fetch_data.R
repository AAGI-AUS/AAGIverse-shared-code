#!/usr/bin/env Rscript

# ------------------------------------------------------------
# Project configuration (change here if needed)
# ------------------------------------------------------------

PUBLISHED_CSV_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQrMbNgIBmkS9Vgy-AWpHkYiCwqEW_IsE0jvUWMqTC2ha_j50eS68uc1AYKzegViEMZs32buIQGf8R4/pub?gid=1344631779&single=true&output=csv"
OUT_CSV <- "data/processed/submissions_raw.csv"
STAMP_FILE <- "data/processed/last_refresh.txt"

# ------------------------------------------------------------
# Fetch published Google Sheet CSV
# ------------------------------------------------------------

dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)

tmp <- tempfile(fileext = ".csv")
download.file(PUBLISHED_CSV_URL, tmp, mode = "wb", quiet = TRUE)

# Basic sanity check
if (!file.exists(tmp) || file.info(tmp)$size < 50) {
    stop("Downloaded file looks invalid. Check the published CSV URL.")
}

# Catch common mistake: HTML instead of CSV
first_line <- readLines(tmp, n = 1, warn = FALSE)
if (grepl("^\\s*<(!doctype|html)", tolower(first_line))) {
    stop("URL returned HTML, not CSV. Use 'Publish to web → CSV', not the share link.")
}

file.copy(tmp, OUT_CSV, overwrite = TRUE)

# Write refresh timestamp
stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
dir.create(dirname(STAMP_FILE), recursive = TRUE, showWarnings = FALSE)
writeLines(stamp, STAMP_FILE)

cat("✓ CSV fetched successfully\n")
cat("  Source:", PUBLISHED_CSV_URL, "\n")
cat("  Output:", OUT_CSV, "\n")
cat("  Updated:", stamp, "\n")
