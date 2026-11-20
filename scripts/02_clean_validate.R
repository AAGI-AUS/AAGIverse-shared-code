suppressPackageStartupMessages({
    library(here)
    library(readr)
    library(dplyr)
    library(stringr)
})

source(here("scripts/00_helpers.R"))

# Approved entries live in the repo. New ones are added from Microsoft Forms
# exports with scripts/import_form_responses.R (see data/README.md).
ENTRIES_CSV <- here("data/submissions/entries.csv")
QUESTIONS_CSV <- here("data/submissions/form_questions.csv")
OUT_CSV <- here("data/processed/submissions_cleaned.csv")
STAMP_FILE <- here("data/processed/last_refresh.txt")

questions <- read_csv(QUESTIONS_CSV, col_types = cols(.default = "c"))
# Columns starting with "_" are only used while importing and aren't stored.
stored <- filter(questions, !startsWith(column, "_"))
required <- stored$column[stored$required == "yes"]

df <- read_csv(ENTRIES_CSV, col_types = cols(.default = "c"), na = c("", "NA")) |>
    mutate(across(everything(), ~ na_if(str_squish(.x), ""))) |>
    distinct()

# Collect every problem before stopping, so one run shows them all.
problems <- character(0)

missing_cols <- setdiff(stored$column, names(df))
if (length(missing_cols)) {
    problems <- c(problems, paste("Missing column(s):", paste(missing_cols, collapse = ", ")))
}

for (col in intersect(required, names(df))) {
    rows <- which(is.na(df[[col]]))
    if (length(rows)) {
        # +1 for the header line, so the number matches the line in the file.
        problems <- c(problems, sprintf("'%s' is empty on line(s) %s", col, paste(rows + 1, collapse = ", ")))
    }
}

if ("Software website" %in% names(df)) {
    url <- df$`Software website`
    bad <- which(!is.na(url) & !grepl("^https?://[^ ]+$", url))
    if (length(bad)) {
        problems <- c(problems, sprintf("'Software website' isn't a http(s) URL on line(s) %s",
                                        paste(bad + 1, collapse = ", ")))
    }
    # Same rule as the import script: ignore http(s)://, www. and a trailing /.
    site <- tolower(sub("/+$", "", sub("^https?://(www\\.)?", "", url)))
    dup <- which(!is.na(url) & duplicated(site))
    if (length(dup)) {
        problems <- c(problems, sprintf("'Software website' repeats an earlier entry on line(s) %s",
                                        paste(dup + 1, collapse = ", ")))
    }
}

if (length(problems)) {
    stop("data/submissions/entries.csv has problems:\n  - ",
         paste(problems, collapse = "\n  - "), call. = FALSE)
}

dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
write_csv(df, OUT_CSV)
writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), STAMP_FILE)

cat(sprintf("✓ %d entries cleaned and validated\n", nrow(df)))
