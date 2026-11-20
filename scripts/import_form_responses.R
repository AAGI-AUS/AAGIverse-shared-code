#!/usr/bin/env Rscript

# Add new Microsoft Forms responses to data/submissions/entries.csv.
#
# From a Forms export (Responses > Open results in Excel):
#
#   Rscript scripts/import_form_responses.R [path/to/export.xlsx]
#
#   With no path, it uses the newest .xlsx or .csv in data/raw/, which is
#   gitignored, so exports (emails, unreviewed answers) never get committed.
#
# From one response sent by Power Automate (what the form-submission
# workflow runs):
#
#   Rscript scripts/import_form_responses.R --payload payload.json [--summary summary.md]
#
#   payload.json is {"response": {"<column or question>": "<answer>", ...}}.
#   --summary writes a short markdown description of what was added, for the
#   pull request. It never includes email addresses.
#
# Responses already in entries.csv are skipped, so importing the same export
# twice adds nothing. See data/README.md.

suppressPackageStartupMessages({
    library(here)
    library(readr)
    library(dplyr)
    library(stringr)
    library(jsonlite)
})

source(here("scripts/00_helpers.R"))  # derive_name()

ENTRIES_CSV <- here("data/submissions/entries.csv")
QUESTIONS_CSV <- here("data/submissions/form_questions.csv")

`%|%` <- function(a, b) ifelse(is.na(a), b, a)

# ---- Arguments ------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
flag_value <- function(flag) {
    i <- match(flag, args)
    if (is.na(i)) return(NULL)
    if (i == length(args)) stop(flag, " needs a file path after it.", call. = FALSE)
    args[i + 1]
}
payload_path <- flag_value("--payload")
summary_path <- flag_value("--summary")
positional <- args[!args %in% c("--payload", "--summary", payload_path, summary_path)]

# ---- Read the responses ---------------------------------------------------------

read_export <- function(path) {
    if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
            stop("Reading .xlsx needs the readxl package: install.packages(\"readxl\").\n",
                 "Or save the export as CSV (UTF-8) in Excel and pass that instead.", call. = FALSE)
        }
        readxl::read_excel(path, col_types = "text", .name_repair = "minimal")
    } else {
        read_csv(path, col_types = cols(.default = "c"), name_repair = "minimal")
    }
}

# A payload is one response. Each answer becomes one column of a one-row
# table, so it goes through exactly the same matching as an export.
read_payload <- function(path) {
    p <- fromJSON(path, simplifyVector = FALSE)
    r <- p$response
    if (!is.list(r) || !length(r) || is.null(names(r))) {
        stop("The payload needs a \"response\" object holding the answers, e.g.\n",
             "  {\"response\": {\"Code maintainer\": \"...\", \"Organisation\": \"...\"}}\n",
             "Top-level keys found: ", paste(names(p), collapse = ", "), call. = FALSE)
    }
    values <- lapply(r, function(v) {
        if (is.null(v) || !length(v)) return(NA_character_)
        # Forms may send checkbox answers as an actual list.
        if (is.list(v) || length(v) > 1) return(toJSON(unlist(v), auto_unbox = FALSE))
        as.character(v)
    })
    as_tibble(values, .name_repair = "minimal")
}

from_payload <- !is.null(payload_path)
if (from_payload) {
    if (!file.exists(payload_path)) stop("Payload not found: ", payload_path, call. = FALSE)
    export <- read_payload(payload_path)
} else {
    path <- if (length(positional)) positional[1] else {
        found <- list.files(here("data/raw"), pattern = "\\.(xlsx|csv)$", full.names = TRUE, ignore.case = TRUE)
        if (!length(found)) {
            stop("No export given, and no .xlsx or .csv in data/raw/.\n",
                 "Download the responses from Microsoft Forms (Responses > Open results in Excel)\n",
                 "into data/raw/, or pass the file's path.", call. = FALSE)
        }
        found[which.max(file.mtime(found))]
    }
    if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
    message("Reading ", path)
    export <- read_export(path)
}

# ---- Match form questions to entry columns ------------------------------------

questions <- read_csv(QUESTIONS_CSV, col_types = cols(.default = "c"))

# Compare ignoring case, punctuation and spacing, so small edits in the form
# (a missing "?", an extra space) don't break the import. "_ip_check" and
# "IP check" also compare equal.
norm <- function(x) str_squish(gsub("[^a-z0-9]+", " ", tolower(x)))
export_q <- norm(names(export))

# A payload names fields by their entry column ("Email"), an export by their
# question text. Exports are matched on question text only: Forms adds its
# own "Email" and "Name" columns (the signed-in responder, usually
# "anonymous"), which must not be mistaken for the answers.
find_field <- function(column, question) {
    hit <- if (from_payload) which(export_q == norm(column)) else integer(0)
    if (!length(hit)) hit <- which(export_q == norm(question))
    # Forms can add text after a question title, so allow a prefix match.
    if (!length(hit)) hit <- which(startsWith(export_q, norm(question)))
    if (length(hit)) hit[1] else NA_integer_
}
questions$export_col <- mapply(find_field, questions$column, questions$question, USE.NAMES = FALSE)

# Every question has to be found, optional ones too: "optional" means the
# answer can be blank, and a missed question would silently empty a column
# for every imported entry.
unmatched <- filter(questions, is.na(export_col))
if (nrow(unmatched)) {
    # Printed with message(), because stop() cuts its text off at 1000 characters.
    source_name <- if (from_payload) "payload" else "export"
    message("These fields in data/submissions/form_questions.csv aren't in the ", source_name, ":\n  - ",
            paste(sprintf("%s: %s", unmatched$column, unmatched$question), collapse = "\n  - "),
            "\n\nThe ", source_name, "'s fields are:\n",
            paste(sprintf("  %2d. %s", seq_along(export), str_squish(names(export))), collapse = "\n"),
            if (from_payload) {
                "\n\nAdd the missing keys to the Compose action in the Power Automate flow (see data/README.md)."
            } else {
                "\n\nIf a question was reworded in the form, copy its new text into form_questions.csv."
            })
    stop("Fields don't match, see above.", call. = FALSE)
}

if (from_payload) {
    extra <- setdiff(seq_along(export), questions$export_col)
    if (length(extra)) {
        message("Ignoring payload keys that aren't in form_questions.csv: ",
                paste(names(export)[extra], collapse = ", "))
    }
}

# Checkbox answers arrive as "A;B;" in an export and as '["A","B"]' from
# Power Automate. The site wants "A, B".
multi_to_commas <- function(x) {
    vapply(x, function(v) {
        if (is.na(v)) return(NA_character_)
        parts <- if (startsWith(v, "[")) {
            tryCatch(as.character(unlist(fromJSON(v))), error = function(e) strsplit(v, ";", fixed = TRUE)[[1]])
        } else {
            strsplit(v, ";", fixed = TRUE)[[1]]
        }
        parts <- str_squish(parts)
        paste(parts[nzchar(parts)], collapse = ", ")
    }, character(1), USE.NAMES = FALSE)
}

incoming <- as_tibble(lapply(seq_len(nrow(questions)), function(i) {
    x <- na_if(str_squish(as.character(export[[questions$export_col[i]]])), "")
    if (questions$multi[i] == "yes") x <- na_if(multi_to_commas(x), "")
    x
}), .name_repair = "minimal")
names(incoming) <- questions$column

# ---- Skip what can't or needn't be imported ------------------------------------

entries <- read_csv(ENTRIES_CSV, col_types = cols(.default = "c"), na = c("", "NA"))

# An entry is the same submission if it has the same website, or, with no
# website, the same maintainer and description.
entry_key <- function(d) {
    site <- tolower(sub("/+$", "", sub("^https?://(www\\.)?", "", d$`Software website`)))
    ifelse(is.na(site), paste(tolower(d$`Code maintainer`), tolower(d$Description), sep = "|"), site)
}

# Workflow logs are public, so in payload mode name responses by their
# website only.
label <- function(r) {
    site <- r$`Software website` %|% "no website"
    if (from_payload) site else sprintf("%s (%s), %s", r$`Code maintainer` %|% "?", r$Organisation %|% "?", site)
}

not_shareable <- tolower(coalesce(incoming$`_ip_check`, "")) == "no"
already_in <- entry_key(incoming) %in% entry_key(entries)
repeated <- duplicated(entry_key(incoming))
blank <- rowSums(!is.na(select(incoming, -starts_with("_")))) == 0

for (i in which(not_shareable & !already_in)) {
    message("Skipped (answered No to the IP question): ", label(incoming[i, ]))
}
if (from_payload && any(already_in)) {
    message("Skipped (already in entries.csv): ", label(incoming[which(already_in)[1], ]))
}

new <- incoming[!(not_shareable | already_in | repeated | blank), ]

if (!nrow(new)) {
    message(sprintf("No new responses: %d received, all already in entries.csv or skipped.", nrow(incoming)))
    quit(save = "no")
}

# ---- Report and write ---------------------------------------------------------

follow_ups <- function(r) {
    notes <- character(0)
    if (grepl("not sure", r$`_ip_check` %|% "", ignore.case = TRUE)) notes <- c(notes, "unsure about IP")
    if (grepl("not sure|unlicensed", r$License %|% "", ignore.case = TRUE)) notes <- c(notes, "needs a licence")
    req <- questions$column[questions$required == "yes" & !startsWith(questions$column, "_")]
    missing <- req[vapply(req, function(m) is.na(r[[m]]), logical(1))]
    if (length(missing)) notes <- c(notes, paste("missing", paste(missing, collapse = ", ")))
    notes
}

message(sprintf("\nAdding %d response(s) to data/submissions/entries.csv:", nrow(new)))
for (i in seq_len(nrow(new))) {
    notes <- follow_ups(new[i, ])
    message("  + ", label(new[i, ]),
            if (length(notes)) paste0("  <- follow up: ", paste(notes, collapse = "; ")) else "")
}

if (!is.null(summary_path)) {
    md <- unlist(lapply(seq_len(nrow(new)), function(i) {
        r <- new[i, ]
        notes <- follow_ups(r)
        c(sprintf("### %s", derive_name(r$`Software website`) %|% (r$`Code maintainer` %|% "New entry")),
          "",
          sprintf("- **Maintainer:** %s (%s)", r$`Code maintainer` %|% "?", r$Organisation %|% "?"),
          sprintf("- **Website:** %s", r$`Software website` %|% "none given"),
          sprintf("- **Type / language:** %s / %s", r$`Code type` %|% "?", r$Language %|% "?"),
          sprintf("- **Licence:** %s", r$License %|% "?"),
          sprintf("- **IP register checked:** %s", r$`_ip_check` %|% "?"),
          if (length(notes)) sprintf("- :warning: **Follow up:** %s", paste(notes, collapse = "; ")),
          "",
          sprintf("> %s", r$Description %|% ""),
          "")
    }))
    writeLines(md, summary_path)
}

# Columns starting with "_" (the IP answer) are only for these checks.
out <- bind_rows(entries, select(new, -starts_with("_")))
write_csv(out, ENTRIES_CSV, na = "")

if (!from_payload) {
    message("\nNext: review with `git diff data/submissions/entries.csv`, delete any row that",
            "\nshouldn't be published, run `Rscript scripts/02_clean_validate.R`, then commit.")
}
