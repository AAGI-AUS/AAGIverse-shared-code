#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(here)
    library(readr)
    library(dplyr)
    library(janitor)
    library(stringr)
    library(lubridate)
})

source("scripts/00_helpers.R")

# ------------------------------------------------------------
# Project configuration (change here if needed)
# ------------------------------------------------------------
HEADER_KEY_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQrMbNgIBmkS9Vgy-AWpHkYiCwqEW_IsE0jvUWMqTC2ha_j50eS68uc1AYKzegViEMZs32buIQGf8R4/pub?gid=267641157&single=true&output=csv"
RAW_CSV <- here("data/raw/submissions_raw.csv")
OUT_CSV <- here("data/processed/submissions_cleaned.csv")


header_key <- read_csv(HEADER_KEY_URL) |> clean_names()

df <- read_csv(RAW_CSV, show_col_types = FALSE) |>
    # clean_names() |>
    mutate(across(where(is.character), ~ str_squish(.x))) |>
    distinct()

# Rename columns based on header key
header_lookup <- setNames(header_key$submission_question, header_key$dt_display_header)

df <- df |>
    rename(all_of(header_lookup)) #|> 
    # mutate(`Code link` = make_hyperlink(`Code link`),
            # Website = make_hyperlink(Website)) 

dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
write_csv(df, OUT_CSV)

cat("✓ Data cleaned and validated successfully\n")
