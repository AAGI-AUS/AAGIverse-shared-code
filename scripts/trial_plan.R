# Helpers for the trial plan page (database/field.qmd).
#
# Each package is drawn as a plot in a field trial. Blocks are
# organisations. The crop in a plot shows how recently the package was
# worked on (growth stage) and how many people contribute (one plant per
# contributor).
#
# Everything here is drawn at the origin. assets/field.js works out where
# plots go for the current screen size.

library(dplyr)
library(htmltools)

# Sizes in plan units. field.js reads them from the svg's data attributes,
# so this is the only place they need changing.
PLOT_W <- 14
PLOT_H <- 8.5
COL_GAP <- 1
RANGE_GAP <- 2
BLOCK_GAP <- 10
PLAN_PAD <- 8

svg_node <- function(name, ..., .noWS = NULL) {
  tag(name, list(...), .noWS = .noWS)
}

format_count <- function(x) {
  if (is.na(x)) return("—")
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_date <- function(x, fmt) {
  if (is.na(x)) return("—")
  format(as.Date(x), fmt)
}


# Plots and blocks ----------------------------------------------------------

# Adds a display name, block, block letter and plot id to each entry.
# Blocks are ordered biggest first, and plots within a block by most
# recent commit. Ids (A01, A02, ...) are fixed here so they don't change
# with the screen layout.
assign_plots <- function(df) {
  df <- df |>
    mutate(
      name = vapply(`Software website`, derive_name, character(1), USE.NAMES = FALSE),
      block = coalesce(Organisation, "Unaffiliated")
    )

  block_order <- df |>
    count(block) |>
    arrange(desc(n), block) |>
    pull(block)

  df |>
    mutate(block = factor(block, levels = block_order)) |>
    arrange(block, desc(coalesce(last_commit_date, as.Date("1900-01-01")))) |>
    group_by(block) |>
    mutate(
      letter = LETTERS[as.integer(block)],
      idx = row_number(),
      id = sprintf("%s%02d", letter, idx)
    ) |>
    ungroup() |>
    mutate(block = as.character(block))
}


# Activity ------------------------------------------------------------------

time_ago <- function(days) {
  if (days < 1) return("today")
  if (days < 2) return("yesterday")
  if (days < 60) return(sprintf("%d days ago", days))
  if (days < 700) return(sprintf("%d months ago", round(days / 30.4)))
  sprintf("%d years ago", round(days / 365))
}

# Level 3 (active) to 0 (dormant), from the date of the last commit.
activity <- function(last_commit, today = Sys.Date()) {
  if (is.na(last_commit)) {
    return(list(level = 0, label = "Unscored", ago = ""))
  }
  days <- as.numeric(today - as.Date(last_commit))
  level <- if (days <= 90) 3 else if (days <= 365) 2 else if (days <= 730) 1 else 0
  list(
    level = level,
    label = c("Dormant", "Quiet", "Maintained", "Active")[level + 1],
    ago = time_ago(days)
  )
}

ACTIVITY_LEVELS <- list(
  list(level = 3, label = "Active", hint = "<3 mo"),
  list(level = 2, label = "Maintained", hint = "<1 yr"),
  list(level = 1, label = "Quiet", hint = "<2 yr"),
  list(level = 0, label = "Dormant", hint = "")
)


# Crop drawings -------------------------------------------------------------
# One glyph per growth stage, drawn growing up from (0, 0).

ear_grains <- local({
  j <- 0:4
  side <- ifelse(j %% 2 == 1, 0.11, -0.11)
  y <- -2.25 - j * 0.17
  angle <- ifelse(j %% 2 == 1, 22, -22)
  paste(sprintf(
    '<ellipse class="grain" cx="%.2f" cy="%.2f" rx="0.11" ry="0.19" transform="rotate(%d %.2f %.2f)"/>',
    side, y, angle, side, y
  ), collapse = "")
})

GLYPHS <- list(
  # in ear
  "3" = paste0(
    '<path class="stem" d="M0 0 Q0.06 -1.2 0 -2.1"/>',
    '<path class="leaf" d="M0 -0.7 Q-0.55 -1 -0.9 -0.75"/>',
    '<path class="leaf" d="M0 -1.3 Q0.55 -1.6 0.85 -1.35"/>',
    ear_grains,
    '<path class="awn" d="M0 -2.95 L-0.08 -3.4 M0.11 -2.75 L0.36 -3.2 M-0.11 -2.6 L-0.38 -3.05"/>'
  ),
  # leafy
  "2" = paste0(
    '<path class="stem" d="M0 0 Q0.05 -1 0 -2"/>',
    '<path class="leaf" d="M0 -0.55 Q-0.5 -0.85 -0.85 -0.65"/>',
    '<path class="leaf" d="M0 -1.05 Q0.5 -1.35 0.8 -1.15"/>',
    '<path class="leaf" d="M0 -1.55 Q-0.3 -1.95 -0.55 -2.1"/>'
  ),
  # seedling
  "1" = paste0(
    '<path class="stem" d="M0 0 L0 -0.55"/>',
    '<path class="leaf" d="M0 -0.45 Q-0.15 -0.85 -0.5 -0.95"/>',
    '<path class="leaf" d="M0 -0.5 Q0.2 -0.95 0.45 -1.05"/>'
  ),
  # stubble
  "0" = '<path class="stubble" d="M-0.12 0 L-0.18 -0.45 M0.12 0 L0.16 -0.4"/>'
)

glyph <- function(level) HTML(GLYPHS[[as.character(level)]])

# Small icon of one plant, used in the key and the status filter chips.
crop_icon <- function(level, class, soil = FALSE) {
  svg_node("svg",
    class = class, viewBox = "-1.1 -3.5 2.2 3.6", `aria-hidden` = "true",
    svg_node("g", class = paste0("g-v", level), glyph(level)),
    if (soil) svg_node("line", class = "soil", x1 = -1, x2 = 1, y1 = 0, y2 = 0)
  )
}

# Pseudo-random but repeatable, so the crop looks hand-sown without
# changing between renders.
wobble <- function(i, seed) sin(i * 12.9898 + seed * 78.233)

# A row of n plants spread across x0..x1, standing on y. `delay` staggers
# the grow-in animation across the field.
crop_row <- function(level, n, x0, x1, y, seed, scale = 1, delay = 0) {
  n <- max(1, min(n, 14))
  xs <- x0 + (x1 - x0) * (seq_len(n) - 0.5) / n

  lapply(seq_len(n), function(i) {
    height <- scale * (1 + 0.08 * wobble(i, seed))
    lean <- 4 * wobble(i + 7, seed)

    svg_node("g",
      transform = sprintf("translate(%.2f %.2f)", xs[i], y),
      svg_node("g",
        class = "plant", style = sprintf("--d:%.2fs", delay + i * 0.03),
        svg_node("g",
          class = "sway", style = sprintf("--s:%.2fs", 0.15 * abs(wobble(i, seed + 3))),
          svg_node("g",
            class = paste0("g-v", level),
            transform = sprintf("rotate(%.1f) scale(%.2f)", lean, height),
            glyph(level)
          )
        )
      )
    )
  })
}

plant_count <- function(contributors) {
  if (is.na(contributors)) 3 else contributors
}

plot_seed <- function(id) sum(utf8ToInt(id))


# A plot --------------------------------------------------------------------

plot_tag <- function(pkg) {
  status <- activity(pkg$last_commit_date)
  code_kw <- split_tags(pkg$`Code keywords`)
  topic_kw <- split_tags(pkg$`Topic keywords`)
  on_cran <- !is.na(pkg$cran_package)
  name <- coalesce(pkg$name, "")
  maintainer <- coalesce(pkg$`Code maintainer`, "")

  search_text <- tolower(paste(
    pkg$name, pkg$`Code maintainer`, pkg$Organisation, pkg$Description,
    paste(code_kw, collapse = " "), paste(topic_kw, collapse = " "),
    pkg$Language, pkg$License, pkg$`Code type`
  ))

  # Shrink long names so they always fit across the plot
  name_size <- min(1.6, (PLOT_W - 1.2) / (0.62 * max(1, nchar(name))))
  by_size <- min(0.9, (PLOT_W - 1.2) / (0.5 * max(1, nchar(maintainer))))
  soil_y <- PLOT_H - 0.55

  svg_node("g",
    class = paste0("plot is-v", status$level),
    tabindex = "0",
    role = "button",
    `data-id` = pkg$id,
    `data-search` = search_text,
    `data-keywords` = tolower(paste(code_kw, collapse = "|")),
    `data-status` = tolower(status$label),
    `data-cran` = as.integer(on_cran),

    svg_node("title", paste(name, "—", maintainer)),
    svg_node("rect", class = "plot-fill", width = PLOT_W, height = PLOT_H),
    svg_node("line", class = "soil", x1 = 0.5, x2 = PLOT_W - 0.5, y1 = soil_y, y2 = soil_y),
    svg_node("g",
      class = "crop",
      crop_row(
        status$level, plant_count(pkg$contributors), 0.6, PLOT_W - 0.6, soil_y,
        seed = plot_seed(pkg$id), scale = 0.9,
        delay = 0.05 * (match(pkg$letter, LETTERS) - 1) + 0.02 * pkg$idx
      )
    ),
    svg_node("text", class = "plot-id", x = 0.55, y = 1.35, pkg$id),
    if (on_cran) {
      svg_node("circle",
        class = "plot-cran", cx = PLOT_W - 0.95, cy = 1, r = 0.38,
        svg_node("title", "On CRAN")
      )
    },
    svg_node("text", class = "plot-name", x = 0.6, y = 3, `font-size` = round(name_size, 2), name),
    svg_node("text", class = "plot-by", x = 0.6, y = 4.15, `font-size` = round(by_size, 2), maintainer)
  )
}

block_tag <- function(block_plots) {
  letter <- block_plots$letter[1]

  svg_node("g",
    class = "block", `data-letter` = letter,

    svg_node("text",
      class = "block-cap", `data-name` = block_plots$block[1],
      svg_node("tspan", class = "block-letter", paste("Block", letter)),
      .noWS = c("after-begin", "before-end")
    ),
    svg_node("g", class = "block-extras"),
    lapply(seq_len(nrow(block_plots)), function(i) plot_tag(block_plots[i, ]))
  )
}


# Plot entry ----------------------------------------------------------------
# One <template> per plot; field.js copies it into the card that
# unfolds from the plot.

entry_row <- function(label, value, hint = NULL) {
  div(class = "e-row", title = hint, tags$dt(label), tags$dd(value))
}

keyword_line <- function(words) {
  if (length(words) == 0) return(p(class = "e-tags", "—"))
  dot <- span(class = "dot", "·", .noWS = "outside")
  items <- unlist(lapply(words, function(w) list(w, dot)), recursive = FALSE)
  p(class = "e-tags", head(items, -1))
}

entry_template <- function(pkg) {
  status <- activity(pkg$last_commit_date)
  name <- coalesce(pkg$name, pkg$`Code type`)
  repo <- pkg$`Software website`

  tag("template", list(
    `data-for` = pkg$id,
    svg_node("svg",
      class = "e-crop", viewBox = "0 0 26 4.2", preserveAspectRatio = "xMinYMax meet",
      `aria-hidden` = "true",
      svg_node("line", class = "soil", x1 = 0, x2 = 26, y1 = 3.9, y2 = 3.9),
      crop_row(status$level, plant_count(pkg$contributors), 1.2, 24.8, 3.9, seed = plot_seed(pkg$id))
    ),
    p(class = "e-kicker", sprintf("Plot %s · Block %s · ", pkg$id, pkg$letter), span(class = "e-pos")),
    h2(
      class = "e-name",
      if (!is.na(repo) && nzchar(repo)) a(href = repo, target = "_blank", rel = "noopener", name) else name
    ),
    p(class = "e-by", paste(coalesce(pkg$`Code maintainer`, ""), "·", coalesce(pkg$Organisation, ""))),
    p(
      class = "e-status", status$label,
      if (nzchar(status$ago)) span(paste(" · last commit", status$ago))
    ),
    p(class = "e-desc", coalesce(pkg$Description, "")),
    tags$dl(
      class = "e-list",
      entry_row("Language", coalesce(pkg$Language, "")),
      entry_row("Licence", coalesce(pkg$License, "")),
      entry_row("Type", coalesce(pkg$`Code type`, "")),
      entry_row("CRAN", coalesce(pkg$cran_package, "—")),
      entry_row("First commit", format_date(pkg$first_commit_date, "%b %Y")),
      entry_row("Last commit", format_date(pkg$last_commit_date, "%d %b %Y")),
      entry_row("GitHub stars", format_count(pkg$github_stars)),
      entry_row("Contributors", format_count(pkg$contributors)),
      entry_row("CRAN downloads, last month", format_count(pkg$cran_downloads_last_month), "cranlogs, last 30 days"),
      entry_row("CRAN downloads, all time", format_count(pkg$cran_downloads_total), "cranlogs, since Oct 2012")
    ),
    h3(class = "e-h", "Code keywords"),
    keyword_line(split_tags(pkg$`Code keywords`)),
    h3(class = "e-h", "Topics"),
    keyword_line(split_tags(pkg$`Topic keywords`)),
    p(
      class = "e-links",
      a(href = pkg$`Software website`, target = "_blank", rel = "noopener", "Repository ↗"),
      a(href = paste0("mailto:", pkg$Email), "Email maintainer")
    )
  ))
}


# Filters and key -----------------------------------------------------------

# Filter chips use the site's .chip style (assets/aagi.scss) so they match
# the database page. field.js fills in the counts.
filter_chip <- function(group, key, label, icon = NULL) {
  tags$button(
    type = "button",
    class = "chip facet-chip",
    `data-group` = group,
    `data-key` = key,
    `aria-pressed` = "false",
    icon,
    span(class = "chip-label", label),
    span(class = "chip-n", `aria-hidden` = "true")
  )
}

keyword_chips <- function(keywords) {
  lapply(keywords, function(k) filter_chip("kw", tolower(k), k))
}

status_chips <- function() {
  lapply(ACTIVITY_LEVELS, function(s) {
    filter_chip("status", tolower(s$label), s$label, crop_icon(s$level, "chip-crop"))
  })
}

cran_chip <- function() {
  filter_chip("cran", "1", "On CRAN", tags$i(class = "chip-dot", `aria-hidden` = "true"))
}

facet <- function(id, label, chips) {
  tags$section(
    class = "facet", role = "group", `aria-labelledby` = id,
    h3(class = "facet-label", id = id, label),
    div(class = "facet-chips", chips)
  )
}

key_items <- function() {
  stages <- lapply(ACTIVITY_LEVELS, function(s) {
    span(
      crop_icon(s$level, "lg-crop", soil = TRUE),
      s$label,
      if (nzchar(s$hint)) tags$small(s$hint)
    )
  })
  tagList(
    stages,
    span(tags$i(class = "sw is-buffer"), "Buffer"),
    span(tags$i(class = "sw is-cran"), "On CRAN"),
    span(class = "legend-note", "One plant per contributor")
  )
}
