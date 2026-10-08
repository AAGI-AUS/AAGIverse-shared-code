make_hyperlink <- function(x) {
  return(paste0("<a href='", x,"' target='_blank'>", x,"</a>"))
}

make_hyperlink_email <- function(x) {
  return(paste0("<a href='mailto:", x,"'>", x,"</a>"))
}



library(htmltools)

truncate_field <- function(x, nchar = 140){
  return(
    paste0(substr(x, 1, nchar), "…") |> 
      htmltools::htmlEscape()
         )
}

tooltip_span <- function(full, preview){
  # Escape for safe HTML attributes / display
  full_esc <- htmltools::htmlEscape(full)
  preview_esc <- htmltools::htmlEscape(preview) # e.g from truncate_field above
  
  # Tooltip span
  return(
    sprintf('<span title="%s">%s</span>', 
            full_esc, 
            preview_esc)
  )
}




# Entries have no name field of their own, so owner/repo are derived from the
# software website: github.com/owner/repo and owner.github.io/repo both map to
# owner/repo. Returns NULL for anything that isn't GitHub-hosted.
parse_github_repo <- function(url) {
  if (is.na(url) || !nzchar(url)) return(NULL)
  u <- sub("/+$", "", sub("^https?://", "", url))
  parts <- strsplit(u, "/")[[1]]
  host <- tolower(parts[1])
  if (grepl("\\.github\\.io$", host) && length(parts) >= 2) {
    return(list(owner = sub("\\.github\\.io$", "", host), repo = parts[2]))
  }
  if (host == "github.com" && length(parts) >= 3) {
    return(list(owner = parts[2], repo = sub("\\.git$", "", parts[3])))
  }
  NULL
}

# Short display name for an entry, taken from its website:
# github.com/owner/repo -> repo, owner.github.io/pkg -> pkg.
derive_name <- function(url) {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  u <- sub("/+$", "", sub("^https?://", "", url))
  parts <- strsplit(u, "/")[[1]]
  host <- parts[1]
  if (grepl("github\\.io$", host) && length(parts) >= 2) return(parts[2])
  if (identical(host, "github.com") && length(parts) >= 3) return(parts[3])
  if (length(parts) >= 2) return(parts[length(parts)])
  sub("\\..*$", "", host)
}

# "a, b, c" -> c("a", "b", "c")
split_tags <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  out <- trimws(unlist(strsplit(x, ",")))
  out[nzchar(out)]
}
