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




# Code hosts we fetch metrics from. `pages` is the host's static site domain,
# so owner.github.io/repo maps back to github.com/owner/repo, and the same for
# gitlab.io and codeberg.page.
FORGES <- list(
  github   = list(label = "GitHub",   host = "github.com",   pages = "github.io"),
  gitlab   = list(label = "GitLab",   host = "gitlab.com",   pages = "gitlab.io"),
  codeberg = list(label = "Codeberg", host = "codeberg.org", pages = "codeberg.page")
)

# Entries have no name field of their own, so the repository is derived from
# the software website. Returns list(forge, owner, repo, path), where path is
# "owner/repo", or NULL for a site on none of the FORGES. GitLab projects can
# sit in nested groups, so there owner is everything before the last segment.
parse_repo <- function(url) {
  if (is.na(url) || !nzchar(url)) return(NULL)
  u <- sub("/+$", "", sub("[?#].*$", "", sub("^https?://", "", url)))
  parts <- strsplit(u, "/")[[1]]
  host <- sub("^www\\.", "", tolower(parts[1]))
  path <- parts[-1]

  for (forge in names(FORGES)) {
    f <- FORGES[[forge]]
    pages <- paste0(".", f$pages)
    if (endsWith(host, pages) && length(path) >= 1) {
      owner <- substr(host, 1, nchar(host) - nchar(pages))
      repo <- path[1]
    } else if (host == f$host) {
      # Drop the page part of the URL: GitLab starts it with "/-/" and
      # GitHub/Codeberg repos are always owner/repo.
      if (forge == "gitlab") {
        cut <- match("-", path)
        if (!is.na(cut)) path <- head(path, cut - 1)
      } else {
        path <- head(path, 2)
      }
      if (length(path) < 2) return(NULL)
      owner <- paste(head(path, -1), collapse = "/")
      repo <- sub("\\.git$", "", path[length(path)])
    } else {
      next
    }
    return(list(forge = forge, owner = owner, repo = repo, path = paste(owner, repo, sep = "/")))
  }
  NULL
}

# Short display name for an entry, taken from its website:
# github.com/owner/repo -> repo, owner.gitlab.io/pkg -> pkg.
derive_name <- function(url) {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  repo <- parse_repo(url)
  if (!is.null(repo)) return(repo$repo)
  u <- sub("/+$", "", sub("^https?://", "", url))
  parts <- strsplit(u, "/")[[1]]
  if (length(parts) >= 2) return(parts[length(parts)])
  sub("\\..*$", "", parts[1])
}

# "a, b, c" -> c("a", "b", "c")
split_tags <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  out <- trimws(unlist(strsplit(x, ",")))
  out[nzchar(out)]
}
