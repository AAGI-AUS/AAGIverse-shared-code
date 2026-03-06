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


