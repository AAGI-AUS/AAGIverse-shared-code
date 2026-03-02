make_hyperlink <- function(x) {
  return(paste0("<a href='", x,"' target='_blank'>", x,"</a>"))
}

make_hyperlink_email <- function(x) {
  return(paste0("<a href='mailto:", x,"'>", x,"</a>"))
}
