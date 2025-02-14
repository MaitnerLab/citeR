get_bibtex_from_cran <- function(package) {

  # Construct URL for the CITATION file
  citation_url <- sprintf("https://cran.r-project.org/web/packages/%s/citation.html", package)

  # Read the webpage content
  suppressWarnings(

    citation_page <- tryCatch(
      readLines(citation_url,
                warn = FALSE),
      error = function(e){e}  )


  )

  if (inherits(x = citation_page,what = "error")) {
    #message("Citation not found or package does not exist on CRAN.")
    return(citation_page)
  }


  # Extract BibTeX block (usually inside <pre> tags)
  bibtex <- sub(".*(<pre>.*?</pre>).*", "\\1", paste(citation_page, collapse = "\n"))
  bibtex <- gsub("<pre>|</pre>", "", bibtex)  # Remove HTML tags

  return(bibtex)
}
