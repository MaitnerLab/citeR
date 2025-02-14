#'Get citation information for packages used
#'
#'Returns information needed to cite R packages used in analyses
#' @param packages_used_dataframe Output returned by `get_packages_used`
#' @param dependencies Logical. If TRUE, will additionally return information on the dependencies used by each R file.
#' @return Dataframe containing information on which packages are used by which R files.
#' @importFrom stringr str_match_all str_match
#' @export
#' @examples {
#' packages_used <- get_packages_used()
#' }
#'
get_package_citations <- function(packages_used_dataframe){


  # get list of unique packages

    unique_packs <- data.frame(package =unique(packages_used_dataframe$package),
                            citation = NA)

    unique_packs <- unique_packs[which(!is.na(unique_packs$package)),]

    for(i in 1:nrow(unique_packs)){

      # First, try citation

        cite_i <- tryCatch(citation(unique_packs$package[i]),
                           error=function(e){e})

        if(!inherits(x = cite_i,what = "error")){
          unique_packs$citation[i] <- paste(unlist(toBibtex(cite_i)),collapse = "")
          next}

      # Next, try CRAN

        cite_i <- tryCatch(get_bibtex_from_cran(package = unique_packs$package[i]),
                           error=function(e){e})

        if(!inherits(x = cite_i,what = "error")){
          unique_packs$citation[i] <- cite_i
          next}

      # Failing that, try installing the package

        tryCatch(install.packages(unique_packs$package[i],
                                  quiet = TRUE,
                                  verbose = FALSE),
                 error=function(e){e})


        cite_i <- tryCatch(citation(unique_packs$package[i]),
                           error=function(e){e})

        if(!inherits(x = cite_i,what = "error")){
          unique_packs$citation[i] <-paste(unlist(toBibtex(cite_i)),collapse = "")

          next}

      # Try installing most recent version


        tryCatch(devtools::install_version(unique_packs$package[i]),
                 error=function(e){e})



        cite_i <- tryCatch(citation(unique_packs$package[i]),
                           error=function(e){e})

        if(!inherits(x = cite_i,what = "error")){

          unique_packs$citation[i] <-paste(unlist(toBibtex(cite_i)),collapse = "")

          next}

      # Give up

        message("Couldn't find package ",unique_packs$package[i])


    }



}
