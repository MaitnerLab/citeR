
#'Get information on packaged used by code on Github
#'
#'Returns information needed to cite the GVS
#' @param github_repos character. A single repository or a vector of repositories.
#' @param verbose Logical. Default is TRUE. If TRUE displays messages on status.
#' @param bibtex_file. NULL or character. An optional file to which citations are written as a .bib file.
#' @return Dataframe containing information on which packages are used by which R files.
#' @importFrom stringr str_match_all str_match
#' @export
#' @examples {
#' packages_used <- get_github_citations(github_repos = "https://github.com/Plant-Functional-Trait-Course/PFTC6_Norway",
#'                                       verbose = TRUE,
#'                                       bibtex_file = NULL)
#' }
#'
get_github_citations <- function(github_repos,
                                 verbose = TRUE,
                                 bibtex_file = NULL){

  # make temp folder
  dir_to_delete <- tempdir()

  # pull the github repos

    for(i in 1:length(github_repos)){

      path_i <- paste(github_repos[i],"/archive/master.zip",collapse = "",sep = "")

      download.file(url = path_i,
                    destfile = file.path(dir_to_delete,paste(i,".zip",sep = "")))

      unzip(zipfile = file.path(dir_to_delete,paste(i,".zip",sep = "")),
            exdir = dir_to_delete)


    }

  # get the packages

    packages_used <- get_packages_used(dir_to_delete)

  # get the citations

    packages_used_citations <- suppressWarnings(get_package_citations(packages_used_dataframe = packages_used))

  # write the citations if a file was specified

    if(!is.null(bibtex_file )){

      packages_used_citations %>%
        select(citation) %>%
        unique()%>%
        na.omit()%>%
        pull(citation)->test

      class(test)


      writeLines(text = test,
                 con=bibtex_file)

    }

  # delete the temporary folder

    unlink(dir_to_delete,recursive = TRUE,force = TRUE)

  # return the data.frame

    return(packages_used_citations)



}
