
github_repos <- "https://github.com/Plant-Functional-Trait-Course/pftc7_south_africa"

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

  # write the citations

    stop("Write code")

  # delete the temporary folder

    unlink(dir_to_delete,recursive = TRUE,force = TRUE)

  # return the data.frame

    return(packages_used_citations)



}
