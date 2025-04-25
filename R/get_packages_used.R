#'Get information on packaged used
#'
#'Returns information needed for citation
#' @param ... R files and/or folders containing R files. If left blank, will default to the current working directory.
#' @param version_installed Logical. If TRUE, will return version information for installed packages.
#' @param dependencies Logical. If TRUE, will additionally return information on the dependencies used by each R file.
#' @return Dataframe containing information on which packages are used by which R files.
#' @importFrom stringr str_match_all str_match
#' @export
#' @examples {
#' packages_used <- get_packages_used()
#' }
#'
get_packages_used <- function(...,
                              version_installed = FALSE,
                              dependencies = FALSE) {

  #package up the input

    files <- c(...)

  # get files in directory if nothing was supplied

  if(length(files) < 1){

    files <- dir(pattern="\\.R", ignore.case = TRUE,recursive = TRUE,full.names = TRUE)

  }

  # pull out any R files supplied

    if(any(!file.exists(files))){
      warning("File ",files[!file.exists(files)]," not found. Ignoring.")

      files <- files[file.exists(files)]

    }


  #handle files that were specified directly vs those specified as directory contents

  dirs_to_check <- files[dir.exists(files)]

  files_to_check <- files[!dir.exists(files)]

  dir_files <- dir(path = dirs_to_check,pattern="\\.R", ignore.case = TRUE,recursive = TRUE,full.names = TRUE)

  all_files_to_check <- c(dir_files,files_to_check) |> unique()


  # parse
  libs <- sapply(all_files_to_check, FUN = function(x) {

    conn <- file(x)
    text <- readLines(conn, warn = FALSE)

    # Extract package names with quoted or unquoted format
    #text <- str_match_all(text, "\\b(library|require)\\(['\"]?([a-zA-Z0-9.]+)['\"]?\\)")
    pkgs_called <- str_match_all(text, "\\b(library|require)\\(['\"]?([a-zA-Z0-9.]+)['\"]?[,\\)]")

    # Flatten the list and capture only package names (3rd element in each match)
    pkgs_called <- unlist(lapply(pkgs_called, function(x) x[, 3]))

    # Remove NA values
    pkgs_called <- na.omit(pkgs_called)

    # Extract packages from imp and impFrom directories
    import_matches <- str_match(text, "@import\\s+([a-zA-Z0-9.]+)")
    import_from_matches <- str_match(text, "@importFrom\\s+([a-zA-Z0-9.]+)")

    # Combine and filter the results
    all_imports <- c(import_matches[, 2], import_from_matches[, 2])
    all_imports <- na.omit(unique(all_imports))

    # Combine imports and library/require

    all_packages <- c(pkgs_called, all_imports)

    # Display the package names
    close(conn)

    packages_x <- unique(all_packages)
#
    if(length(packages_x)==0){packages_x <- NA}

    out <- data.frame(file = x,
               package=packages_x)


    return(out)
          },
              simplify = FALSE,
              USE.NAMES = TRUE)

  libs <- do.call("rbind", libs)
  rownames(libs) <- NULL

  # Optionally get dependencies

    if(dependencies == TRUE){

      libs$type <- "loaded"

      deps <- get_dependencies(packages_used_df = libs)

      libs <- rbind(libs,deps)

    }


  # Optionally get version numbers
  if(version_installed){

  libs$version <-
    sapply(X = libs$package,
           FUN = function(x){

             if(is.na(x)){NA}else{
               packageVersion(x)|> as.character()}

           }) |>
    unlist() |>
    unname()

  }

  return(libs)
}

