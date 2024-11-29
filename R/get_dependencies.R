#'Get information on dependencies used
#'
#'Returns information on dependencies used by R
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing bibtex-formatted citation information
#' @importFrom stringr str_match_all str_match
#' @keywords internal
#' @examples {
#' deps <- get_dependencies()
#' }
#'
get_dependencies <- function(packages_used_df){

  dep_out <- apply(packages_used_df,
        MARGIN = 1,
        FUN = function(x){

          if(is.na(x['package'])){
            return(data.frame(file = x[['file']],
                              package = x[['package']],
                              type = "dependency")
            )}else{
              return(data.frame(file = x[['file']],
                                package =   tools::package_dependencies(packages = x['package'],
                                                                        recursive = TRUE)|>
                                  unlist() |>
                                  as.vector()|>
                                  unname() |>
                                  unique()
                                ,
                                type = "dependency")
                                )

                   }#end else
        }#end fx
          )#end apply

  dep_out <- do.call("rbind", dep_out)
  rownames(libs) <- NULL

  return(dep_out)

}# end overall fx
