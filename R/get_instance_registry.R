#' Get a list of all instances files submitted to a form
#'
#' @param xFormId xForm ID of the form
#' @param token Identification token to connect to API
#' @param baseUrl Optional, base url of API
#'
#' @return Instance registry dataframe with all instances submitted to the form
#' @export
#'
#' @examples
getInstanceRegistry <- function(xFormId, token, baseUrl = "https://api.ona.io/api/v1/"){
  response <- serverConnect(paste0("data/", xFormId), token, baseUrl)
  registry <- content(response, "parsed",  encoding = "UTF-8")
  registry <- lapply(registry, makeDataFrame)
  for(i in seq_along(registry)){
    names(registry[[i]]) <- str_replace(names(registry[[i]]),"X_","")
    names(registry[[i]]) <- str_replace(names(registry[[i]]),"attachments.","attachments_")
    names(registry[[i]]) <- str_replace(names(registry[[i]]), "^.*\\.(.*)$","\\1")
  }
  registry <- Reduce(bind_rows, registry)
  return(registry)
}

