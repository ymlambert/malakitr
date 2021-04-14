#' Get information on all active forms from API
#'
#' @param token Identification token to connect to API
#' @param baseUrl Optional, base url of API
#'
#' @return Dataframe with updated information on active forms
#' @importFrom rlang .data
#' @import jsonlite
#' @import httr
#' @export
#'
#' @examples
getForms <- function(token, baseUrl = "https://api.ona.io/api/v1/"){
  forms <- serverConnect("forms", token, baseUrl)
  forms <- fromJSON(content(forms,"text",encoding = "UTF-8"))

  forms <- forms %>% select(xform_id = .data$formid, .data$uuid, .data$title, .data$id_string, .data$version,
                            .data$date_created, .data$date_modified, .data$last_submission_time, .data$num_of_submissions,
                            .data$encrypted, .data$downloadable)
  return(forms)
}
