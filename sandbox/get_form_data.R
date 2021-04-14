#' Retrieve, decrypt and aggregate data of a specified form
#'
#' @param xFormId
#' @param token
#' @param privateKey
#' @param formDirectory
#' @param olData
#' @param baseUrl
#'
#' @return
#' @export
#'
#' @examples
getFormData <- function(xFormId, token, privateKey, formDirectory = NULL, olData = NULL, baseUrl = "https://api.ona.io/api/v1/"){

  formInfo <- NULL

  if(is.null(formDirectory)){
    formInfo <- setupFormDirectory(xFormId, token, baseUrl)
  }
  else {
    if(dir.exists(formDirectory)){
      formInfo <- setupFormDirectory(xFormId, formDirectory, token, baseUrl)
    }
    else{
      formInfo <- setupFormDirectory(xFormId, token, baseUrl)
    }
  }

  instanceRegistry <- getInstanceRegistry(xFormId, token, baseUrl)
  instanceRegistry <- getEncryptedInstances(instanceRegistry = instanceRegistry, writeToPath = formInfo$encryptedInstancePath)
  instanceRegistry <- decryptInstances(instanceRegistry = instanceRegistry, privateKey, writeToPath = formInfo$decryptedInstancePath)

  dt <- aggregateInstances(instanceRegistry = instanceRegistry, formInfo = formInfo)

  return(dt)
}
