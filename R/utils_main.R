#' @NoRd
serverConnect <- function(url, token, baseUrl){
  return(GET(paste0(baseUrl, url), add_headers(Authorization = paste0("Token ", token))))
}

#' @NoRd
makeDataFrame <- function(x){
  x <- unlist(x, recursive = T, use.names = T)
  x <- data.frame(t(x),stringsAsFactors = F)
  return(x)
}

#' @NoRd
decryptInstanceFile <-  function(instanceFilePath, instanceId, base64EncryptedSymmetricKey, base64RsaPrivateKey){
  z <- paste0("java -jar ", "inst/java/malakit_cipher.jar", " decipher",
              " \"", instanceFilePath, "\"",
              " \"", base64EncryptedSymmetricKey, "\"",
              " \"", instanceId,"\"",
              " \"", base64RsaPrivateKey, "\"")
  return(system(z, intern = T))
}

#' @NoRd
encryptInstanceFile <- function(instanceFilePath, instanceId, idString, formVersion, base64RsaPublicKey){
  z <- paste0("java -jar ", "inst/java/malakit_cipher.jar", " cipher",
              " \"", instanceFilePath, "\"",
              " \"", instanceId, "\"",
              " \"", idString, "\"",
              " \"", formVersion, "\"",
              " \"", base64RsaPublicKey, "\"")

  return(system(z, intern = T))
}

#' @NoRd
makeInstanceDataFrame <- function(instanceFilePath, repeatedNodePaths = ""){
  xml <- xmlTreeParse(instanceFilePath, useInternalNodes = T, encoding = "UTF-8")
  numberAllRepeatedNodes(xml, repeatedNodePaths)
  d <- makeDataFrame(xmlToList(xml))
  return(d)
}

#' @NoRd
mergeInstanceData <- function(instanceFilePaths, repeatedNodePaths = ""){
  dt <- lapply(instanceFilePaths, makeInstanceDataFrame, repeatedNodePaths)
  dt <- Reduce(bind_rows, dt)
  dt <- dt[,!grepl(x = names(dt), "^\\.attrs")]
  return(dt)
}


