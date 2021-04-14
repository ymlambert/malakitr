#' Setup directory to retrieve and aggregate data of a form
#'
#' @param xFormId xForm ID of the form
#' @param token Identification token to connect to API
#' @param formDirectory Optional, path to directory where to save form information and data
#' @param baseUrl Optional, base url of API
#'
#' @return Form info object containing information on the form and its directory
#' @import jsonlite
#' @importFrom utils write.csv
#' @export
#'
#' @examples
setupFormDirectory <- function(xFormId, token, formDirectory = NULL, baseUrl = "https://api.ona.io/api/v1/"){
  # Get Form information
  response <- serverConnect(paste0("forms/",xFormId), token, baseUrl)
  response <- fromJSON(content(response,"text",encoding = "UTF-8"))

  # Create main directory with: 'xFormId__idString'
  # Check if exists
  if(is.null(formDirectory)){
    formDirPath <- file.path(getwd(), paste0(response$id_string, "__", response$formid))
  }
  else {
    formDirPath <- formDirectory
  }

  if(!dir.exists(formDirPath)) dir.create(formDirPath)

  # Setup subdirectories
  if(!dir.exists(file.path(formDirPath, "mold"))) dir.create(file.path(formDirPath, "mold"))
  if(!dir.exists(file.path(formDirPath, "instances"))) dir.create(file.path(formDirPath, "instances"))
  if(!dir.exists(file.path(formDirPath, "instances", "encrypted"))) dir.create(file.path(formDirPath, "instances", "encrypted"))
  if(!dir.exists(file.path(formDirPath, "instances", "decrypted"))) dir.create(file.path(formDirPath, "instances", "decrypted"))

  # Get XForm File
  xForm <- getXForm(xFormId, token, baseUrl)
  xFormPath <- file.path(formDirPath, "mold", "xForm.xml")
  saveXML(xForm, xFormPath, encoding = "UTF-8")
  if(file.exists(xFormPath)) message(paste0(xFormPath, " created successfully"))

  # XML mold
  xml <- getXmlMold(xFormPath, idString = response$id_string)
  xmlMoldFilePath <- file.path(formDirPath, "mold", "form_mold.xml")
  saveXML(xml$xmlMold, file = xmlMoldFilePath, encoding = "ASCII", prefix = "<?xml version='1.0'?>")
  if(file.exists(xmlMoldFilePath)) message(paste0(xmlMoldFilePath, " created successfully"))

  # CSV Mold
  repeatedNodeNames <- sub(x= xml$repeatedNodePaths, pattern="^.*\\.(.*)$", "\\1")
  csv <- getCsvMold(xml$xmlMold, repeatedNodeNames)
  csvMoldFilePath <- file.path(formDirPath, "mold", "form_mold.csv")
  write.csv(x = csv, file = csvMoldFilePath, row.names = F)
  if(file.exists(csvMoldFilePath)) message(paste0(csvMoldFilePath, " created successfully"))

  # Création du form info
  formInfoFilePath <- file.path(formDirPath, "form_info.json")

  formInfo <- list(
    xFormId = xFormId,
    idString = response$id_string,
    version = xml$formVersion,
    formDirPath = formDirPath,
    formInfoFilePath = formInfoFilePath,
    xFormPath = xFormPath,
    xmlMoldFilePath = xmlMoldFilePath,
    csvMoldFilePath = csvMoldFilePath,
    encryptedInstancePath = file.path(formDirPath, "instances", "encrypted"),
    decryptedInstancePath = file.path(formDirPath, "instances", "decrypted"),
    repeatedNodePaths = xml$repeatedNodePaths,
    repeatedNodeCounts = xml$repeatedNodeCounts,
    dateCreation = Sys.time()
  )

  write_json(toJSON(formInfo, auto_unbox = T, pretty = T), formInfoFilePath)
  if(file.exists(formInfoFilePath)) message(paste0(formInfoFilePath, " created successfully"))

  return(formInfo)
}

