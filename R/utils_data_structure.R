# Fonction récursive de remplacement des valeurs d'une liste
# Credit: https://stat.ethz.ch/pipermail/r-help/2010-April/237255.html
#' @NoRd
replaceInList <- function (x, FUN, ...) {
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[i] <- list(replaceInList(x[[i]], FUN, ...))
    }
    x
  }
  else FUN(x, ...)
}


# Téléchargement d'un fichier XForm depuis ONA.io
#' @NoRd
#' @import httr
#' @import XML
getXForm <- function(xFormId, token, baseUrl = "https://api.ona.io/api/v1/"){
  response <- serverConnect(paste0("forms/",xFormId,"/form.xml"), token, baseUrl)
  xml <- content(response, "text", encoding = "UTF-8")
  xml <- xmlTreeParse(xml, useInternalNodes = T, encoding = "UTF-8")
  return(xml)
}


# Création d'un fichier de schéma de données XML à partir d'un formulaire XForm
# ATTENTION en cas de répétitions multiples :
## les CHEMINS des noeuds de répétion et les nombres respectifs de répétitions
## doivent être indiqués par ordre hiérarchique décroissant (noeuds supérieurs en premier)
#' @NoRd
#' @import XML
getXmlMold <- function(xFormFilePath, idString, repeatedNodePaths = "", repeatedNodeCounts = "", maxRepeatIterations = 5){
  # Lecture du fichier XForm
  xml <- xmlTreeParse(xFormFilePath, useInternalNodes = T, encoding = "UTF-8")

  # Extraction et formatage de l'arborescence propre aux instances du formulaire
  root <- xmlClone(getNode(xml, "head.model.instance")[[1]])
  updateInstanceRoot(root, idString)

  removeXMLNamespaces(root, all=T)
  formVersion <- xmlAttrs(root)["version"]
  moldFilePath <- file.path(tempdir(), paste0(idString, formVersion, ".xml"))

  xmlNamespaces(root) <- xmlNamespaces(xml)
  saveXML(root,file = moldFilePath, encoding = "ASCII", prefix = "<?xml version='1.0'?>")
  root <- xmlTreeParse(moldFilePath, useInternalNodes = T, encoding = "UTF-8")

  # Détection des noeuds de répétition si argument non spécifié
  if(repeatedNodePaths == "" || is.na(repeatedNodePaths)){
    repeatedNodes <- xpathApply(root, "//*[@jr:template]")
    if(length(repeatedNodes) > 0){
      repeatedNodePaths <- sapply(repeatedNodes, getNodePath)
      repeatedNodeCounts <- rep(maxRepeatIterations, length(repeatedNodePaths))
    }
  }

  # Inversion de l'ordre des noeuds de répétition
  repeatedNodePaths <- rev(repeatedNodePaths)
  repeatedNodeCounts <- rev(repeatedNodeCounts)

  # Copie des noeuds de répétition
  pattern <- "^.*\\.(.*)$"

  if(repeatedNodePaths != "" && !is.na(repeatedNodePaths)){
    if(length(repeatedNodeCounts) == length(repeatedNodePaths)){
      for(i in seq_along(repeatedNodePaths)){
        # Extraction du noeud et de son parent
        repeatedNodePath <- repeatedNodePaths[i]
        repeatedNodeCount <- repeatedNodeCounts[i]
        repeatedNode <- getNode(root, repeatedNodePath)
        removeAttributes(repeatedNode)
        repeatedNodeName <- xmlName(repeatedNode)

        parentNode <- xmlParent(repeatedNode)

        # Détection de la position du noeud parmi les noeuds frères
        siblingNodeNames <- names(xmlChildren(parentNode))
        repeatedNodePosition <- which(siblingNodeNames == repeatedNodeName)

        # Création des copies numérotées du noeud
        newNodes <- lapply(1:repeatedNodeCount, function(j){
          n <- xmlClone(repeatedNode)
          xmlName(n) <- paste0(repeatedNodeName, "_", j)
          removeXMLNamespaces(n, all=T)
          return(n)
        })

        # Ajout des copies numérotées à l'arborescence et suppression du noeud original
        addChildren(parentNode, kids = newNodes, at = repeatedNodePosition[length(repeatedNodePosition)])
        removeChildren(parentNode, kids = parentNode[repeatedNodePosition])
      }
    }
    else{
      warning(paste0(xFormFilePath, " - length mismatch: repeated node paths and repeated node counts"))
    }
  }

  # Effacement des valeurs par défaut
  nodesWithValue <- names(unlist(xmlToList(root)))
  nodesWithValue <- nodesWithValue[!grepl(x = nodesWithValue, "^\\.attrs")]
  for(n in nodesWithValue) setNodeValue(root, n, "")


  response <- list(
    formVersion = formVersion,
    xmlMold = xmlRoot(root),
    repeatedNodePaths = rev(repeatedNodePaths),
    repeatedNodeCounts = rev(repeatedNodeCounts)
  )

  return(response)
}



# Création d'un fichier de schéma de données .csv à partir d'un schéma xml
# Colonnes de sortie :
## nom de variable (tient compte des répétitions éventuelles)
## numéro de question (si formatage par groupes <q**>)
## chemin xml de la variable
# ATTENTION en cas de répétitions multiples :
## les NOMS des noeuds de répétion doivent être indiqués par ordre hiérarchique décroissant (noeuds supérieurs en premier)
#' @NoRd
#' @import XML
getCsvMold <- function(xmlMold, repeatedNodeNames = ""){
  # Extraction des noeuds
  moldId <- xmlName(xmlMold)
  formVersion <- xmlAttrs(xmlMold)["version"]
  s <- xmlToList(xmlMold)

  # Remplacement des valeurs nulles dans la liste des noeuds
  s <- replaceInList(s, function(x)if(is.null(x)) NA else x)

  # Extraction des chemins
  s <- unlist(s)
  s <- s[!grepl(x = names(s), "^\\.attrs")]
  p <- names(s)

  # Extraction des noms de variables et prise en compte des répétitions
  suffix <- ""
  if(repeatedNodeNames != "" && !is.na(repeatedNodeNames)){
    for (r in repeatedNodeNames){
      pattern <- paste0(".*",r,"_([0-9]+).*")
      nb <- ifelse(grepl(x=p,pattern), sub(x = p, pattern = pattern, "\\.\\1"),"")
      suffix <- paste0(suffix,nb)
    }
  }
  n <- sub(x= p, pattern="^.*\\.(.*)$", "\\1")
  n <- paste0(n, suffix)

  # Création et écriture du schéma
  mold <- data.frame(name=n, path=p, stringsAsFactors = F)

  return(mold)
}


# Vérification de l'adéquation des données au schéma
#' @NoRd
checkDataStructure <- function(data, mold){
  match <- F
  diff <- setdiff(names(data),mold$path)
  if(length(diff) == 0){
    match <- T
  }
  else{
    warning(paste0("The following variable paths do not match the mold: ", paste0(diff, collapse = ", ")))
  }
  return(match)
}



