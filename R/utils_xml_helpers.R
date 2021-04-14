#' @NoRd
updateInstanceRoot <- function(xmlTree, value){
  x <- xmlRoot(xmlTree)
  xmlName(x) <- value
  xmlAttrs(x)["id"] <- value
  return(value)
}

#' @NoRd
getNode <- function(xmlTree, path){
  node <- xmlRoot(xmlTree)
  path <- strsplit(path,"\\.")[[1]]
  for (n in path){
    node <- node[[n]]
  }
  return(node)
}

#' @NoRd
getNodePath <- function(node){
  path <- NULL
  while(!is.null(xmlParent(node))){
    path <- c(xmlName(node),path)
    node <- xmlParent(node)
  }
  path <- paste0(path, collapse=".")
  return(path)
}

#' @NoRd
setNodeValue <- function(xmlTree, path, value){
  node <- getNode(xmlTree, path)
  xmlValue(node) <- value
}

#' @NoRd
getNodeValue <- function(xmlTree, path){
  y <- NA
  if(!is.na(path)){
    y <- xmlValue(getNode(xmlTree, path))
    if(y=="" & !is.na(y)){
      y <- NA
    }
  }
  return(y)
}

#' @NoRd
getRepeatedNodes <- function(xmlTree, path){
  rootpath <- ""
  pattern <- "^(.*)\\..*$"
  if(grepl(x = path, pattern)){
    rootpath <- sub(x = path, pattern = pattern, "\\1")
    path <- sub(x = path, pattern = paste0(rootpath,"\\."), "")
  }
  node <- getNode(xmlTree, rootpath)
  return(node[path])
}

#' @NoRd
numberRepeatedNodes <- function(xmlTree, path){
  nodes <- getRepeatedNodes(xmlTree, path)
  if(length(nodes)>0){
    name <- sub(x=path, pattern="^.*\\.(.*)$", "\\1")
    for(i in 1:length(nodes)){
      xmlName(nodes[[i]]) <- paste0(name,"_",i)
    }
  }
  return(nodes)
}

#' @NoRd
numberAllRepeatedNodes <- function(xmlTree, paths){
  root <- xmlRoot(xmlTree)
  counts <- NULL
  i <- 1
  while (i <= length(paths)) {
    node <- paths[i]
    count <- length(numberRepeatedNodes(root, node))
    counts <- c(counts, count)
    if(i<length(paths)){
      done <- paths[1:i]
      rest <- paths[(i+1):length(paths)]
      newRest <- NULL
      if(count > 0){
        prefix <- paste0(node,"_",1:count)
        for(r in rest){
          if(grepl(x=r,node)){
            suffix <- sub(x = r, pattern = node, "")
            r <- paste0(prefix, suffix)
          }
          newRest <- c(newRest, r)
        }
      }
      else {
        newRest <- rest[!grepl(x=rest,node)]
      }
      paths <- c(done, newRest)
    }
    i <- i+1
  }
  names(counts) <- paths
  return(counts)
}
