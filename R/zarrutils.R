#' zarrcreateGroup
#'
#' get information of an ImgArray object
#'
#' @param store the location of (zarr) store
#' @param name name of the group
#' @export
zarrcreateGroup <- function(store, name){
  split.name <- strsplit(name, split = "\\/")[[1]]
  if(length(split.name) > 1){
    split.name <- vapply(rev(seq_len(length(split.name)))[seq_len(2)], 
                         function(x) paste(split.name[seq_len(x)], collapse = "/"), 
                         FUN.VALUE = character(1)) 
    if(!dir.exists(file.path(store,split.name[2])))
      zarrcreateGroup(store = store, name = split.name[2])
  }
  dir.create(file.path(store, split.name[1]), showWarnings = FALSE)
  write("{\"zarr_format\":2}", file = file.path(store, split.name[1], ".zgroup"))
}

#' open_zarr
#'
#' open zarr store
#'
#' @param dir the location of zarr store
#' @param name name of the zarr store
#' @export
open_zarr <- function(dir, name){
  zarrcreateGroup(store = dir, name = name)
}