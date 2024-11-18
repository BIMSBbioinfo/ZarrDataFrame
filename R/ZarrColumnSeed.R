#' Column of an Zarr-based data frame
#'
#' Represent a column of a Zarr-based data frame as a 1-dimensional \linkS4class{DelayedArray}.
#' This allows us to use Zarr-backed data inside \linkS4class{DataFrame}s without loading them into memory.
#'
#' @param path String containing a path to a Zarr-based data frame.
#' @param name String containing the Zarr group of the Zarr file.
#' @param column String containing the name of the column inside the Zarr file.
#' @param length Integer containing the number of rows.
#' If \code{NULL}, this is determined by inspecting the file.
#' This should only be supplied for efficiency purposes, to avoid a file look-up on construction.
#' @param type String specifying the type of the data.
#' If \code{NULL}, this is determined by inspecting the file.
#' Users may specify this to avoid a look-up, or to coerce the output into a different type.
#' @param x Either a string containing the path to an Zarr-based data frame file (to be used as \code{path}),
#' or an existing ZarrColumnSeed object.
#' @param ... Further arguments to be passed to the \code{ZarrColumnSeed} constructor.
#'
#' @return For \code{ZarrColumnSeed}, a ZarrColumnSeed is returned, obviously.
#' 
#' For \code{ZarrColumnVector}, a ZarrColumnVector is returned.
#'
#' @author Artür Manukyan
#'
#' @aliases
#' ZarrColumnSeed-class
#' dim,ZarrColumnSeed-method
#' type,ZarrColumnSeed-method
#' path,ZarrColumnSeed-method
#' extract_array,ZarrColumnSeed-method
#' ZarrColumnVector-class
#' DelayedArray,ZarrColumnSeed-method
#'
#' @name ZarrColumnSeed
NULL

#' @export
#' @import methods
setClass("ZarrColumnSeed", slots=c(path="character", name = "character", column="character", length="integer", type="character"))

#' @export
setMethod("dim", "ZarrColumnSeed", function(x) x@length)

#' @export
#' @importFrom DelayedArray type
setMethod("type", "ZarrColumnSeed", function(x) x@type)

#' @export
#' @importFrom BiocGenerics path
setMethod("path", "ZarrColumnSeed", function(object) object@path)

#' @export
#' @importFrom DelayedArray extract_array
#' @importFrom pizzarr zarr_open_array
setMethod("extract_array", "ZarrColumnSeed", function(x, index) {
    slice <- index[[1]]
    
    if (is.null(slice)) {
      zarrarray <- pizzarr::zarr_open_array(store = x@path, path = paste0(x@name, "/", x@column), mode = "r")
      output <- zarrarray$get_item("...")$data
    } else if (length(slice) == 0) {
        output <- logical()
    } else {
        original <- slice
        modified <- FALSE

        if (anyDuplicated(slice)) {
            slice <- unique(slice)
            modified <- TRUE
        }

        if (is.unsorted(slice)) {
            slice <- sort(slice)
            modified <- TRUE
        }

        zarrarray <- pizzarr::zarr_open_array(store = x@path, path = paste0(x@name, "/", x@column), mode = "r")
        output <- zarrarray$get_orthogonal_selection(list(slice))$data
        if (modified) {
            m <- match(original, slice)
            output <- output[m]
        }
    }
    
    if (!is(output, x@type)) {
        output <- as(output, x@type)
    }

    array(output)
})

#' @export
#' @rdname ZarrColumnSeed
#' @importFrom DelayedArray type
#' @importFrom pizzarr zarr_open_array slice
ZarrColumnSeed <- function(path, name, column, type=NULL, length=NULL) {
    if (is.null(type) || is.null(length)) {
      zarrarray <- pizzarr::zarr_open_array(store = path, path = paste0(name, "/", column), mode = "r")
        if (is.null(type)){ 
          type <-  DelayedArray::type(zarrarray$get_item(list(pizzarr::slice(1,1)))$data)
        }
        if (is.null(length)) {
          length <- zarrarray$get_shape()
        }
    } 
    # print(type)
    new("ZarrColumnSeed", path=path, name=name, column=column, length=length, type=type)
}

#' @export
setClass("ZarrColumnVector", contains="DelayedArray", slots=c(seed="ZarrColumnSeed"))

#' @export
#' @importFrom DelayedArray DelayedArray
setMethod("DelayedArray", "ZarrColumnSeed", function(seed) ZarrColumnVector(seed))

#' @export
#' @rdname ZarrColumnSeed
ZarrColumnVector <- function(x, ...) {
    if (!is(x, "ZarrColumnSeed")) {
        x <- ZarrColumnSeed(x, ...)
    }
    new("ZarrColumnVector", seed=x)
}
