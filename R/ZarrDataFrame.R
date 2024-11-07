#' Zarr-backed DataFrame
#'
#' Create a Zarr-backed \linkS4class{DataFrame}, where the data are kept on disk until requested.
#' 
#' @param tab A set of ZarrArrays that are the columns of a data frame.
#' @param name String containing the Zarr group of the Zarr file.
#' @param columns Character vector containing the names of columns in a  Zarr-based data frame.
#' If \code{NULL}, this is determined from \code{path}.
#' @param nrows Integer scalar specifying the number of rows in a  Zarr-based data frame.
#' If \code{NULL}, this is determined from \code{path}.
#'
#' @return A ZarrDataFrame where each column is a \linkS4class{ZarrColumnVector}.
#'
#' @author Artür Manukyan
#'
#' @aliases
#' ZarrDataFrame-class
#'
#' nrow,ZarrDataFrame-method
#' ncol,ZarrDataFrame-method
#' length,ZarrDataFrame-method
#' path,ZarrDataFrame-method
#'
#' rownames,ZarrDataFrame-method
#' names,ZarrDataFrame-method
#' rownames<-,ZarrDataFrame-method
#' names<-,ZarrDataFrame-method
#'
#' extractROWS,ZarrDataFrame,ANY-method
#' extractCOLS,ZarrDataFrame-method
#' [[,ZarrDataFrame-method
#'
#' replaceROWS,ZarrDataFrame-method
#' replaceCOLS,ZarrDataFrame-method
#' normalizeSingleBracketReplacementValue,ZarrDataFrame-method
#' [[<-,ZarrDataFrame-method
#'
#' cbind,ZarrDataFrame-method
#' cbind.ZarrDataFrame
#'
#' as.data.frame,ZarrDataFrame-method
#' coerce,ZarrDataFrame,DFrame-method
#'
#' @export
ZarrDataFrame <- function(tab, name, columns=NULL, nrows=NULL) {
    if (is.null(columns) || is.null(nrows)) {
        if (is.null(columns)) {
            columns <- names(tab)
        }
        if (is.null(nrows)) {
            nrows <- length(tab[[1]])
        }
    } 
    path <- DelayedArray::path(tab[[1]])
    name <- dirname(tab[[1]]@seed@name)
    new("ZarrDataFrame", path=path, name = name, columns=columns, nrows=nrows)
}

.DollarNames.ZarrDataFrame <- function(x, pattern = "")
  grep(pattern, x@columns, value=TRUE)

#' @export
setClass("ZarrDataFrame", contains="DataFrame", slots=c(path="character", name = "character", columns="character", nrows="integer"))

#' @export
setMethod("nrow", "ZarrDataFrame", function(x) x@nrows)

#' @export
setMethod("length", "ZarrDataFrame", function(x) length(x@columns))

#' @export
setMethod("path", "ZarrDataFrame", function(object) object@path)

#' @export
setMethod("rownames", "ZarrDataFrame", function(x) NULL)

#' @export
setMethod("names", "ZarrDataFrame", function(x) x@columns)

#' @export
setReplaceMethod("rownames", "ZarrDataFrame", function(x, value) {
    if (!is.null(value)) {
        x <- .collapse_to_df(x)
        rownames(x) <- value
    }
    x
})

#' @export
setReplaceMethod("names", "ZarrDataFrame", function(x, value) {
    if (!identical(value, names(x))) {
        x <- .collapse_to_df(x)
        names(x) <- value
    }
    x
})

#' @export
#' @importFrom S4Vectors extractROWS
setMethod("extractROWS", "ZarrDataFrame", function(x, i) {
    if (!missing(i)) {
        collapsed <- .collapse_to_df(x)
        extractROWS(collapsed, i)
    } else {
        x
    }
})

#' @export
#' @importFrom stats setNames
#' @importFrom S4Vectors extractCOLS normalizeSingleBracketSubscript
setMethod("extractCOLS", "ZarrDataFrame", function(x, i) {
    if (!missing(i)) {
        xstub <- setNames(seq_along(x), names(x))
        i <- normalizeSingleBracketSubscript(i, xstub)
        x@columns <- x@columns[i]
        x@elementMetadata <- extractROWS(x@elementMetadata, i)
    }
    x
})

#' @export
#' @importFrom S4Vectors normalizeDoubleBracketSubscript
setMethod("[[", "ZarrDataFrame", function(x, i, j, ...) {
    if (!missing(j)) {
        stop("list-style indexing of a ZarrDataFrame with non-missing 'j' is not supported")
    }

    if (missing(i) || length(i) != 1L) {
        stop("expected a length-1 'i' for list-style indexing of a ZarrDataFrame")
    }

    i <- normalizeDoubleBracketSubscript(i, x)
    ZarrColumnVector(x@path, column=x@columns[i], name = x@name)
})

#' @export
#' @importFrom S4Vectors replaceROWS
setMethod("replaceROWS", "ZarrDataFrame", function(x, i, value) {
    x <- .collapse_to_df(x)
    replaceROWS(x, i, value)
})

#' @export
#' @importFrom S4Vectors normalizeSingleBracketReplacementValue
setMethod("normalizeSingleBracketReplacementValue", "ZarrDataFrame", function(value, x) {
    if (is(value, "ZarrColumnVector")) {
        return(new("ZarrDataFrame", path=value@seed@path, columns=value@seed@column, nrows=length(value)))
    }
    callNextMethod()
})

#' @export
#' @importFrom stats setNames
#' @importFrom S4Vectors replaceCOLS normalizeSingleBracketSubscript
setMethod("replaceCOLS", "ZarrDataFrame", function(x, i, value) {
    xstub <- setNames(seq_along(x), names(x))
    i2 <- normalizeSingleBracketSubscript(i, xstub, allow.NAs=TRUE)
    if (length(i2) == 1L && !is.na(i2)) {
        if (is(value, "ZarrDataFrame")) {
            if (x@path == value@path && identical(x@columns[i2], value@columns)) {
                return(x)
            }
        }
    }

    # In theory, it is tempting to return a ZarrDataFrame; the problem is
    # that assignment will change the mapping of column names to their
    # contents, so it is no longer a pure representation of a ZarrDataFrame.
    x <- .collapse_to_df(x)
    replaceCOLS(x, i, value)
})

#' @export
#' @importFrom S4Vectors normalizeDoubleBracketSubscript
setMethod("[[<-", "ZarrDataFrame", function(x, i, j, ..., value) {
    i2 <- normalizeDoubleBracketSubscript(i, x, allow.nomatch=TRUE)
    if (length(i2) == 1L && !is.na(i2)) {
        if (is(value, "ZarrColumnVector")) {
            if (x@path == value@seed@path && x@columns[i2] == value@seed@column) {
                return(x)
            }
        }
    }

    x <- .collapse_to_df(x)
    x[[i]] <- value
    x
})

#' @export
#' @importFrom S4Vectors mcols make_zero_col_DFrame combineRows
cbind.ZarrDataFrame <- function(..., deparse.level=1) {
    preserved <- TRUE
    all_columns <- character(0)
    objects <- list(...)
    xpath <- NULL

    for (i in seq_along(objects)) {
        obj <- objects[[i]]
        if (is(obj, "ZarrDataFrame")) {
            if (is.null(xpath)) {
                xpath <- obj@path
            } else if (obj@path != xpath) {
                preserved <- FALSE
                break
            } 
            all_columns <- c(all_columns, obj@columns)

        } else if (is(obj, "ZarrColumnVector")) {
            if (is.null(xpath)) {
                xpath <- obj@seed@path
            } else if (obj@seed@path != xpath || !identical(names(objects)[i], obj@seed@column)) {
                preserved <- FALSE
                break
            } 
            all_columns <- c(all_columns, obj@seed@column)

        } else {
            preserved <- FALSE
            break
        }
    }

    if (!preserved) {
        for (i in seq_along(objects)) {
            obj <- objects[[i]]
            if (is(obj, "ZarrDataFrame")) {
                objects[[i]] <- .collapse_to_df(obj)
            }
        }
        do.call(cbind, objects)

    } else {
        all_mcols <- list()
        has_mcols <- FALSE
        all_metadata <- list()

        for (i in seq_along(objects)) {
            obj <- objects[[i]]

            mc <- NULL
            md <- list()
            if (is(obj, "DataFrame")) {
                mc <- mcols(obj, use.names=FALSE)
                md <- metadata(obj)
                if (is.null(mc)) {
                    mc <- make_zero_col_DFrame(length(obj))
                } else {
                    has_mcols <- TRUE
                }
            } else {
                mc <- make_zero_col_DFrame(1)
            }

            all_mcols[[i]] <- mc
            all_metadata[[i]] <- md
        }

        if (has_mcols) {
            all_mcols <- do.call(combineRows, all_mcols)
        } else {
            all_mcols <- NULL
        }

        new("ZarrDataFrame", 
            path=xpath,
            columns=all_columns,
            nrows=NROW(objects[[1]]),
            elementMetadata=all_mcols,
            metadata=do.call(c, all_metadata)
        )
    }
}

#' @export
#' @importFrom S4Vectors bindCOLS
setMethod("cbind", "ZarrDataFrame", cbind.ZarrDataFrame)

#' @importFrom S4Vectors make_zero_col_DFrame mcols mcols<- metadata metadata<-
.collapse_to_df <- function(x) {
    df <- make_zero_col_DFrame(x@nrows)
    for (i in seq_along(x@columns)) {
        df[[as.character(i)]] <- ZarrColumnVector(x@path, x@name, column=x@columns[i], length = x@nrows)
    }
    colnames(df) <- x@columns
    mcols(df) <- mcols(x, use.names=FALSE)
    metadata(df) <- metadata(x)
    df
}

#' @export
setMethod("as.data.frame", "ZarrDataFrame", function(x, row.names = NULL, optional = FALSE, ...) {
  df <- make_zero_col_DFrame(x@nrows)
  for (i in seq_along(x@columns)) {
    print(paste0(x@name, "/", x@columns[i]))
    zarrarray <- pizzarr::zarr_open_array(store = x@path, path = paste0(x@name, "/", x@columns[i]), mode = "r")
    df[[as.character(i)]] <- zarrarray$get_item("...")$data
  }
  colnames(df) <- x@columns
  mcols(df) <- mcols(x, use.names=FALSE)
  metadata(df) <- metadata(x)
  as.data.frame(df)
})

#' @export
setAs("ZarrDataFrame", "DFrame", function(from) .collapse_to_df(from))

