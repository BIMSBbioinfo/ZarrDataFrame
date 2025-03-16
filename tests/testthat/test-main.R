library(Rarr)

# zarr
dir.create(td <- tempfile())
zarr_name <- "test.zarr"
output_zarr <- file.path(td, zarr_name)

# data
data("chickwts")
metadata <- chickwts
metadata2 <- chickwts
names(metadata2) <- paste0("new", names(metadata2))

test_that("create metadata", {
  
  # open zarr
  open_zarr(dir = td, name = zarr_name)
  expect_true(dir.exists(output_zarr))
  expect_true(file.exists(file.path(output_zarr, ".zgroup")))
  
  # set metadata
  meta.data_list <- list()
  zarrcreateGroup(store = output_zarr, name = "assay")
  for(i in 1:ncol(metadata)){
    cur_column <- as.vector(subset(metadata, select = colnames(metadata)[i]))[[1]]
    nchar <- NULL
    if(is.character(cur_column) || is.factor(cur_column)){
      cur_column <- as.character(cur_column)
      nchar <- max(vapply(cur_column, function(x) nchar(x), numeric(1)))
    }
    cur_column <- as.array(cur_column)
    meta.data_list[[colnames(metadata)[i]]] <- 
      Rarr::writeZarrArray(cur_column, 
                           zarr_array_path = file.path(output_zarr, paste0("assay", "/", colnames(metadata)[i])), 
                           chunk_dim = min(length(cur_column), 2000), nchar = nchar)
  }
  metadata_large <- ZarrDataFrame::ZarrDataFrame(meta.data_list, name = "assay", columns = names(meta.data_list))
  
  # check functions
  expect_equal(dim(metadata_large), dim(metadata))
  expect_equal(ncol(metadata_large), ncol(metadata))
  expect_equal(nrow(metadata_large), nrow(metadata))
  expect_equal(names(metadata_large), names(metadata))
  
  # random access/subset
  metadata_subset <- metadata[c(1,4,8),]
  metadata_large_subset <- metadata_large[c(1,4,8),]
  expect_equal(dim(metadata_large_subset), dim(metadata_subset))
  expect_equal(ncol(metadata_large_subset), ncol(metadata_subset))
  expect_equal(nrow(metadata_large_subset), nrow(metadata_subset))
  expect_equal(names(metadata_large_subset), names(metadata_subset))

  # conversion
  metadata_large_local <- as.data.frame(metadata_large)
  expect_equal(dim(metadata_large_local), dim(metadata))
  expect_equal(ncol(metadata_large_local), ncol(metadata))
  expect_equal(nrow(metadata_large_local), nrow(metadata))
  expect_equal(names(metadata_large_local), names(metadata))
  
  # merge with in memory metadata
  metadata3 <- cbind(metadata_large, metadata2)
  
  # add new column
  metadata_large$weightnew <- metadata$weight
  metadata_large[["weightnew2"]] <- metadata$weight
  
  # TODO: cbind two zarrdataframes doesnt work
  # set new metadata
  # meta.data_list <- list()
  # zarrcreateGroup(store = output_zarr, name = "assay2")
  # for(i in 1:ncol(metadata2)){
  #   cur_column <- as.vector(subset(metadata2, select = colnames(metadata2)[i]))[[1]]
  #   nchar <- NULL
  #   if(is.character(cur_column) || is.factor(cur_column)){
  #     cur_column <- as.character(cur_column)
  #     nchar <- max(vapply(cur_column, function(x) nchar(x), numeric(1)))
  #   }
  #   cur_column <- as.array(cur_column)
  #   meta.data_list[[colnames(metadata2)[i]]] <-
  #     Rarr::writeZarrArray(cur_column,
  #                          zarr_array_path = file.path(output_zarr, paste0("assay2", "/", colnames(metadata2)[i])),
  #                          chunk_dim = min(length(cur_column), 2000), nchar = nchar)
  # }
  # metadata2_large <- ZarrDataFrame::ZarrDataFrame(meta.data_list, name = "assay2", columns = names(meta.data_list))

  # merge with in memory metadata
  # metadata3 <- cbind(metadata_large, metadata2_large)
  

  # refresh
  unlink(output_zarr, recursive = TRUE)
})