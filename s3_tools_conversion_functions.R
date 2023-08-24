# please see the accompanying notes available here (update link when published)
# https://github.com/moj-analytical-services/user-guidance/blob/botor/source/documentation/appendix-docs/botor.md#replacement-functions
library(botor)


read_using <- function(FUN, s3_path, ...) {
  # trim s3:// if included by the user
  s3_path <- paste0("s3://", gsub('^s3://', "", s3_path))
  # find fileext
  file_ext <- paste0('.', tolower(tools::file_ext(s3_path)))
  # download file to tempfile()
  tmp <- botor::s3_download_file(s3_path, 
                                 tempfile(fileext = file_ext), 
                                 force = TRUE)
  FUN(tmp, ...)
}

# if you are using a file with .gz, .bz or .xz extension, please use
# botor::s3_read directly
s3_path_to_full_df <- function(s3_path, ...) {
  # trim s3:// if included by the user
  s3_path <- paste0('s3://', gsub('^s3://', "", s3_path))
  message(s3_path)
  # fileexts accepted by s3_read
  accepted_direct_fileext <- c('csv' = read.csv, 
                               'json' = jsonlite::fromJSON,
                               'jsonl' = jsonlite::stream_in,
                               'rds' = readRDS,
                               'sas7bdat' = haven::read_sas,
                               'sav' = haven::read_spss,
                               'dta' = haven::read_stata)
  # specify all other accepted filetypes
  excel_filepaths <- c('xlsx', 'xls', 'xlsm')
  accepted_fileext <- c(names(accepted_direct_fileext), excel_filepaths)
  fileext <- tolower(tools::file_ext(s3_path))
  # error if invalid filepath is entered
  if(!grepl(paste0('(?i)', accepted_fileext, collapse = "|"), fileext)) {
    stop(paste0("Invalid filetype entered. Please confirm that your file",
                " extension is one of the following: ", 
                paste0(accepted_fileext, collapse = ', '), ". \n ",
                "Alternatively, use botor directly to read in your file."))
  }
  # if we are using a function accepted by s3_read, then use that to parse 
  # the data
  if(grepl(paste0('(?i)', names(accepted_direct_fileext), collapse = "|"), 
           fileext)) {
    # read from s3 using our designated method
    tryCatch({
      botor::s3_read(s3_path, fun = accepted_direct_fileext[[tolower(fileext)]])
    },
    error = function(cond){
      message("Here's the original error message:")
      message(cond)
      stop("\nError, file cannot be parsed. \nYou either don't have access to this bucket, or are using an invalid s3_path argument (the s3_path you've entered needs correcting).")
    })
    
  } else {
    tryCatch({
      read_using(FUN = readxl::read_excel, s3_path = s3_path, ...)
    },
    error = function(cond){
      message("Here's the original error message:")
      message(cond)
      stop("\nError, file cannot be parsed. \nYou either don't have access to this bucket, or are using an invalid s3_path argument (the s3_path you've entered needs correcting).")
    })
    
  }
}

s3_path_to_preview_df = function(s3_path, ...) {
  s3_path <- stringr::str_replace(s3_path, "s3://", "")
  split_path <- stringr::str_split(s3_path, "/")[[1]]
  bucket <- split_path[1]
  key <- stringr::str_c(split_path[-1], collapse="/")    
  fext <- tolower(tools::file_ext(key))
  if (!(fext %in% c("csv", "tsv"))) {
    message(stringr::str_glue("Preview not supported for {fext} files"))
    NULL
  } else {
    tryCatch(
      {
        client <- botor::botor()$client("s3")
        obj <- client$get_object(Bucket = bucket, Key = key,
                                 Range = "bytes=0-12000")
        obj$Body$read()$decode() %>%
          textConnection() %>%
          read.csv() %>%
          head(n = 5)
      },
      error = function(c) {
        message("Could not read ", s3_path)
        stop(c)
      }
    )
  }
}

download_file_from_s3 <- function(s3_path, local_path, overwrite = FALSE) {
  # trim s3:// if included by the user and add it back in where required
  s3_path <- paste0("s3://", gsub('^s3://', "", s3_path))
  if (!(file.exists(local_path)) || overwrite) {
    local_path_folders <- stringr::str_extract(local_path, ".*[\\/]+")
    if(!is.na(local_path)) {
      dir.create(local_path_folders, showWarnings = FALSE, recursive = TRUE)
    }
    # download file
    tryCatch({
      # download file to tempfile()
      botor::s3_download_file(s3_path, 
                              local_path, 
                              force = overwrite)
    },
    error = function(cond){
      stop("\nError, file cannot be found. \nYou either don't have access to this bucket, or are using an invalid s3_path argument (file does not exist).")
    })
    
    
    
  } else {
    stop(paste0("The file already exists locally and you didn't specify", 
                " overwrite=TRUE"))
  }
}

write_df_to_csv_in_s3 <- function(df, s3_path, overwrite = FALSE,
                                  multipart = "unused", ...) { 
  # add errors
  if(!any(grepl('data.frame', class(df)))) {
    stop("df entered isn't a valid dataframe object")
  }
  if(tools::file_ext(s3_path) != 'csv') {
    stop("s3_path entered is either not a csv or is missing the .csv suffix")
  }
  # trim s3:// if included by the user - removed so we can supply both 
  # alpha-... and s3://alpha - and then add again
  s3_path <- paste0("s3://", gsub('^s3://', "", s3_path))
  if(!overwrite & botor::s3_exists(s3_path)) {
    stop("s3_path entered already exists and overwrite is FALSE")
  }
  # write csv
  botor::s3_write(df, fun = write.csv, uri = s3_path, ...)
}

write_df_to_table_in_s3 <- function(df, s3_path, overwrite = FALSE,
                                    multipart = "unused", ...) { 
  # add errors
  if(!any(grepl('data.frame', class(df)))) {
    stop("df entered isn't a valid dataframe object")
  }
  if(tolower(tools::file_ext(s3_path)) != 'csv') {
    stop("s3_path entered is either not a csv or is missing the .csv suffix")
  }
  # trim s3:// if included by the user - removed so we can supply both 
  # alpha-... and s3://alpha - and then add again
  s3_path <- paste0("s3://", gsub('^s3://', "", s3_path))
  if(!overwrite & botor::s3_exists(s3_path)) {
    stop("s3_path entered already exists and overwrite is FALSE")
  }
  # write csv
  botor::s3_write(df, fun = write.table, uri = s3_path, ...)
}

write_file_to_s3 <- function(local_file_path, s3_path, overwrite=FALSE, 
                             multipart = "unused") {
  # ensure s3:// is present if not already
  s3_path <- paste0("s3://", gsub("^s3://", "", s3_path))
  if (overwrite || !(botor::s3_exists(s3_path))) {
    tryCatch(
      botor::s3_upload_file(local_file_path, s3_path),
      error = function(c) {
        message(paste0("Could not upload ", local_file_path, " to ", s3_path),
                appendLF = TRUE)
        stop(c, appendLF = TRUE)
      }
    )
    
  } else {
    stop("File already exists and you haven't set overwrite = TRUE, stopping")
  }
}

list_files_in_buckets <- function(bucket_filter = NULL, prefix = NULL,
                                  path_only = FALSE, max = "unused") {
  historic_column_names <- c(
    "key" = "key",
    "last_modified" = "lastmodified",
    "size" = "size",
    "bucket_name" = "bucket",
    "path" = "path"
  )
  
  if (is.null(bucket_filter)) {
    stop(paste0("You must provide one or more buckets e.g. ",
                "accessible_files_df('alpha-everyone')  This function will ",
                "list their contents"))
  }
  if(!is.character(bucket_filter)) {
    stop("Supplied bucket_filter is not of class: character")
  }
  if(!is.character(prefix)&!is.null(prefix)) {
    stop("Supplied prefix is not of class: character")
  }
  list_files_in_bucket <- function(bucket) {
    # trim s3:// if included by the user - removed so we can supply both
    # alpha-... and s3://alpha-...
    bucket <- gsub('^s3://', "", bucket)
    cols_to_keep <- c("key","last_modified","size","bucket_name")
    path_prefix <- (paste0('s3://', bucket, "/", prefix))
    list <- botor::s3_ls(path_prefix)
    if (is.null(list)) {
      warning(path_prefix, ' matches 0 files')
      return(list)
    }
    list <- list[,cols_to_keep]
    list["path"] <- paste(list$bucket_name, list$key, sep = '/')
    if(is.null(prefix)) {
      return(list)
    } else {
      return(list[grepl(prefix, list$key, ignore.case = TRUE),])
    }
  }
  file_list <- dplyr::bind_rows(purrr::map(bucket_filter, 
                                           list_files_in_bucket))
  if(is.numeric(max)) file_list <- head(file_list, max)
  # apply some finishing touches so it aligns with s3tools version
  colnames(file_list) <- stringr::str_replace_all(colnames(file_list), historic_column_names)
  file_list[["filename"]] = coalesce(stringr::str_extract(file_list$key, "[^\\/]+$"), stringr::str_replace_all(file_list$key, "\\/", ""))
  
  if (path_only) return(file_list$path)
  file_list
}