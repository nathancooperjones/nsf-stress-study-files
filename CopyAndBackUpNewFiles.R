pp_file_pattern <- '.*_pp.csv'


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
getAllDirectoryList <- function(directory) {
  return(list.dirs(path=directory, full.names=F, recursive=F))
}

getMatchedFileNames <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F))
}

getMatchedFileNamesRecursively <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=T))
}

getMatchedFileNamesFullPath <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F, full.names=T))
}

copyReExtractedDataToNsfDir <- function() {
  subj_list <- getAllDirectoryList(re_extract_pp_dir)
  
  sapply(subj_list, function(subj_name) {
    subj_dir <- file.path(getwd(), re_extract_pp_dir, subj_name)
    subj_serial <- as.numeric(substr(subj_name, 2, 4))
    
    pp_file_name <- getMatchedFileNames(subj_dir, pp_file_pattern)
    pp_file_full_path <- getMatchedFileNamesFullPath(subj_dir, pp_file_pattern)
    
    if(!isEmpty(pp_file_name)) {
      grp_name <- paste0('Group', toString(subj_serial%/%60 + 1))
      pp_new_file_name <- paste0(substr(pp_file_name, 1, nchar(pp_file_name)-4), '_new.csv')
      pp_new_dest_path <- file.path(getwd(), data_dir, grp_name, subj_name, 'SuperSession', pp_new_file_name)
      file.copy(from = pp_file_full_path, to = pp_new_dest_path)
    }
  })
}





#-------------------------#
#-------Main Program------#
#-------------------------#
# CHANGE THIS 
current_dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(current_dir)

copyReExtractedDataToNsfDir()