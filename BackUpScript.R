#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
getMatchedFileNames <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=T))
}

getMatchedFileNamesFullPath <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F, full.names=T))
}

getAllDirectoryList <- function() {
  return(list.dirs(path=getwd(), full.names=F, recursive=F))
}

DeleteSuperSession <- function() {
  subj_list <- getAllDirectoryList()
  
  sapply(subj_list, function(subj_name) {
    # subj_interface_file_name <- getMatchedFileNames(file.path(getwd(), subj_name), '.*.xlsx')
    subj_interface_file_path <- getMatchedFileNamesFullPath(file.path(getwd(), subj_name), '.*.xlsx')
    print(subj_interface_file_path)
    # file.copy(subj_interface_file_path, dirname(dirname(subj_interface_file_path)))
    # file.rename(dirname(subj_interface_file_path), gsub("SuperSession", "Back Up Files", dirname(subj_interface_file_path)))
    
  })
}


#-------------------------#
#-------Main Program------#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)


DeleteSuperSession()

