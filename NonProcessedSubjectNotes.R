library(dplyr)


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
reason_df <- tibble()

yes <- 'YES'
no <- '.' #'no'
none <- ''
need_discuss <- '' #'Need to discuss'
cross <- 'X'
msg_sensor <- 'Sensor data is not appropriate.'
msg_file_missing <- 'Multiple file/data missing.'
msg_file_name <- 'File Name problem.'
msg_bioharness <- 'Zephyr bioharness data problem.'
msg_session_marker <- 'Session marker file/data missing.'
msg_s_interface <- 'S-interface crashed.'
msg_script_problem <- 'Some script problem'
msg_bad_subject <- 'Bad Subject'
msg_duplicate_subject <- 'This subject was used for another subjet due to S-interface crash'

note_back_up <- 'Backed up the data inside a zip file.'

#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

update_reason_list <- function(subj_name, reason, possibility='', resolved='', 
                               aws_update='', time_stamp_update='', notes='') {
  
  temp_reason_df <- tibble('Subject'=subj_name, 'Reason'=reason, 'Possibility'=possibility, 
                           'Resolved'=resolved, 'AWS_Update'=aws_update, 
                           'Time Stamp Update'=time_stamp_update, 'Notes'=notes)
  reason_df <<- rbind(reason_df, temp_reason_df)

}


#-------------------------#
#-------Main Program------#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)


#------------------------------------------- UH
reason_df <- tibble('Subject'='T011', 'Reason'=msg_bioharness, 'Possibility'=yes, 
                    'Resolved'=yes, 'AWS_Update'=yes, 'Time Stamp Update'=no, 
                    'Notes'=paste('There were multiple bioharness file.', note_back_up))
update_reason_list('T017', msg_s_interface, cross, yes, yes, cross, 
                   paste('S-interface crashed. We moved from T017 to T019.', note_back_up, ' At T019 directory.'))
update_reason_list('T021', msg_sensor, yes, yes, yes, no, 'The sensor data date does not matches with s-interface date. Need to get correct sensor data from amanveer.')
update_reason_list('T023', msg_bad_subject, no, no, yes, no, 'Discarded subject. S-interface crashed. We collected data for T023 and T029. But the first half pp was not good and no .dat file generated. Backed up at HD')
update_reason_list('T033', msg_duplicate_subject, no, no, yes, no, 'S-interface crashed. We collected data for T031 and T033. Done merging to T031 for all pp, breath data with relative time and update session_marker file.')
update_reason_list('T037', msg_s_interface, yes, yes, yes, no, 'S-interface crashed. We collected data for T037 and T039. Need to merge it to T037(Check already merged or not) all pp, breath data with relative time and update session_marker file. Need time for this.')
update_reason_list('T039', msg_duplicate_subject, no, no, yes, none, 'Copied everything to T037.')
update_reason_list('T049', msg_duplicate_subject, no, no, yes, none, 'Copied everything to T047.')



update_reason_list('T035', msg_script_problem, yes, no, no, no, 'Problem with date conversion. Check hms(intermittent_df$Baseline.Stress.Timestamp. Need to investigate more.)')
update_reason_list('T051', msg_script_problem, no, no, no, no, 'Problem with date conversion. Check the error msg. Need to investigate more.)')




#------------------------------------------- TAMU
update_reason_list('T062', msg_session_marker, need_discuss, no, no, none, 'No session data captured by the experimenter during the experiment. Might be S-interface issue also, as pp signal is very less.')
update_reason_list('T076', msg_session_marker, need_discuss, no, no, none, 'No session marker file. No pp signal file.')
update_reason_list('T081', msg_file_missing, need_discuss, no, no, none, 'Only pp and session marker file. No other file. No data for session marker.')
update_reason_list('T094', msg_session_marker, need_discuss, no, no, none, 'No session marker file.')
update_reason_list('T095', msg_file_missing, need_discuss, no, no, none, 'Only pp and session marker file. No other file. No data for session marker.')
update_reason_list('T107', msg_session_marker, need_discuss, no, no, none, 'No session data captured by the experimenter during the experiment. Might be S-interface issue also, as pp signal is very less.')
update_reason_list('T109', msg_session_marker, need_discuss, no, no, none, 'No session marker file.')
update_reason_list('T110', msg_file_missing, need_discuss, no, no, none, 'Only pp and session marker file. No other file. No data for session marker.')
update_reason_list('T111', msg_file_name, need_discuss, yes, no, none, 'Excel file named for T110. This might be added with discussing with TAMU')



#------------------------------------------- UCI
update_reason_list('T127', msg_session_marker, need_discuss, no, no, none, 'No session marker and pp signal file.')
update_reason_list('T142', msg_session_marker, need_discuss, no, no, none, 'No session marker, pp and sensor files.')
update_reason_list('T146', msg_session_marker, need_discuss, no, no, none, 'No session marker and pp signal file.')
update_reason_list('T148', msg_session_marker, need_discuss, no, no, none, 'No session marker, pp and sensor files.')
update_reason_list('T155', msg_session_marker, need_discuss, no, no, none, 'No session marker and pp signal file.')
update_reason_list('T158', msg_session_marker, need_discuss, no, no, none, 'No session marker file.')
update_reason_list('T167', msg_session_marker, need_discuss, no, no, none, 'No session marker and pp signal file.')
update_reason_list('T168', msg_file_missing, need_discuss, no, no, none, 'No session marker, pp signal and sensor data file.')
update_reason_list('T169', msg_file_missing, need_discuss, no, no, none, 'Only pp back up file. Delete the pp_backup file from AWS')
update_reason_list('T170', msg_file_missing, need_discuss, no, no, none, 'No excel file. Very less data for every signals.')
update_reason_list('T171', msg_file_missing, need_discuss, no, no, none, 'No excel file.')
update_reason_list('T180', msg_session_marker, need_discuss, no, no, none, 'No session marker file.')


convert_to_csv(reason_df, '@Datasets/subjects_not_processed.csv')




