library(dplyr)


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
msg_half_screen <- 'Half Screen Captured, No Sound'
msg_video_error <- 'Could not render the video'
msg_processed <- 'Processed'
msg_partial_processed <- 'Partially Processed'

manual_edit_df <- tibble()


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

update_manual_edit_list <- function(subj_name, msg, bw_Latency='', sc_Latency='', dt_Latency='', aws_updated='') {
  temp_manual_edit_df <- tibble("Subject" = subj_name, "Status" = msg, 'BW_Latency' = bw_Latency, 
                                'SC_Latency' = sc_Latency, 'DT_Latency' = dt_Latency, 'Aws_Updated' = aws_updated)
  manual_edit_df <<- rbind(manual_edit_df, temp_manual_edit_df)
}


#-------------------------#
#-------Main Program------#
#-------------------------#
full_df_file <- '@Datasets/full_df.csv'
full_df <- read.csv(full_df_file)

processed_subj_list <- unique(full_df$Subject)
processed_subj_list

manual_edit_df <- tibble("Subject" = 'T003', "Status" = msg_half_screen, 
                         'BW_Latency' = '', 'SC_Latency' = '', 'DT_Latency' = '', 'Aws_Updated' = '')


update_manual_edit_list('T005', msg_half_screen)
update_manual_edit_list('T006', msg_video_error)
update_manual_edit_list('T009', msg_half_screen)
update_manual_edit_list('T016', msg_half_screen)

# update_manual_edit_list('T005', msg_half_screen, '1min 55sec', '1min 40sec', '1min 31sec', 'No!')
# update_manual_edit_list('T006', msg_video_error, '39sec', '1min 4sec', '2min 27sec', 'No!')

update_manual_edit_list('T019', msg_processed, '32sec', '30sec', '1min 19sec', 'Yes')
update_manual_edit_list('T021', msg_processed, '20sec', '40sec', '1min 47sec', 'Yes')
update_manual_edit_list('T031', msg_processed, '28sec', '21sec', '2min 9sec', 'Yes')
update_manual_edit_list('T032', msg_processed, '1min 8sec', '20sec', '1min 10sec', 'Yes')
update_manual_edit_list('T046', msg_processed, '34sec', '9sec', '1min', 'Yes')
update_manual_edit_list('T047', msg_processed, '22sec', '23sec', '11min 12sec', 'Yes')
update_manual_edit_list('T051', msg_processed, '43sec', '20sec', '1min 28sec', 'Yes')

update_manual_edit_list('T061', msg_processed, '34sec', '3min 18sec', '14sec', 'Yes')
update_manual_edit_list('T063', msg_processed, '35sec', '3min 11sec', '29sec', 'Yes')
update_manual_edit_list('T064', msg_processed, '32sec', '3min 11sec', '20sec', 'Yes')
update_manual_edit_list('T065', msg_processed, '38sec', '3min 33sec', '20sec', 'Yes')
update_manual_edit_list('T066', msg_processed, '52sec', '3min 17sec', '15sec', 'Yes')
update_manual_edit_list('T067', msg_video_error)
update_manual_edit_list('T076', msg_processed, '6min 40sec', '3min 33sec', '2min 13sec', 'Yes')
update_manual_edit_list('T077', msg_processed, '40sec', '2min 51sec', '12sec', 'Yes')
update_manual_edit_list('T078', msg_processed, '50sec', '2min 53sec', '12sec', 'Yes')
update_manual_edit_list('T079', msg_processed, '34sec', '3min 2sec', '56sec', 'Yes')
update_manual_edit_list('T080', msg_video_error)
update_manual_edit_list('T124', msg_video_error)


convert_to_csv(manual_edit_df, '@Datasets/manual_time_edit_list.csv')




