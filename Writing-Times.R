
#devtools::install_github("statsmaths/coreNLP") 
#install.packages('rJava') 

options(java.parameters = "-Xmx4096m") 
library(rJava) 
library(tidyverse) 
#coreNLP::downloadCoreNLP() 
library(coreNLP) 
library(XLConnect) 
library(scales) 
library(readr) 
library(grid) 
library(gridExtra) 
library(cowplot) 

# CHANGE THIS! 
# Point this to the nsf-stress-study directory. 
# setwd("~/Desktop/")

current_dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(current_dir)

data_dir <- 'nsf-stress-study' 
super_session_pattern <- '^SuperSession$' 
plots <- list() 
result_df <- tibble() # IMPORTANT: global tibble here for scope purposes :) 

getAllDirectoryList <- function(directory) { 
  return(list.dirs(path=directory, full.names=F, recursive=F)) 
} 

getMatchedFileNames <- function(directory, file_pattern) { 
  return(list.files(path=directory, pattern=file_pattern, recursive=T)) 
} 

isMatchedString <- function(pattern, str) { 
  return(grepl(pattern, str, perl=TRUE)) 
} 

isMatch <- function(pattern, str) { 
  return(grepl(str, pattern)) 
} 

convert_to_csv <- function(df, file_path) { 
  write.table(df, file = file_path, row.names=F, sep = ',') 
} 


find_subjects <- function(cond) { 
  plots <<- list() 
  
  grp_list <- getAllDirectoryList(data_dir) 
  #sapply(grp_list, function(grp_name) { 
  sapply(grp_list, function(grp_name) { 
    
    grp_dir <- file.path(data_dir, grp_name) 
    subj_list <- getAllDirectoryList(grp_dir) 
    
    sapply(subj_list, function(subj_name) { 
    #sapply(subj_list[6], function(subj_name) { 
      
      subj_dir <- file.path(grp_dir, subj_name) 
      #session_list <- list.dirs(path=subj_dir, full.names=F, recursive=F) 
      session_list <- getAllDirectoryList(subj_dir) 
      session_list <- session_list[isMatchedString(super_session_pattern, session_list)] 
      
      sapply(session_list, function(session_name) { 
      #sapply(session_list[3], function(session_name) { 
        
        session_dir <- file.path(getwd(), subj_dir, session_name) 
        
        tryCatch({ 
          doCoreNLP(session_dir, subj_name, cond) 
        }, 
        warning=function(cond) { 
          message('----------------------------------------------------------') 
          message(paste0(grp_name, '-', subj_name, '-', session_name, ': WARNING')) 
          message(paste0(cond, '\n')) 
        }, 
        error=function(cond) { 
          message('----------------------------------------------------------') 
          message(paste0(grp_name, '-', subj_name, '-', session_name, ': ERROR!!')) 
          message(paste0(cond, '\n')) 
        }) 
      }) 
    }) 
  }) 
} 





doCoreNLP <- function(session_dir, subj_name, cond) { 
  
  subj_interface_file_pattern <- paste0('.*-', subj_name, '.xlsx') 
  subj_interface_file_name <- getMatchedFileNames(session_dir, subj_interface_file_pattern) 
  
  ## FIGURE OUT THE CONDITION 
  condition <- NA 
  if (isMatch(subj_interface_file_name, "intermittent-high")) { 
    condition <- "IH" 
  } else if (isMatch(subj_interface_file_name, "batch-high")) { 
    condition <- "BH" 
  } else if (isMatch(subj_interface_file_name, "intermittent-low")) { 
    condition <- "IL" 
  } else if (isMatch(subj_interface_file_name, "batch-low")) { 
    condition <- "BL" 
  } 
  
  if (condition != cond) { 
    return() 
  } 
  
  subj_interface_df <- readWorksheet(XLConnect::loadWorkbook( 
    file.path(session_dir, subj_interface_file_name)), sheet = 'Sheet1') 
  
  email_1_total <- subj_interface_df$Email.1.Response.Time 
  email_2_total <- subj_interface_df$Email.2.Response.Time 
  email_3_total <- subj_interface_df$Email.3.Response.Time 
  email_4_total <- subj_interface_df$Email.4.Response.Time 
  email_5_total <- subj_interface_df$Email.5.Response.Time 
  email_6_total <- subj_interface_df$Email.6.Response.Time 
  email_7_total <- subj_interface_df$Email.7.Response.Time 
  email_8_total <- subj_interface_df$Email.8.Response.Time 
  
  emails_total <- sum(email_1_total, email_2_total, email_3_total, email_4_total, 
                  email_5_total, email_6_total, email_7_total, email_8_total, na.rm = TRUE) 
  
  if (emails_total == 0) { 
    message(paste0("Error for subject ", subj_name, ": total email time is 0.")) 
    flush.console() 
    return() 
  } 
  
  essay_total <- (50 * 60) - emails_total 
  
  df <- tibble("Writing" = "Emails", "Time" = emails_total) 
  df <- rbind(df, tibble("Writing" = "Report", "Time" = essay_total)) 
  
  # result_df <<- rbind(result_df, tibble("Subject" = subj_name, "Condition" = condition, 
  #                                       "Report" = "DT", "Positive" = dual_task_positive_count, 
  #                                       "Neutral" = dual_task_neutral_count, 
  #                                       "Negative" = dual_task_negative_count, 
  #                                       "WordCount" = dual_task_words, 
  #                                       "SentenceCount" = dual_task_sentences)) 
  
  ## PLOTTING 
  g1 <- df %>% 
    mutate(Writing = factor(Writing, levels = c("Emails", "Report"))) %>% 
    ggplot(aes(x = Writing, y = Time)) + 
      geom_col() + 
      labs(title = paste0("Writing Times for ", subj_name), 
           x = "Writing Type", 
           y = "Time [s]") + 
      theme_bw() + 
      theme(panel.background=element_rect(fill = "NA"), 
            panel.grid.minor = element_line(colour = "#E0E0E0"), 
            axis.text.x = element_text(angle = 90, hjust = 1), 
            plot.title = element_text(size=11), 
            plot.subtitle = element_text(size=9, face="italic"), 
            legend.position="none") + 
      scale_x_discrete(drop=FALSE) + 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) 
  
  plots[[length(plots)+1]] <<- g1 
  
} 


## IH 
find_subjects("IH") 
title <- ggdraw() + draw_label("Intermittent Filler (IF) Writing Times", fontface='bold') 
gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9) 
gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1)) 
save_plot("nsf-stress-study-scripts/IH_writing_times.pdf", gg, ncol = 2, base_height = 20, base_width = 7)
# save_plot("nsf-stress-study-files/IH_writing_times.pdf", gg, ncol = 2, base_height = 20, base_width = 7) 

## IL 
find_subjects("IL") 
title <- ggdraw() + draw_label("Intermittent Nothing (IN) Writing Times", fontface='bold') 
gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9) 
gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1)) 
save_plot("nsf-stress-study-scripts/IL_writing_times.pdf", gg, ncol = 2, base_height = 20, base_width = 7)
# save_plot("nsf-stress-study-files/IL_writing_times.pdf", gg, ncol = 2, base_height = 20, base_width = 7) 

## BH 
find_subjects("BH") 
title <- ggdraw() + draw_label("Batch Filler (BF) Writing Times", fontface='bold') 
gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9) 
gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1)) 
save_plot("nsf-stress-study-scripts/BH_writing_times.pdf", gg, ncol = 2, base_height = 20, base_width = 7)
# save_plot("nsf-stress-study-files/BH_writing_times.pdf", gg, ncol = 2, base_height = 20, base_width = 7) 

## BL 
find_subjects("BL") 
title <- ggdraw() + draw_label("Batch Nothing (BN) Writing Times", fontface='bold') 
gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9) 
gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1)) 
save_plot("nsf-stress-study-scripts/BL_writing_times.pdf", gg, ncol = 2, base_height = 20, base_width = 7)
# save_plot("nsf-stress-study-files/BL_writing_times.pdf", gg, ncol = 2, base_height = 20, base_width = 7) 
