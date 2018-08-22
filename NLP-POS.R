
#devtools::install_github("statsmaths/coreNLP") 
#install.packages('rJava') 

current_dir <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(current_dir) 

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
library(ngram) 


data_dir <- 'nsf-stress-study' 
super_session_pattern <- '^SuperSession$' 
plots <- list() 
result_df <- tibble() # IMPORTANT: global tibble here for scope purposes :) 

POS_vec <- c("CC", 
"CD", 
"DT", 
"EX", 
"FW", 
"IN", 
"JJ", 
"JJR", 
"JJS", 
"LS", 
"MD", 
"NN", 
"NNS", 
"NNP", 
"NNPS", 
"PDT", 
"POS", 
"PRP", 
"PRP$", 
"RB", 
"RBR", 
"RBS", 
"RP", 
"SYM", 
"TO", 
"UH", 
"VB", 
"VBD", 
"VBG", 
"VBN", 
"VBP", 
"VBZ", 
"WDT", 
"WP", 
"WP$", 
"WRB", 
"Other" 
) 


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


# RUN THIS ONCE! 
# IF YOU RUN OUT OF MEMORY, THEN RESTART R AND RUN AGAIN 
# IF THIS DOESN'T WORK, INCREASE `mem` TO MORE RAM 
# IF THAT DOESN'T WORK, GOOGLE IT 
# https://cran.r-project.org/web/packages/coreNLP/coreNLP.pdf 
# initCoreNLP(mem = "4g") 


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
          if (subj_name != "T067") { 
            doCoreNLP(session_dir, subj_name, cond) 
          } else { 
            message('----------------------------------------------------------') 
            message(paste0("NOPE: ", subj_name, ' has no valid writings. ')) 
            message(paste0(cond, '\n')) 
          } 
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
  # CHECK WHY THE PLOTS ARE EMPTY
  # print('------------------------------')
  # print(paste0('.*-', subj_name, '.xlsx'))
  # print(session_dir)
  # print(getMatchedFileNames(session_dir, subj_interface_file_pattern))
  
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
  
  baseline_essay <- subj_interface_df$Essay.Baseline.Content 
  dual_task_essay <- subj_interface_df$Essay.Dualtask.Content 
  
  ## DO THE NLP 
  baseline_annotated = annotateString(baseline_essay) 
  dual_task_annotated = annotateString(dual_task_essay) 
  
  baseline_words <- wordcount(baseline_essay) 
  dual_task_words <- wordcount(dual_task_essay) 
  
  baseline_tokens <- getToken(baseline_annotated) %>% 
    mutate(POS = factor(POS, levels = POS_vec)) 
  dual_task_tokens <- getToken(dual_task_annotated) %>% 
    mutate(POS = factor(POS, levels = POS_vec)) 
  baseline_tokens[is.na(baseline_tokens$POS), ] <- "Other" 
  dual_task_tokens[is.na(dual_task_tokens$POS), ] <- "Other" 
  
  baseline_tokens_num <- nrow(baseline_tokens) 
  dual_task_tokens_num <- nrow(dual_task_tokens) 
  
  
  ## PLOTTING 
  g1 <- baseline_tokens %>% 
    ggplot(aes(x = POS)) + 
    geom_bar() + 
    labs(title = paste0("WB Essay for ", subj_name), 
         subtitle = paste0("Token Count: ", baseline_tokens_num), 
         x = "POS", 
         y = "Token Count") + 
    theme_bw() + 
    theme(panel.background=element_rect(fill = "NA"), 
          panel.grid.minor = element_line(colour = "#E0E0E0"), 
          axis.text.x = element_text(angle = 90, hjust = 1), 
          plot.title = element_text(size=11), 
          plot.subtitle = element_text(size=9, face="italic"), 
          legend.position="none") + 
    scale_x_discrete(drop=FALSE) + 
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) 
  
  g2 <- dual_task_tokens %>% 
    ggplot(aes(x = POS)) + 
    geom_bar() + 
    labs(title = paste0("DT Essay for ", subj_name), 
         subtitle = paste0("Token Count: ", dual_task_tokens_num), 
         x = "POS") + 
    theme_bw() + 
    theme(panel.background=element_rect(fill = "NA"), 
          panel.grid.minor = element_line(colour = "#E0E0E0"), 
          axis.text.x = element_text(angle = 90, hjust = 1), 
          plot.title = element_text(size=11), 
          plot.subtitle = element_text(size=9, face="italic"), 
          legend.position="none", 
          axis.title.y = element_blank()) + 
    scale_x_discrete(drop=FALSE) + 
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) 
  
  g3 <- plot_grid(g1, g2, ncol = 2, align = "v") 
  
  plots[[length(plots)+1]] <<- g3 
} 


## IH 
find_subjects("IH") 
title <- ggdraw() + draw_label("Intermittent Filler (IF) Essays POS", fontface='bold') 
gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9) 
gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1)) 
save_plot("nsf-stress-study-scripts/IH_essays_POS.pdf", gg, ncol = 2, base_height = 30, base_width = 14) 

## IL 
find_subjects("IL") 
title <- ggdraw() + draw_label("Intermittent Nothing (IN) Essays POS", fontface='bold') 
gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9) 
gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1)) 
save_plot("nsf-stress-study-scripts/IL_essays_POS.pdf", gg, ncol = 2, base_height = 20, base_width = 7) 

## BH 
find_subjects("BH") 
title <- ggdraw() + draw_label("Batch Filler (BF) Essays POS", fontface='bold') 
gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9) 
gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1)) 
save_plot("nsf-stress-study-scripts/BH_essays_POS.pdf", gg, ncol = 2, base_height = 20, base_width = 7) 

## BL 
find_subjects("BL") 
title <- ggdraw() + draw_label("Batch Nothing (BN) Essays POS", fontface='bold') 
gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9) 
gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1)) 
save_plot("nsf-stress-study-scripts/BL_essays_POS.pdf", gg, ncol = 2, base_height = 20, base_width = 7) 
