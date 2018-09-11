
## READ ME FIRST!! 
## You will need to adjust paths for different files. Do a 'find' search for the comment with CHANGE THIS to find areas to change. 
## I have done my best to comment all important blocks of code to ensure you know what they do. 
## Enjoy the script and my attempt at humorous comments! 

# CHANGE THIS 
# Really nothing for you to change here, just wanted to let you know that if you didn't run `AllSignals.Rmd` first 
# then this script WILL NOT WORK. Don't be foolish - just do it! 

# CHANGE THIS 
# You might already notice there is another file names `HypothesisTesting_v2.0_reduced.Rmd` that is awfully similar to this one. 
# That's because it is. I will NOT comment that one nearly as much as this one because it is almost exactly the same, 
# just reduced. 
# ... 
# Oh yeah, nothing to change here either. Sorry! 


# You already know. 
library(tidyverse) 
library(knitr) 
library(grid) 
library(gridExtra) 
#install.packages("devtools") 
#devtools::install_github("kassambara/ggpubr") 
library(ggpubr) 
library(kableExtra) 


# CHANGE THIS 
# Go ahead, I will wait. 
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)
# setwd("~/Desktop/nsf-stress-study-files/") 
full_df <- read_csv("@Datasets/full_df.csv", col_types = cols()) 
full_df$HR <- as.numeric(full_df$HR) 

# Setting up three (that's right, three!) whole global tibbles. Wow! 
result_df <- tibble() 
t_test_df <- tibble() 
final_testing_df <- tibble() 

isMatch <- function(string_we_have, pattern_we_are_looking_for) { 
  return(grepl(pattern_we_are_looking_for, string_we_have)) 
} 




# Like "Where's Waldo?" but way less fun. 
find_subjects <- function() { 
  
  for (subj_name in levels(factor(full_df$Subject))) { 
    
    df <- full_df %>% 
      filter(Subject == subj_name) 
    
    tryCatch({ 
      
      condition <- df$Condition[1] 
      df <- df[ , !(names(df) %in% c("Subject", "Condition", "Task"))] 
      
      process_subject(df, subj_name, condition) 
    }, warning = function(w) { 
      message(paste0("Warnings present for subject ", subj_name, ": ", w, " Continuing process anyway.")) 
      flush.console() 
      return() 
    }, error = function(e) { 
      message(paste0("Error present for subject ", subj_name, ": ", e, " ")) 
      flush.console() 
      return() 
    }) 
  } 
} 



# This function takes a subject and computes the proper dataframe. 
# This involves renaming some columns, adding in the subject's condition, etc., then `rbinds()` it to the global `result_df`. 
# NOTE that this does NOT edit the global dataframe result_df, refer to `mess_with_columns` below for that. 
# This is like `mess_with_columns`'s little brother - close, but not quite there yet. 
process_subject <- function(df, subj_name, condition) { 
  
  # Test to see if the dataframe df actually has some rows in it and it is not just empty (more than five is good enough for me)! 
  if (nrow(df) <= 5) { 
    message(paste0("Dataframe for subject ", subj_name, " is empty. ")) 
    flush.console() 
    return() 
  } 
  
  counting_df <- df[df$Session == "DualTask", ] 
  sep <- floor(nrow(counting_df) / 4) 
  counting_df[1:sep, "Session"] <- "DualTask1" 
  counting_df[(sep):(sep*2), "Session"] <- "DualTask2" 
  counting_df[(sep*2):(sep*3), "Session"] <- "DualTask3" 
  counting_df[(sep*3):(nrow(counting_df)), "Session"] <- "DualTask4" 
  
  final_subject_df <- rbind(df[df$Session %in% c("RestingBaseline", "BaselineWriting", "StressCondition"), ], counting_df, 
                            df[df$Session == "Presentation", ]) 
  
  # Convert each of the Sessions into a factor so R knows there is an order to these! 
  all_sessions <- c("RestingBaseline", "BaselineWriting", "StressCondition", "DualTask1", "DualTask2", "DualTask3", "DualTask4", "Presentation") 
  final_subject_df$Session <- factor(final_subject_df$Session, levels = all_sessions) 
  
  # We now group by session and find the mean values for ALL of the columns for that subject with the whole fancy thing. Cool, right? 
  temp_df <- final_subject_df %>% 
    group_by(Session) %>% 
    summarize_all(mean, na.rm = TRUE) %>% 
    mutate(Subject = subj_name, Condition = condition) 
  
  # We now add this subject's mean df on to the global dataframe and move on to the next one. NEXT! 
  result_df <<- rbind(result_df, temp_df) 
  
} 





# This function adjusts the global dataframe, result_df, after all of the subjects' own mean dataframes have been added. 
mess_with_the_columns <- function() { 
  
  # We now get rid of columns we don't need any more and adjust the order of our final dataframe, result_df. 
  col_names_final <- c("Subject", "Condition", "Session", "PP", "HR", "BR", "D.EDA", "N.EDA", "D.HR", "N.HR") 
  result_df <<- result_df[ , (colnames(result_df) %in% col_names_final)] 
  result_df <<- result_df[col_names_final] 
  
} 



# We finally made it! Now we can finally compute all the tests. 
# Wasn't that exhausting to go through just to get here? 
# Yeah, I know. 
# I had to write it and then re-write it to get it here. 
# I hope you can feel my pain. 
test_time <- function(condition, col, full = TRUE) { 
  
  # Set a global variable to determine which test we do: 
  # 't' = t-test 
  # 'tt' = transformed t-test 
  # 'w' = Wilcoxon test 
  test_type <- "t" 
  end_str <- NA 
  
  # Firstly, get rid of missing values. Duh! 
  temp <- result_df[!is.na(result_df[[col]]), ] 
  
  # Ensure that n >= 7 for these conditions, or else we can run any tests on it since there aren't enough subjects. 
  # Note that I compute n in a really fancy, dynamic way that makes sense once you read it. 
  # Why do I do this? Because there is one row for each session, so that means if T0XX has a `PP` value, there is a row for 
  # RestingBaseline, WritingBaseline, ..., and Presentation, making it 5 rows for a single subject. Because we want n >= 7, 
  # we really want to test that there n is greater than the number of rows for that subject, meaning if each subject has 5 rows, 
  # one for each session, n has to be >= 35. 
  # Aren't I smart! 
  n <- length(levels(factor(temp$Session))) * 7 
  
  # Now we can filter by condition and ensuring that we have enough subjects for that condition based on the `n` computed right above. 
  temp <- temp %>% 
    group_by(Condition) %>% 
    filter(Condition == condition && n() >= n) 
  
  # I guess we didn't have enough subjects for this condition! 
  if (nrow(temp) == 0) { 
    # THIS IS IMPORTANT! 
    # Because this function gets called twice (you will see why later), we don't want to print out the same thing twice! 
    # So, I very GENIUSLY use a variable to dictate when to print something to the console and when to print it to the screen. Ha ha! 
    if (!full) { 
      message(paste0(condition, " has LESS than 7 subjects for ", col, ". Cannot continue with test. ")) 
      flush.console() 
    } else { 
      cat(paste0(condition, " has LESS than 7 subjects for ", col, ". Cannot continue with test. \n-----\n")) 
    } 
  } else { # This means that we have 7 or more subjects, so we can actually do a test now! :) 
    
    # Ensure that we always log for PP and EDAs. 
    # Of course to do that, we have to make sure all values are above 0 and shift up by the minimum value (plus a bit more) if not! 
    # We make this a permanent change! 
    # EDIT: Professor wanted these measurements logged no matter what - regardless of whether it is inherently Normal or not. 
    if (col == "PP" || col == "D.EDA" || col == "N.EDA") { # we don't want to do it for these columns AGAIN, now do we? 
      shift_val <- 0 
      if (min(temp[[col]]) <= 0) { 
        shift_val <- min(temp[[col]]) + 0.001 
      } 
      temp[[col]] <- log(temp[[col]]) + shift_val 
    } 
    
    # We now perform the Shapiro-Wilks test to test if our data is naturally Normal! 
    # EDIT: Here we make a variable `temp_col` that points to the actual dataframe's column (temp[[col]]). We do this so that 
    # `temp_col` acts as a practice dummy in a way - we first log `temp_col` to see if we like the results. If we do, then 
    # we know it is safe and can permanently make the change for `temp[[col]]`. If we log it and hate it, `temp_col` is great 
    # because we never made that change permanently, so we can just continue on with the calculations with the unmodified column. 
    temp_col <- temp[[col]] 
    shapiro_test_results <- shapiro.test(temp_col) 
    # This line below will make much more sense in just a bit... 
    temp_for_plotting <- temp 
    if (shapiro_test_results$p.value < 0.05) { 
      if (!full) { 
        message(paste0(col, " is NOT normal (p = ", round(shapiro_test_results$p.value, 4), "). Adjusting with `log` now. ")) 
        flush.console() 
      } 
      # Dang, it wasn't Normal, so try correcting with a shift and then ln. 
      if (col != "PP" && col != "D.EDA" && col != "N.EDA") { 
        shift_val <- 0 
        if (min(temp_col) <= 0) { 
          shift_val <- min(temp_col) + 0.001 
          if (!full) { 
            message(paste0(col, " values less than 0, shifted each value up by ", shift_val, ". ")) 
            flush.console() 
          } 
        } 
        temp_col <- log(temp_col) + shift_val 
      } 
      
      shapiro_test_results <- shapiro.test(temp_col) 
      
      # We preserve the current status of our dataframe by saving it for plotting later without any log adjustments to it. 
      # Why? Because I was told to make it like this. 
      temp_for_plotting <- temp 
      
      # Okay, the moment we have all been waiting for: do we have Normal data? 
      if (shapiro_test_results$p.value < 0.05) { 
        if (full) { 
          end_str <- paste0("In the following tests, we applied ln(", col, "). \n\n") 
        } else { 
          message(paste0("Transformed data for ", col, " is still NOT normal (p = ", round(shapiro_test_results$p.value, 4), 
                         "). Continuing with Wilcoxn test. ")) 
          flush.console() 
        } 
        
        # Dang, STILL not normal, so correct with ln anyway (per Professor Pavlidis' request), but this time permanently. 
        test_type <- "w" 
        if (col != "PP" && col != "D.EDA" && col != "N.EDA") { 
          shift_val <- 0 
          if (min(temp[[col]]) <= 0) { 
            shift_val <- min(temp[[col]]) + 0.001 
          } 
          temp[[col]] <- log(temp[[col]]) + shift_val 
        } 
      } else { 
        if (!full) { 
          message(paste0("Transformed data for ", col, " is now normal (p = ", round(shapiro_test_results$p.value, 4), 
                         "). Continuing with t-test. ")) 
          flush.console() 
        } 
        
        # We are doing a transformed t-test, so we still have to permanently log our data for this column! 
        test_type <- "tt" 
        if (col != "PP" && col != "D.EDA" && col != "N.EDA") { 
          shift_val <- 0 
          if (min(temp[[col]]) <= 0) { 
            shift_val <- min(temp[[col]]) + 0.001 
          } 
          temp[[col]] <- log(temp[[col]]) + shift_val 
        } 
      } 
    } else { 
      # Wow, it was actually Normal first try? Neat! 
      if (!full) { 
        message(paste0(col, " is normal (p = ", round(shapiro_test_results$p.value, 4), "). Continuing with t-test. ")) 
        flush.console() 
      } 
      test_type <- "t" 
    } 
    
    conditions <- levels(factor(temp$Condition)) 
    for (cond in conditions) { 
      
      # One condition at a time, please. 
      temp_cond <- temp %>% 
        filter(Condition == cond) 
      
      # This is it - the most complicated code segment of the whole script. This is just gross. 
      # I won't even try explaining it here. Just trust that it works or figure it out on your own through 
      # several rounds of printing, experimenting, and messing up before finally getting it. 
      testing_df <- temp_cond %>% 
        gather(Measurement, Value, -Subject, -Condition, -Session) %>% 
        spread(Session, Value) %>% 
        filter(Measurement == col) %>% 
        filter(!is.na(col)) %>% 
        mutate(WB.RB = BaselineWriting - RestingBaseline, 
               SC.RB = StressCondition - RestingBaseline, 
               DT1.RB = DualTask1 - RestingBaseline, 
               DT2.RB = DualTask2 - RestingBaseline, 
               DT3.RB = DualTask3 - RestingBaseline, 
               DT4.RB = DualTask4 - RestingBaseline, 
               P.RB = Presentation - RestingBaseline) 
      
      # We set up a tibble, `t_df`, to put our test results into. 
      t_df <- tibble() 
      # If the column is one listed below, per Professor Pavlidis, we just continue with a t-test anyway since the 
      # format of the data inherently doesn't work with a Wilcoxon. We just force it as a t-test because we have all 
      # the power here and can do whatever we want. 
      if (col == "PP" || col == "D.EDA" || col == "N.EDA") { 
        if (!full) { 
          message(paste0(col, " is close enough to normal. Continuing with t-test. ")) 
          flush.console() 
        } 
        test_type = "tt" 
      } 
      
      # This code is for a t-test or a transformed t-test (basically the same thing, but with a log transformation). 
      if (test_type == "t" | test_type == "tt") { 
        
        name <- "t-test" 
        if (test_type == "tt") { 
          name <- "Transformed t-test" 
        } 
        
        # We document the name of the test. 
        test <- "Writing Baseline - Resting Baseline" 
        # We calculate the p-value for this session difference. 
        p <- t.test(testing_df[["WB.RB"]])$p.value 
        # We find the sign of our results (this is the '*' thing we put in our two plots way up above). 
        sign <- interpret_results(p) 
        # Then we add EVERYTHING to that tibble we made earlier to hold it for us (wow, how nice!). 
        t_df <- rbind(t_df, tibble(Difference = "WB.RB", Measure = col, Condition = cond, p = p, 
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(WB.RB))), Significance = sign)) 
        
        # And now just rinse and repeat a million times. 
        test <- "Stress Condition - Resting Baseline" 
        p <- t.test(testing_df[["SC.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "SC.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(SC.RB))), Significance = sign)) 
        
        test <- "Dual Task 1 - Resting Baseline" 
        p <- t.test(testing_df[["DT1.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT1.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT1.RB))), Significance = sign)) 
        
        test <- "Dual Task 2 - Resting Baseline" 
        p <- t.test(testing_df[["DT2.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT2.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT2.RB))), Significance = sign)) 
        
        test <- "Dual Task 3 - Resting Baseline" 
        p <- t.test(testing_df[["DT3.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT3.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT3.RB))), Significance = sign)) 
        
        test <- "Dual Task 4 - Resting Baseline" 
        p <- t.test(testing_df[["DT4.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT4.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT4.RB))), Significance = sign)) 
        
        test <- "Presentation - Resting Baseline" 
        p <- t.test(testing_df[["P.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "P.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(P.RB))), Significance = sign)) 
        
        # Not a t-test or a transformed t-test? Don't worry - we got you covered with our friend, Wilcoxon! 
      } else if (test_type == "w" ) { 
        
        name <- "Wilcoxon" 
        
        test <- "Writing Baseline - Resting Baseline" 
        p <- wilcox.test(testing_df[["WB.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "WB.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(WB.RB))), Significance = sign)) 
        
        test <- "Stress Condition - Resting Baseline" 
        p <- wilcox.test(testing_df[["SC.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "SC.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(SC.RB))), Significance = sign)) 
        
        test <- "Dual Task 1 - Resting Baseline" 
        p <- wilcox.test(testing_df[["DT1.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT1.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT1.RB))), Significance = sign)) 
        
        test <- "Dual Task 2 - Resting Baseline" 
        p <- wilcox.test(testing_df[["DT2.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT2.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT2.RB))), Significance = sign)) 
        
        test <- "Dual Task 3 - Resting Baseline" 
        p <- wilcox.test(testing_df[["DT3.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT3.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT3.RB))), Significance = sign)) 
        
        test <- "Dual Task 4 - Resting Baseline" 
        p <- wilcox.test(testing_df[["DT4.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT4.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT4.RB))), Significance = sign)) 
        
        test <- "Presentation - Resting Baseline" 
        p <- wilcox.test(testing_df[["P.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "P.RB", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(P.RB))), Significance = sign)) 
        
      } 
      
      # We create a list with this tibble we just made, the test test, column, condition, and special end string to print out if we need to 
      # and then we return that, which ends up going towards the code to create the plots! 
      # Pretty genius and excessive, no? 
      if (nrow(final_testing_df) > 0) { 
        final_testing_df <<- rbind(final_testing_df, testing_df) 
      } else { 
        final_testing_df <<- testing_df 
      } 
      return(NA) 
    } 
  } 
  return(NA) 
} 





# Whew! We made it! Now let's go on to the easier functions that are WAY more self-explanatory! 
interpret_results <- function(p_value) { 
  if (p_value > 0.05) { 
    return(" ") 
  } else if (p_value <= 0.001) { 
    return("***") 
  } else if (p_value <= 0.01) { 
    return("**") 
  } else if (p_value <= 0.05) { 
    return("*") 
  } 
} 


# Wow, it's like every script starts out the exact same and then halfway through, takes a crazy, 
# wild turn for the worse! 
find_subjects() 
mess_with_the_columns() 

measure_vec <- c("PP", "HR") 
cond <- "IH" 
for (col in measure_vec) { 
  lst <- test_time(cond, col, FALSE) 
} 
cond <- "IL" 
for (col in measure_vec) { 
  lst <- test_time(cond, col, FALSE) 
} 
cond <- "BH" 
for (col in measure_vec) { 
  lst <- test_time(cond, col, FALSE) 
} 
cond <- "BL" 
for (col in measure_vec) { 
  lst <- test_time(cond, col, FALSE) 
} 

write.table(final_testing_df, file = "@Datasets/testing_split_dual_task_df.csv", row.names=F, sep = ',') 
