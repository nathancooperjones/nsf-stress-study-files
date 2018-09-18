




current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

all_data_df <- read.csv("@Datasets/data-uci.csv")
str(all_data_df, 2)

print(all_data_df$big5_n)
hist(all_data_df$big5_n)
