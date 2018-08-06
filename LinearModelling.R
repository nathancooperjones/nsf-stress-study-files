
library(tidyverse) 
library(knitr) 
library(grid) 
library(gridExtra) 
library(kableExtra) 
library(nlme) 


# READING IN CSV FILES 
testing_df <- read_csv("~/Desktop/testing_df.csv", col_types = cols()) 
testing_df <- testing_df %>% 
  mutate(Condition = factor(Condition, levels = c("BL", "IL", "BH", "IH"))) 
colnames(testing_df)[which(names(testing_df) == "Condition")] <- "Group" 

age_df <- read_csv("~/Desktop/Stress_Project/20180730132710-SurveyExport.csv", col_types = cols()) 
age_df <- age_df[, c(22, 23, 24, 25, 27, 29, 30, 19)] 
names(age_df) <- c("Subject", "Age", "Gender", "Nationality", "NativeLanguage", "EducationLevel", "Occupation", "City") 

# MERGING CSV FILES 
diff_df <- testing_df[ , c(1:3, 9:18)] 
names <- names(diff_df)[!(names(diff_df) %in% c("WB.RB", "SC.RB", "SC.WB", "DT.RB", "DT.WB", "DT.SC", "P.RB", "P.WB", "P.SC", "P.DT"))] 
diff_df <- diff_df %>% 
  gather(Session, PP, -names) %>% 
  mutate(Session = factor(Session, levels = c("WB.RB", "SC.RB", "SC.WB", "DT.RB", "DT.WB", "DT.SC", "P.RB", "P.WB", "P.SC", "P.DT"))) %>% 
  filter(Session %in% c("WB.RB", "SC.RB", "DT.RB", "P.RB")) %>% 
  filter(Measurement == "PP") 
levels(diff_df$Session) <- c("WB", "SC", "SC.WB", "DT", "DT.WB", "DT.SC", "P", "P.WB", "P.SC", "P.DT") 
diff_df <- merge(diff_df, age_df, by = "Subject") 

# CREATING LINEAR MODELS 
fit3_diff <- lme(PP ~ 1 + Group + Session, data = diff_df, random = ~1|Subject, na.action = na.omit) 

# SUMMARIZING LINEAR MODELS 
summary(fit3_diff) 

# coefficients(fit) # model coefficients 
# confint(fit, level=0.95) # CIs for model parameters 
# fitted(fit) # predicted values 
# residuals(fit) # residuals 
# anova(fit) # anova table 
# vcov(fit) # covariance matrix for model parameters 
# influence(fit) # regression diagnostics 

## JUNK: 

# READING IN CSV FILES 
raw_df <- read_csv("~/Desktop/full_df.csv", col_types = cols()) 
raw_df <- raw_df %>% 
  mutate(Condition = factor(Condition, levels = c("BL", "IL", "BH", "IH"))) %>% 
  mutate(Session = factor(Session, levels = c("RestingBaseline", "BaselineWriting", "StressCondition", "DualTask", "Presentation"))) 
colnames(raw_df)[which(names(raw_df) == "Condition")] <- "Group" 

testing_df <- read_csv("~/Desktop/testing_df.csv", col_types = cols()) 
testing_df <- testing_df %>% 
  mutate(Condition = factor(Condition, levels = c("BL", "IL", "BH", "IH"))) 
colnames(testing_df)[which(names(testing_df) == "Condition")] <- "Group" 

age_df <- read_csv("~/Desktop/20180730132710-SurveyExport.csv", col_types = cols()) 
age_df <- age_df[, c(22, 23, 24, 25, 27, 29, 30, 19)] 
names(age_df) <- c("Subject", "Age", "Gender", "Nationality", "NativeLanguage", "EducationLevel", "Occupation", "City") 

# MERGING CSV FILES 
raw_df <- raw_df[ , c("Subject", "Group", "Session", "PP")] %>% 
  mutate(PP = log(PP)) 
raw_df <- merge(raw_df, age_df, by = "Subject") 

session_df <- testing_df[ , c(1:8)] 
names <- names(session_df)[!(names(session_df) %in% c("RestingBaseline", "BaselineWriting", "StressCondition", "DualTask", "Presentation"))] 
session_df <- session_df %>% 
  gather(Session, PP, -names) %>% 
  mutate(Session = factor(Session, levels = c("RestingBaseline", "BaselineWriting", "StressCondition", "DualTask", "Presentation"))) %>% 
  filter(Measurement == "PP") 
session_df <- merge(session_df, age_df, by = "Subject") 

diff_df <- testing_df[ , c(1:3, 9:18)] 
names <- names(diff_df)[!(names(diff_df) %in% c("SC.RB", "WB.RB", "SC.WB", "DT.RB", "DT.WB", "DT.SC", "P.RB", "P.WB", "P.SC", "P.DT"))] 
diff_df <- diff_df %>% 
  gather(Session, PP, -names) %>% 
  mutate(Session = factor(Session, levels = c("SC.RB", "WB.RB", "SC.WB", "DT.RB", "DT.WB", "DT.SC", "P.RB", "P.WB", "P.SC", "P.DT"))) %>% 
  filter(Session %in% c("SC.RB", "WB.RB", "DT.RB", "P.RB")) %>% 
  filter(Measurement == "PP") 
diff_df$Session[diff_df$Session == "SC.RB"] <- "SC" 
diff_df$Session[diff_df$Session == "WB.RB"] <- "WB" 
diff_df$Session[diff_df$Session == "DT.RB"] <- "DT" 
diff_df$Session[diff_df$Session == "P.RB"] <- "P" 
diff_df <- merge(diff_df, age_df, by = "Subject") 

# CREATING LINEAR MODELS 
# fit1_raw <- lme(PP ~ 1 + Group + Session, data = raw_df, random = ~1|Subject, na.action = na.omit) 
fit1_session <- lme(PP ~ 1 + Group * Session, data = session_df, random = ~1|Subject, na.action = na.omit) 
fit1_diff <- lme(PP ~ 1 + Group + Session, data = diff_df, random = ~1|Subject, na.action = na.omit) 
fit2_diff <- lm(PP ~ Group, data = diff_df, na.action = na.omit) 
fit3_diff <- lm(PP ~ Session, data = diff_df, na.action = na.omit)
fit3_diff <- lme(PP ~ 1 + Group*Session, data = diff_df, random = ~1|Subject, na.action = na.omit) 

# fit2 <- lme(PP ~ 1 + Group + Session + NativeLanguage, data = full_df, random = ~1|Subject, na.action = na.omit) 
# fit3 <- lme(PP ~ 1 + Group + Session + Age, data = full_df, na.action = na.omit) 
# fit4 <- lm(PP ~ 1 + Group + Session + Group*Session + Age + NativeLanguage + factor(City), data = full_df, na.action = na.omit) 

# SUMMARIZING LINEAR MODELS 
# summary(fit1_raw) 
summary(fit1_session) 
summary(fit1_diff) 
summary(fit2_diff) 
summary(fit3_diff) 