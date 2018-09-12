# CPL scripts for the NSF-funded Stress Study 

Welcome to this Github repository, hosted by yours truly! There are a lot of files here, and I'm sure you are eager to get started! Well, hold your horses and let me tell you the intended order to at least get started: 

1) The first thing you will have to run is `SplitSessions.R` to find all the subjects and create `*_merged.csv` files. We will use this in step #2. 
2) Now we use that in `All-Subjects.Rmd` to generate a full dataframe that contains every single subjects' filtered measurements entitled `full_df.csv`. It is a great dataframe - in fact, it is so great that nearly every other script is based off of it. Neat, right! 
3) More dataframes?! Yes! Now run `Hypothesis-Testing-Full-Sensor-Set.Rmd` to generate a dataframe that contains mean, normalized differences for all subjects' measurements. You'll need this for a few other scripts, most notably `Advanced-Analysis.R`. 
4) Go nuts and run everything! Have fun! 

If you have any questions about how any of the scripts work, please email me at njones10@hawk.iit.edu and I will get back to you as soon as I can. Have fun and good analysis! 
