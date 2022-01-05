#load dplyr package
install.packages("dplyr")
library(dplyr)

#Import and read MechaCar_mpg.csv as a dataframe
mechacar_mpg_df <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#Perform linear regression using lm
lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = mechacar_mpg_df)

#Determine p-value and r-squared value
summary(lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = mechacar_mpg_df))

#Import and read Suspension_Coil.csv as a table
suspension_coil_df <- read.csv(file='Suspension_Coil.csv', check.names = F,stringsAsFactors = F)

#Create summary dataframe
total_summary <- suspension_coil_df %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

#Create summaries for each lot
lot_summary <- suspension_coil_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep') 

#T-Tests on suspension coils
t.test(suspension_coil_df$PSI, mu=1500)

#T-Tests on each lot
t.test(subset(suspension_coil_df, Manufacturing_Lot=="Lot1")$PSI, mu =1500)
t.test(subset(suspension_coil_df, Manufacturing_Lot=="Lot2")$PSI, mu =1500)
t.test(subset(suspension_coil_df, Manufacturing_Lot=="Lot3")$PSI, mu =1500)
