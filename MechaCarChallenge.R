#DELIVERABLE 1

# Use the library() function to load the dplyr package
library(dplyr)
mecha_table <- read.csv(file= 'MechaCar_mpg.csv',check.names = F ,stringsAsFactors = F) # import and read csv file as a dataframe

# Perform linear regression using the lm() function

lm (mpg~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance +AWD,data=mecha_table)

# Using the summary() function, determine the p-value and the r-squared value for the linear regression model.

summary(lm (mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance +AWD,data=mecha_table)) #generate summary statistics


# Deliverable 2

# Import and read in the Suspension_Coil.csv as a table
suspension_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# Create a total_summary data frame using the summarize() function to get the mean, median, variance, and standard deviation of the suspension coil's PSI column
total_summary <- suspension_table %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

# Create a lot_summary data frame using the group_by() and the summarize() functions to group each manufacturing lot.
lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI),Median = median(PSI), Variance = var(PSI), SD = sd(PSI) , .groups = 'keep')


# Deliverable 3

#Use the t.test() function to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch
t.test(suspension_table$PSI,mu=1500)

# Use t.test() function and its subset() argument to determine if the PSI for each manufacturing lot is statistically different from the population mean of 1,500 pounds per square inch
t.test(subset(suspension_table,Manufacturing_Lot=="Lot1")$PSI,mu=1500)
t.test(subset(suspension_table,Manufacturing_Lot=="Lot2")$PSI,mu=1500)
t.test(subset(suspension_table,Manufacturing_Lot=="Lot3")$PSI,mu=1500)