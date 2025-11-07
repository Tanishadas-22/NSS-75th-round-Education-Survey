install.packages("ggplot2")
install.packages("readxl")
install.packages("openxlsx")
install.packages("dplyr")
library(readxl)
library(ggplot2)
library(openxlsx)
library(dplyr)
data<- read_excel("C:/Users/SOSU/Downloads/NSS68_data excel.xlsx")
show(data)

1)selected_col<- data$Sex
plot_data<- data.frame(value = selected_col)
print(plot_data)
counts <- table(selected_col)
print(counts)
barplot(height = counts, xlab="Gender", ylab="Count", main="Gender Distribution", ylim=c(0,max(counts)+10), beside = TRUE, col=c("orange3","olivedrab4"))

2)selected_col<- data$Education
plot_data<- data.frame(value = selected_col)
print(plot_data)
counts <- table(selected_col)
print(counts)
barplot(height = counts, xlab="Education", ylab="Count", main="Education", ylim=c(0,max(counts)+10), beside = TRUE, col=c("orange3","olivedrab4", "blue", "pink","green","pink4","yellow1","red","pink3","orange1"))

3)selected_col<- data$Age
plot_data<- data.frame(value = selected_col)
print(plot_data)
counts <- table(selected_col)
print(counts)

3.1)grouped_data <- data %>%
  group_by(Age) %>%
  summarise(Average_Age = mean(Age, na.rm = TRUE),
            Count = n())
print(grouped_data)

3.2)plot_data$Group <- cut(plot_data$value, breaks = 4, labels = FALSE)
    print(plot_data)

barplot(height = plot_data , xlab="Age", ylab="Count", main="Age Distribution", ylim=c(0,max(counts)+10), beside = TRUE, col=c("orange3","olivedrab4", "blue", "pink"))




plot(x = "", y,main = "Scatter Plot", xlab = "X Values", Ylab = "Y Values", pch = 16, col = "blue" )
ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Count"
  )
?barplot


#Proportion (per 1000) of households having access to use internet facility, for each quintile class of UMPCE for each State/UT
#Importing block 3 data for calculation of UMPCE
library(readxl)
block_3<-read_excel("C:/Users/SOSU/Downloads/Block 3 in Excel file.xlsx",sheet="Block-3-Level-02 Household Char")
hh_consump<-as.array(block_3$hh_cons_exp)
print(hh_consump)
hh_size<-as.array(block_3$hhsize)
print(hh_size)
umpce<-hh_consump/hh_size
print(umpce)
                                              # UMPCE

rm(list=ls())

#--- NSSO-71st round Education Survey --
#-- UMPCE --

setwd("C:/Users/SOSU/Downloads")

# Read the data
block3data <- read_excel("BLOCK 3.xlsx")
View(block3data)


nrow(block3data)
ncol(block3data)
names(block3data)

table(block3data$Sector)

umpce = block3data$HH_Con_exp_rs / block3data$Household_size
summary(umpce)

est_pop_per_sample = block3data$Household_size * block3data$MULT_Combined

summary(block3data$HH_Con_exp_rs)
summary(block3data$Household_size)

block3data = data.frame(cbind(block3data, umpce, est_pop_per_sample)) 
View(block3data)

block3data_rural = subset(block3data,State ==17 & Sector == 1)
block3data_urban = subset(block3data,State ==17 & Sector == 2)

table(block3data_rural$Sector)
table(block3data_urban$Sector)

#----- Finding quintiles for a general data file, say, mydata ----

#mydata = data.frame(block3data_rural)
#View(mydata)

mydata = data.frame(block3data_urban)
View(mydata)

mydata_sorted = data.frame(mydata[order(mydata$umpce), ])
View(mydata_sorted)

cum_est_pop = cumsum(mydata_sorted$est_pop_per_sample)
tot_pop = sum(mydata_sorted$Household_size * mydata_sorted$MULT_Combined)
tot_pop

percent_cum_est_pop = cum_est_pop/tot_pop
quint = c()
quint
min(mydata_sorted$umpce)

pseq = seq(0.2,1.0,by=0.2)
pseq
plength = 5
for(i in c(1:plength)) {
  ser = (1:length(cum_est_pop))[ percent_cum_est_pop <= pseq[i]]
  quint[i] = mydata_sorted$umpce[max(ser)]
}
print(quint)


                                              #Literacy Rate

# 75th round Estimated Literacy Rate (in %) of persons 3-35 yrs for all states/UT  
library(readxl)
library(dplyr)

# Read the data from a CSV file (adjust the path as necessary)
data <- read_excel("C:/Users/TANISHA/Documents/Block 4 in nss 75th survey for finding literacy rate.xlsx", sheet= "Uban,Male, all state, 7 and abo")# Assuming 'data' is your data frame and 'State' is the column for different states
# Create indicators for literate persons and total
data <- data %>%
  mutate(
    x_i = ifelse(Edu_level_general > 1, 1, 0),  # Indicator for literate persons
    y_i = ifelse(Edu_level_general >= 1, 1, 0)  # Indicator for total
  )

# Create multiplier array (assuming MULT_Combined is in the data)
data$mult_i <- data$MULT_Combined

# Group by state and calculate X_hat, Y_hat, and literacy rate
results <- data %>%
  group_by(State) %>%
  summarise(
    X_i = sum(x_i * mult_i, na.rm = TRUE),  # Sum of literate persons weighted by multiplier
    Y_i = sum(y_i * mult_i, na.rm = TRUE),  # Sum of total persons weighted by multiplier
    literacy_rate = (X_i / Y_i) * 100        # Calculate literacy rate
  )

# Print the results
print(results)
#Saving the R_hat_ij data in an excel file
wb <- createWorkbook()
addWorksheet(wb,"Urban,Male")
writeData(wb, "Urban,Male", results)
saveWorkbook(wb, "L_R1 new.xlsx", overwrite = TRUE)


# Literacy Rate bar plot for different sectors 
# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Create the data frame
data <- data.frame(
  State = c("Jammu & Kashmir", "Himachal Pradesh", "Punjub", "Chandigarh", "Uttanchal","Haryana", "Delhi", "Rajasthan", "Uttar Pradesh", "Bihar", "Sikkim", "Arunachal Pradesh", "Nagaland", "Manipur", "Mizoram", "Tripura", "Meghalaya", "Assam", "West Bengal", "Jharkhand", "Odisha", "Chhattisgarh", "Madhya Pradesh", "Gujrat", "Daman & Diu", "D & N Haveli", "Maharashtra", "Andhra Pradesh", "Karnataka", "Goa", "Lakshadweep", "Kerala", "Tamil Nadu","Puducherry", "A & N Islands", "Telengana"),
  Rural_Person = c(75.82430461
                   ,85.55886327
                   ,80.01668352
                   ,85.94383824
                   ,86.07908336
                   ,77.00722794
                   ,70.83457631
                   ,65.49251436
                   ,70.80285332
                   ,69.51708872
                   ,87.98806571
                   ,71.99522674
                   ,83.72717421
                   ,87.24499919
                   ,97.64311538
                   ,87.58464127
                   ,89.3850762
                   ,84.91890255
                   ,77.36499474
                   ,71.35091179
                   ,74.85613696
                   ,75.02956074
                   ,69.84399906
                   ,77.03142311
                   ,85.00166773
                   ,68.12126309
                   ,79.44216938
                   ,60.37259753
                   ,70.95874285
                   ,94.06280138
                   ,97.4094446
                   ,95.36777253
                   ,77.51491803
                   ,89.60674724
                   ,85.72070611
                   ,62.07169167
  ),
  Urban_Person = c(82.57416885
                   ,95.46172133
                   ,90.50012458
                   ,92.99249879
                   ,92.04525336
                   ,87.30981143
                   ,89.39336817
                   ,83.47623531
                   ,81.17121159
                   ,83.07781029
                   ,95.14413732
                   ,82.8043993
                   ,95.38761046
                   ,92.11344921
                   ,98.96213721
                   ,94.52028797
                   ,96.74881718
                   ,93.83192634
                   ,88.14533914
                   ,86.06939063
                   ,90.15560734
                   ,87.16548259
                   ,85.77968698
                   ,91.05294368
                   ,99.02189423
                   ,92.21876362
                   ,91.67079806
                   ,79.60327813
                   ,88.32627386
                   ,95.12181975
                   ,97.52543766
                   ,97.30934635
                   ,89.02944309
                   ,92.51285036
                   ,91.59346542
                   ,85.46913968
  ))
# Reshape the data for ggplot
data_long <- data %>%
  pivot_longer(cols = c(Rural_Person, Urban_Person), 
               names_to = "Area", 
               values_to = "Literacy_Rate")

# Create the plot
ggplot(data_long, aes(x = State, y = Literacy_Rate, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Literacy Rate (Age 7 and Above) by State",
       x = "State",
       y = "Literacy Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Rural_Person" = "orange", "Urban_Person" = "cyan4"), 
                    labels = c("Rural", "Urban")) +
  guides(fill = guide_legend(title = "Area"))




#71st Round Survey:Literacy rate 

library(readxl)
data<- read_excel("C:/Users/SOSU/Documents/71st survey Block-4 - Level -03 Demographic and other particulars of Household members.xlsx", sheet = "Urban+Rural,Male+Female")
show(data)
?dplyr
# Load necessary library
library(dplyr)
library(openxlsx)

# Assuming 'data' is your data frame and 'State' is the column for different states
# Create indicators for literate persons and total
data <- data %>%
  mutate(
    x_i = ifelse(gen_edu > 1, 1, 0),  # Indicator for literate persons
    y_i = ifelse(gen_edu >= 1, 1, 0)  # Indicator for total
  )

# Create multiplier array (assuming MULT_Combined is in the data)
data$mult_i <- data$wgt_combined

# Group by state and calculate X_hat, Y_hat, and literacy rate
results <- data %>%
  group_by(state_cd) %>%
  summarise(
    X_i = sum(x_i * mult_i, na.rm = TRUE),  # Sum of literate persons weighted by multiplier
    Y_i = sum(y_i * mult_i, na.rm = TRUE),  # Sum of total persons weighted by multiplier
    literacy_rate = (X_i / Y_i) * 100        # Calculate literacy rate
  )

# Print the results
print(results)
#Saving the Literacy rates data in an excel file
wb <- createWorkbook()
addWorksheet(wb,"Urban+Rural,Male+Female")
writeData(wb, "Urban+Rural,Male+Female", results)
saveWorkbook(wb,"C:/Users/SOSU/Documents/L_RATE9.xlsx", overwrite = TRUE)

                                          
                                                     #UMPCE

rm(list=ls())

#--- NSSO-71st round Education Survey --
#-- UMPCE --

setwd("C:/Users/SOSU/Downloads")

# Read the data
block3data <- read_excel("BLOCK 3.xlsx")
View(block3data)


nrow(block3data)
ncol(block3data)
names(block3data)

table(block3data$Sector)

umpce = block3data$HH_Con_exp_rs / block3data$Household_size
summary(umpce)

est_pop_per_sample = block3data$Household_size * block3data$MULT_Combined

summary(block3data$HH_Con_exp_rs)
summary(block3data$Household_size)

block3data = data.frame(cbind(block3data, umpce, est_pop_per_sample)) 
View(block3data)

block3data_rural = subset(block3data,State ==17 & Sector == 1)
block3data_urban = subset(block3data,State ==17 & Sector == 2)

table(block3data_rural$Sector)
table(block3data_urban$Sector)

#----- Finding quintiles for a general data file, say, mydata ----

#mydata = data.frame(block3data_rural)
#View(mydata)

mydata = data.frame(block3data_urban)
View(mydata)

mydata_sorted = data.frame(mydata[order(mydata$umpce), ])
View(mydata_sorted)

cum_est_pop = cumsum(mydata_sorted$est_pop_per_sample)
tot_pop = sum(mydata_sorted$Household_size * mydata_sorted$MULT_Combined)
tot_pop

percent_cum_est_pop = cum_est_pop/tot_pop
quint = c()
quint
min(mydata_sorted$umpce)

pseq = seq(0.2,1.0,by=0.2)
pseq
plength = 5
for(i in c(1:plength)) {
  ser = (1:length(cum_est_pop))[ percent_cum_est_pop <= pseq[i]]
  quint[i] = mydata_sorted$umpce[max(ser)]
}

print(quint)

#creating a vector calculate the quantiles for urban sector of 
urban<-read_excel("C:/Users/SOSU/Downloads/NSS 75th Round data Excel/block 3 in 75th.xlsx", sheet="Urban")
urban_multiplier<-as.array(urban$MULT_Combined)
print(urban_multiplier)
urban_umpce<-as.array(urban$UMPCE)
print(urban_umpce)
i=(length(urban_multiplier))
print(i)
quintiles_urban<-c(178,1800,2500,3333,4750,80200)
print(quintiles_urban)


#assigning the quintiles classes depending on the umpce values(Urban)
quintile_class_u<-matrix(nrow=i, ncol=1)
a<-1
for (b in 1:i){
  quintile_class<- ifelse(urban_umpce[a] <=quintiles_urban[2], 1,
                          ifelse(urban_umpce[a] <=quintiles_urban[3], 2,
                                 ifelse(urban_umpce[a] <=quintiles_urban[4], 3,
                                        ifelse(urban_umpce[a] <=quintiles_urban[5], 4,
                                               ifelse(urban_umpce[a] >quintiles_urban[5], 5)))))
  print(urban_umpce[a])
  print(quintile_class)
  quintile_class_u[a,1]<-quintile_class
  a<-a+1
}
print(quintile_class_u)
library(dplyr)
merged<-data.frame(Column1=urban_umpce,Column2=quintile_class_u)
print(merged)
summary(merged)

#exporting the merged data in an excel file
library(writexl)
write_xlsx(merged,"C:/Users/SOSU/Downloads/QC of india urban.xlsx")

#creating a vector calculate the quantiles for rural sector of India
rural<-read_excel("C:/Users/SOSU/Downloads/NSS 75th Round data Excel/block 3 in 75th.xlsx", sheet="Rural")
rural_multiplier<-as.array(rural$MULT_Combined)
print(rural_multiplier)
rural_umpce<-as.array(rural$UMPCE)
print(rural_umpce)
i=(length(rural_multiplier))
print(i)
quintiles_rural<-c(83.3,1000,1333,1649,2100,45000)
print(quintiles_rural)

#assigning the quintiles classes depending on the umpce values(Rural)
quintile_class_r<-matrix(nrow=i, ncol=1)
a<-1
for (b in 1:i){
  quintile_class<- ifelse(rural_umpce[a] <=quintiles_rural[2], 1,
                          ifelse(rural_umpce[a] <=quintiles_rural[3], 2,
                                 ifelse(rural_umpce[a] <=quintiles_rural[4], 3,
                                        ifelse(rural_umpce[a] <=quintiles_rural[5], 4,
                                               ifelse(rural_umpce[a] >quintiles_rural[5], 5)))))
  print(rural_umpce[a])
  print(quintile_class)
  quintile_class_r[a,1]<-quintile_class
  a<-a+1
}
print(quintile_class_r)
library(dplyr)
merged<-data.frame(Column1=rural_umpce,Column2=quintile_class_r)
print(merged)
summary(merged)

#exporting the merged data in an excel file
library(writexl)
write_xlsx(merged,"C:/Users/SOSU/Downloads/QC of india rural.xlsx")



                                                 #TABLE

#Parcentage(per 100) of persons (age 3-35 years) dropping out/discontinuance and never-enrolled for North eastern State 
library(dplyr)
library(readxl)

# Import the Excel file
data <- read_excel("C:/Users/SOSU/Downloads/75th Block 4 & 7 merged.xlsx", sheet = "Rural,Male")

# Check if the required columns have missing values
summary(data$Major_reason_not_enrolled)
summary(data$Whether_ever_attended)
summary(data$MULT_Combined.y)

# Filter the data for State == 11
data <- data %>%
  filter(State.x == 11) %>%
  mutate(
    x_i = ifelse(Whether_ever_attended >= 1, 1, 0),   # Updated logic for x_i
    y_i = ifelse(Whether_ever_attended > 1, 1, 0)    # Updated logic for y_i
  )

# Check if there is data after filtering and mutating
summary(data)

# Create multiplier array
data$mult_i <- data$MULT_Combined.y

# Check if any rows are missing in the multiplier column
summary(data$mult_i)

# Initialize an empty data frame to store results
results <- data.frame(Major_reason_not_enrolled = character(0), X_hat = numeric(0), Y_hat = numeric(0), R_hat = numeric(0))

# Assuming 'data' is your dataframe and 'Major_reason_not_enrolled' is the column containing reasons
reasons <- unique(data$Major_reason_not_enrolled)

# Initialize an empty dataframe to store the results
results <- data.frame(Major_reason_not_enrolled = character(),
                      X_hat = numeric(),
                      Y_hat = numeric(),
                      R_hat = numeric(),
                      stringsAsFactors = FALSE)

# For loop to calculate X_hat, Y_hat, and R_hat for each reason
for (z in 1:length(reasons)) {
  
  # Extract the z-th unique reason
  reason <- reasons[z]
  
  # Filter data for the current reason
  data_reason <- data %>% filter(Major_reason_not_enrolled == reason)
  
  # Check if there is any data for the current reason
  if (nrow(data_reason) > 0) {
    # Calculate X_hat and Y_hat for the current reason
    X_hat <- sum(data_reason$x_i * data_reason$mult_i, na.rm = TRUE)  # Weighted sum of individuals who have ever attended school
    Y_hat <- sum(data_reason$y_i * data_reason$mult_i, na.rm = TRUE)  # Weighted sum of individuals who have not enrolled for "other" reasons
    
    # Calculate R_hat (dropout rate per 1000)
    R_hat <- ifelse(X_hat > 0, (Y_hat / X_hat) * 1000, 0)  # Avoid division by zero
    
    # Append the results to the results data frame
    results <- rbind(results, data.frame(Major_reason_not_enrolled = reason, X_hat = X_hat, Y_hat = Y_hat, R_hat = R_hat))
  }
}

# Calculate the total of R_hat
total_R_hat <- sum(results$R_hat)

# Normalize R_hat to sum to 1000
if (total_R_hat > 0) {
  results$R_hat <- (results$R_hat / total_R_hat) *100
}

# Print the results
print(results)

#Saving the data in an excel file
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb,"S,ALL")
writeData(wb, "S,ALL", results )
saveWorkbook(wb, "C:/Users/SOSU/Downloads/S,ALL.xlsx", overwrite = TRUE)

# Check the sum of R_hat
cat("Sum of R_hat:", sum(results$R_hat), "\n")



#Parcentage(per 100) of persons (age 3-35 years) dropping out among ever enrolled persons for North eastern State 
rm(list=ls())
library(dplyr)
library(readxl)
library(writexl)
#Merging gender with block 7
# Import the Excel file
block7data <- read_excel("C:/Users/SOSU/Downloads/NSS 75th Round data Excel/block 7, 75th round.xlsx")
block4data <- read_excel("C:/Users/SOSU/Downloads/BLOCK 4 for 75th round original.xlsx")
newblockwithagetoreason = merge()

# Combine the column with the second file
newblockwithagetoreason <- merge(block4data, 
                             block7data[, c("HHID", "Per_serialno", "Age", "Whether_ever_attended", "School_enrolment_age", "Level_enrolment",	"Course_type",	"Completed_last_enrolled",	"Last_attended_12_grade"	,"Age_last_enrolld",	"Type_institution",	"Preparing_higher_last365days",	"Exp_preparation_amt",	"Major_reason_not_enrolled")], 
                             by = c("HHID", "Per_serialno"), 
                             all.x = TRUE)

# Write the merged data to a new Excel file
write_xlsx(newblockwithagetoreason, "merged_file.xlsx")


#Parcentage(per 100) of persons (age 3-35 years) dropping out among ever enrolled persons for North eastern State 
rm(list=ls())
library(readxl)
data7 <- read_excel("C:/Users/SOSU/Documents/75th block 4 gender with block7.xlsx")
data4 <- read_excel("C:/Users/SOSU/Downloads/NSS 75th Round data Excel/R75252L04 (Block-4)-Demographic and other particulars of household members.xlsx")
x_i = ifelse(data4$enrolmt_3_35_yrs_status > 1 & data4$State == 18 , 1, 0)  
y_i = ifelse(data7$Completed_last_enrolled == 2 & data7$State == 18 , 1, 0)                              
mult_i7 <- as.array(data7$MULT_Combined)
mult_i4 <- as.array(data4$MULT_Combined)
X_hat = sum(x_i * mult_i4, na.rm = TRUE)  
Y_hat = sum(y_i * mult_i7, na.rm = TRUE)
R_hat = (Y_hat / X_hat) * 100     
print(R_hat) 


                                                 #GAR

# Load necessary libraries
library(dplyr)
library(readxl)

# Sample data frame (replace this with your actual data)
data <- read_excel("C:/Users/SOSU/Downloads/NSS 75th Round data Excel/R75252L04 (Block-4)-Demographic and other particulars of household members.xlsx")

# Create indicators for persons attending Classes I-V  and population in the age-group 6-10 years 

data <- data %>%
  mutate(
    x_i = ifelse(Edu_level_general == 7, 1, 0),                  # Indicator for persons attending Classes I-V
    y_i = ifelse(Age>5 & Age<11,  1, 0)                          # Indicator for population in the age-group 6-10 years 
  )

# Create multiplier array (assuming MULT_Combined is in the data)
data$mult_i <- data$MULT_Combined

# Group by state and calculate X_hat, Y_hat, and literacy rate
results <- data %>%
  
  summarise(
    X_hat = sum(x_i * mult_i, na.rm = TRUE),  
    Y_hat = sum(y_i * mult_i, na.rm = TRUE), 
    R_hat = (X_hat / Y_hat) * 100        
  )

# Print the results
print(results)


# Load necessary libraries
library(dplyr)
library(readxl)

# Load the dataset
data <- read_excel("C:/Users/SOSU/Downloads/NSS 75th Round data Excel/R75252L04 (Block-4)-Demographic and other particulars of household members.xlsx", sheet ="Urban,Male")

# Create indicators for different levels of education
data <- data %>%
  mutate(
    x_i = ifelse(Edu_level_general == 7, 1, 0),
    y_i = ifelse(Edu_level_general == 7 & Age >= 6 & Age <= 10, 1, 0)
  )

# Create multiplier array (assuming MULT_Combined is in the data)
data$mult_i <- data$MULT_Combined

# Calculate GAR for each level of education by gender
gar_results <- data %>%
  summarise(
    X_hat = sum(x_i * mult_i, na.rm = TRUE),  
    Y_hat = sum(y_i * mult_i, na.rm = TRUE), 
    R_hat = (X_hat / Y_hat) * 100                            
    ) %>%
  ungroup()
  
# Print the GAR results
print(gar_results)

rm(list=ls())
library(readxl)
data <- read_excel("C:/Users/SOSU/Downloads/NSS 75th Round data Excel/R75252L04 (Block-4)-Demographic and other particulars of household members.xlsx", sheet ="Rural,Male")
x_i <- ifelse(data$Edu_level_general == 7 & data$Age >= 6 & data$Age <= 10,  1, 0)
y_i <- ifelse(data$Edu_level_general == 7 & data$enrolmt_3_35_yrs_status == 4 , 1, 0)
print(x_i)
print(y_i)
mult_i<-as.array(data$MULT_Combined)
print(mult_i)
length(x_i)
length(mult_i)
length(y_i)
X_hat <- sum(x_i * mult_i, na.rm = TRUE)
print(X_hat)
Y_hat <-  sum(y_i * mult_i, na.rm = TRUE)
print(Y_hat)
R_hat = (Y_hat / X_hat) *100
print(R_hat)

                                             #Computer and Internet

#Percentage of households with computer and internet facility for different States
rm(list=ls())
library(readxl)
library(dplyr)
data <- read_excel("C:/Users/SOSU/Downloads/NSS 75th Round data Excel/block 3 in 75th.xlsx", sheet ="R75252L02 (Block 3)-household c")
data <- data %>%
  filter(State == 17) %>%
  mutate(
    x_i = ifelse(Member_internet >=1 ,1,0),                                                             
    y_i = ifelse(Member_internet ==1, 1, 0)                            
  )
mult_i <- as.array(data$MULT_Combined)
results <- data %>%
  summarise(
    X_hat = sum(x_i * mult_i, na.rm = TRUE),  
    Y_hat = sum(y_i * mult_i, na.rm = TRUE), 
    R_hat = (Y_hat / X_hat) * 100     
  ) 
print(results)

                                               
#Percentage of persons of age 5 years and above with ability to operate computer for different States
rm(list=ls())
library(readxl)
library(dplyr)
data <- read_excel("C:/Users/SOSU/Downloads/NSS 75th Round data Excel/R75252L04 (Block-4)-Demographic and other particulars of household members.xlsx", sheet ="R75252L04 (Block-4)-Demographic")
data <- data %>%
  filter(State == 17) %>%
  mutate(
    x_i = ifelse(Operate_internet_age_5yrs >=1 ,1,0),                                                             
    y_i = ifelse(Operate_internet_age_5yrs ==1, 1, 0)                            
  )
mult_i <- as.array(data$MULT_Combined)
results <- data %>%
  summarise(
    X_hat = sum(x_i * mult_i, na.rm = TRUE),  
    Y_hat = sum(y_i * mult_i, na.rm = TRUE), 
    R_hat = (Y_hat / X_hat) * 100     
  ) 
print(results)

#creating a vector calculate the quantiles for rural sector of NE states
library(readxl)
rural<-read_excel("C:/Users/SOSU/Downloads/Block4_Block7_UMPCE.xlsx", sheet="Assam Rural")
rural_multiplier<-as.array(rural$MULT_Combined)
print(rural_multiplier)
rural_umpce<-as.array(rural$umpce)
print(rural_umpce)
i=(length(rural_multiplier))
print(i)
quintiles_rural<-c(140,	1000,	1250,	1467,	1743,	10625)
print(quintiles_rural)

#assigning the quintiles classes depending on the umpce values(Rural)
quintile_class_r<-matrix(nrow=i, ncol=1)
a<-1
for (b in 1:i){
  quintile_class<- ifelse(rural_umpce[a] <=quintiles_rural[2], 1,
                          ifelse(rural_umpce[a] <=quintiles_rural[3], 2,
                                 ifelse(rural_umpce[a] <=quintiles_rural[4], 3,
                                        ifelse(rural_umpce[a] <=quintiles_rural[5], 4,
                                               ifelse(rural_umpce[a] >quintiles_rural[5], 5)))))
  print(rural_umpce[a])
  print(quintile_class)
  quintile_class_r[a,1]<-quintile_class
  a<-a+1
}
print(quintile_class_r)
library(dplyr)
merged<-data.frame(Column1=rural_umpce,Column2=quintile_class_r)
print(merged)
summary(merged)

#exporting the merged data in an excel file
library(writexl)
write_xlsx(merged,"C:/Users/SOSU/Downloads/QC of Assam rural .xlsx")

#creating a vector calculate the quantiles for urban sector of Wb(Urban)
urban<-read_excel("C:/Users/SOSU/Downloads/Block4_Block7_UMPCE.xlsx", sheet="Assam Urban")
urban_multiplier<-as.array(urban$MULT_Combined)
print(urban_multiplier)
urban_umpce<-as.array(urban$umpce)
print(urban_umpce)
i=(length(urban_multiplier))
print(i)
quintiles_urban<-c(250,	1667,	2140,	3000,	5000, 15075)
print(quintiles_urban)


#assigning the quintiles classes depending on the umpce values(Urban)
quintile_class_u<-matrix(nrow=i, ncol=1)
a<-1
for (b in 1:i){
  quintile_class<- ifelse(urban_umpce[a] <=quintiles_urban[2], 1,
                          ifelse(urban_umpce[a] <=quintiles_urban[3], 2,
                                 ifelse(urban_umpce[a] <=quintiles_urban[4], 3,
                                        ifelse(urban_umpce[a] <=quintiles_urban[5], 4,
                                               ifelse(urban_umpce[a] >quintiles_urban[5], 5)))))
  print(urban_umpce[a])
  print(quintile_class)
  quintile_class_u[a,1]<-quintile_class
  a<-a+1
}
print(quintile_class_u)
library(dplyr)
merged<-data.frame(Column1=urban_umpce,Column2=quintile_class_u)
print(merged)
summary(merged)

#exporting the merged data in an excel file
library(writexl)
write_xlsx(merged,"C:/Users/SOSU/Downloads/QC Assam urban .xlsx")






#Percentage of households with computer for each quintile class of Usual Monthly per Capital Expenditure (UMPCE) for India
rm(list=ls())
library(readxl)
library(dplyr)
data <- read_excel("C:/Users/SOSU/Downloads/block 3 hh char 75th survey.xlsx", sheet="Sheet14")
data <- data %>%
  filter(`Quintile Class` == 5) %>%
  mutate(
    x_i = ifelse(Member_internet>=1 ,1,0),                                                             
    y_i = ifelse(Member_internet==1, 1, 0)                            
  )
mult_i <- as.array(data$MULT_Combined)
results <- data %>%
  summarise(
    X_hat = sum(x_i * mult_i, na.rm = TRUE),  
    Y_hat = sum(y_i * mult_i, na.rm = TRUE), 
    R_hat = (Y_hat / X_hat) * 100     
  )%>%
  ungroup()
print(results)

                     #...................Trial

rm(list=ls())
library(readxl)
data <- read_excel("C:/Users/SOSU/Downloads/MERGED FILE of age to reasons of bl 7 with bl 4.xlsx")
x_i <- ifelse(data$enrolmt_3_35_yrs_status >1 & data$State == 18,  1, 0)
y_i <- ifelse(data$Completed_last_enrolled ==2  & data$State == 18 , 1, 0)
mult_i<-as.array(data$MULT_Combined)
X_hat <- sum(x_i * mult_i, na.rm = TRUE)
Y_hat <-  sum(y_i * mult_i, na.rm = TRUE)
R_hat = (Y_hat / X_hat) *100
print(R_hat)


#...................... Merging UMPCE WITH MERGED FILE 
rm(list=ls())
library(dplyr)
library(readxl)
library(writexl)
# Import the Excel file
premergedata <- read_excel("C:/Users/SOSU/Downloads/MERGED FILE of age to reasons of bl 7 with bl 4.xlsx")
block3data <- read_excel("C:/Users/SOSU/Downloads/block 3 hh char 75th survey.xlsx")

# Combine the column with the second file
result <- merge(premergedata , 
                                 block3data[, c("HHID", "umpce")], 
                                 by = c("HHID"), 
                                 all.x = TRUE)

# Write the merged data to a new Excel file
write_xlsx(result, "merged_file.xlsx")


#....................... For dropout Logistic Regression
rm(list=ls())
data <- read_excel("C:/Users/SOSU/Downloads/Block4_Block7_UMPCE.xlsx")
y_i <- ifelse(data$Completed_last_enrolled ==2 , 1, 0)
#log_model <- glm(y_i~ sector+gender+Householdtype+umpce_quintile_factor, family = binomial(link=logit), data = survey_data_new)
log_model <- glm(y_i~ factor(Sector)+factor(Gender)+umpce, family = binomial(link=logit), data = data)
summary(log_model)
# Extract the coefficients
coefs <- coef(log_model)
print(coefs)
# Exponentiate the coefficients to get odds ratios
odds_ratios<- exp(coefs)
# Print the odds ratios
print(odds_ratios)
table(factor(data$Gender))


#logistic regressions for INDIA 75th 
rm(list=ls())
library(readxl)
data <- read_excel("C:/Users/SOSU/Downloads/Block4_Block7_UMPCE.xlsx", sheet = "Assam ever enrolled")
y_1 <- ifelse(data$Completed_last_enrolled ==2 , 1, 0)
print(y_1)
y_2<-ifelse(data$Completed_last_enrolled ==2 & data$Level_enrolment == 07 , 1,0 )
print(y_2)
y_3<-ifelse(data$Completed_last_enrolled ==2 & data$Level_enrolment == 08 , 1,0)
print(y_3)
y_4<-ifelse(data$Completed_last_enrolled ==2 & data$Level_enrolment == 10 , 1,0)
print(y_4)
y_5<-ifelse(data$Completed_last_enrolled ==2 & data$Level_enrolment == 11 , 1,0)
print(y_5)
y_6<-ifelse(data$Completed_last_enrolled ==2 & data$Level_enrolment == 15 , 1,0)
print(y_6)
y_7<-ifelse(data$Completed_last_enrolled ==2 & data$Level_enrolment == 16 , 1,0)
print(y_7)

y_8<-ifelse(data$Completed_last_enrolled ==2 & data$Level_enrolment == 12 , 1,0)
print(y_8)
y_9<-ifelse(data$Completed_last_enrolled ==2 & data$Level_enrolment == 13 , 1,0)
print(y_9)
y_10<-ifelse(data$Completed_last_enrolled ==2 & data$Level_enrolment == 14 , 1,0)
print(y_10)


Sector<- data$Sector#factor1
Gender_new <- data$`Gender new` #factor2, taking male and transgender together and coded 1
#Neverenrollmentreason <-data$Major_reason_not_enrolled   #not taking because there are 19 reasons
umpce_quintile_class <- data$`quintile class`#factor3
print(umpce_quintile_class)
umpce_quintile_factor <- factor(umpce_quintile_class, levels = 1:5)
print(umpce_quintile_factor)
Age <- data$Age.y
#institution<- data$Type_institution
#weights<-round(survey_data_new$wgt_combined.x)
#print(weights)

prop.table(table(y_1,factor(data$Type_institution) ))
chisq.test(table(y_3,factor(data$`Gender new`) ))
prop.table()

# Fitting the logistic regression model for y_1
log_model1 <- glm(y_1~ factor(Sector)+factor(Gender_new)+umpce_quintile_factor+Age, family = binomial(link=logit), data = data)
summary(log_model1)

# log_model1_trial <- glm(y_1~ factor(institution), family = binomial(link=logit), data = data)
# summary(log_model1_trial)
# exp(coef(log_model1_trial))
# Extracting the coefficients
coefs1 <- coef(log_model1)
print(coefs1)
# Exponentiating the coefficients to get odds ratios
odds_ratios1<- exp(coefs1)
# Printing the odds ratios
print(odds_ratios1)
#plot(log_model1)

# Fit the logistic regression model for y_2
log_model2 <- glm(y_2~ factor(Sector)+factor(Gender_new)+umpce_quintile_factor+Age, family = binomial(link=logit), data = data)
summary(log_model2)
# Extract the coefficients
coefs2 <- coef(log_model2)
print(coefs2)
# Exponentiate the coefficients to get odds ratios
odds_ratios2<- exp(coefs2)
# Print the odds ratios
print(odds_ratios2)
#plot(log_model2)

# Fit the logistic regression model for y_3
log_model3 <- glm(y_3~factor(Sector)+factor(Gender_new)+umpce_quintile_factor+Age, family = binomial(link=logit),  data = data)
summary(log_model3)
# Extract the coefficients
coefs3 <- coef(log_model3)
print(coefs3)
# Exponentiate the coefficients to get odds ratios
odds_ratios3<- exp(coefs3)
# Print the odds ratios
print(odds_ratios3)
#plot(log_model3)

# Fit the logistic regression model for y_4
log_model4 <- glm(y_4~ factor(Sector)+factor(Gender_new)+umpce_quintile_factor+Age, family = binomial(link=logit), data = data)
summary(log_model4)
# Extract the coefficients
coefs4 <- coef(log_model4)
print(coefs4)
# Exponentiate the coefficients to get odds ratios
odds_ratios4<- exp(coefs4)
# Print the odds ratios
print(odds_ratios4)
#plot(log_model4)

# Fit the logistic regression model for y_5
log_model5 <- glm(y_5~ factor(Sector)+factor(Gender_new)+umpce_quintile_factor+Age , family = binomial(link=logit), data = data)
summary(log_model5)
# Extract the coefficients
coefs5 <- coef(log_model5)
print(coefs5)
# Exponentiate the coefficients to get odds ratios
odds_ratios5<- exp(coefs5)
# Print the odds ratios
print(odds_ratios5)
#plot(log_model5)
#colours()

# Fit the logistic regression model for y_6
log_model6 <- glm(y_6~factor(Sector)+factor(Gender_new)+umpce_quintile_factor+Age , family = binomial(link=logit), data = data)
summary(log_model6)
# Extract the coefficients
coefs6 <- coef(log_model6)
print(coefs6)
# Exponentiate the coefficients to get odds ratios
odds_ratios6<- exp(coefs6)
# Print the odds ratios
print(odds_ratios6)
#plot(log_model6)
#colours()

# Fit the logistic regression model for y_7
log_model7 <- glm(y_7~factor(Sector)+factor(Gender_new)+umpce_quintile_factor+Age , family = binomial(link=logit), data = data)
summary(log_model7)
# Extract the coefficients
coefs7 <- coef(log_model7)
print(coefs7)
# Exponentiate the coefficients to get odds ratios
odds_ratios7<- exp(coefs7)
# Print the odds ratios
print(odds_ratios7)
#plot(log_model7)
#colours()

# Fit the logistic regression model for y_8
log_model8 <- glm(y_8~factor(Sector)+factor(Gender_new)+umpce_quintile_factor+Age , family = binomial(link=logit), data = data)
summary(log_model8)
# Extract the coefficients
coefs8 <- coef(log_model8)
print(coefs8)
# Exponentiate the coefficients to get odds ratios
odds_ratios8<- exp(coefs8)
# Print the odds ratios
print(odds_ratios8)
#plot(log_model8)
#colours()

# Fit the logistic regression model for y_9
log_model9 <- glm(y_9~factor(Sector)+factor(Gender_new)+umpce_quintile_factor+Age , family = binomial(link=logit), data = data)
summary(log_model9)
# Extract the coefficients
coefs9 <- coef(log_model9)
print(coefs9)
# Exponentiate the coefficients to get odds ratios
odds_ratios9<- exp(coefs9)
# Print the odds ratios
print(odds_ratios9)
#plot(log_model9)
#colours()

# Fit the logistic regression model for y_10
log_model10 <- glm(y_10~factor(Sector)+factor(Gender_new)+umpce_quintile_factor+Age , family = binomial(link=logit), data = data)
summary(log_model10)
# Extract the coefficients
coefs10 <- coef(log_model10)
print(coefs10)
# Exponentiate the coefficients to get odds ratios
odds_ratios10<- exp(coefs10)
# Print the odds ratios
print(odds_ratios10)
#plot(log_model10)
#colours()