#PFDA Assignment
#Name: Akish Ezek Sean
#TP No: TP068726

#THIS IS WHERE THE ASSIGNMENT BEGINS 
#---------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

#Installing Packages for additional functions
#Load the dplyr library for data manipulation

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("lubridate")  # Install the package
library(lubridate)  # Load the package


#Employee Data CSV is read and viewed as table
Emp_data = read.csv ("D:\\Productivity\\Uni\\PFDA\\Assignment\\employee_attrition.csv",header=TRUE)
View(Emp_data)

#setting personal header names for the dataset
names(Emp_data)=c("Emp_ID","Emp_RecordDate","Emp_Bday","Emp_HiredDate","Emp_TerminationDate","Emp_Age","Emp_ServiceLength","City_Name","Department_Name","Emp_JobTitle","Store_Name","Emp_GenderShort","Emp_GenderLong","Emp_TerminReason","Emp_TerminType","Emp_StatusYear","Emp_Status","Emp_BussinessU")

#Preprocessing exploration

# Display summary statistics for Emp_data
summary(Emp_data)

# Display column names for Emp_data
names(Emp_data)

# Display the number of rows in Emp_data
nrow(Emp_data)

# Display the number of columns in Emp_data
ncol(Emp_data)

# Display the structure of Emp_data including data type and variable names
str(Emp_data)

# Display the first 6 rows of Emp_data
head(Emp_data)

# Display the last 6 rows of Emp_data
tail(Emp_data)

# Display a random sample of 5 rows from Emp_data
sample_n(Emp_data,5)

# Display the number of missing values for each column in Emp_data
colSums(is.na(Emp_data))

#-------------------------------------------------------------------------#
#Questions


#Question 1: What is the demographic of this company and what are notable quirks of its employee base

#Analysis 1-1: Explore the primary age groups of the company: 

age_groups <- cut(Emp_data$Emp_Age, breaks = c(20, 30, 40, 50, 60, 70))
age_counts <- table(age_groups)
print(age_counts)

#Analysis 1-2: Explore gender distribution of the company:

library(dplyr)

gender_distribution <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), lubridate::year(Emp_TerminationDate) != 1900) %>%
  group_by(Emp_GenderLong) %>%
  summarise(Employee_Count = n()) %>%
  mutate(Percentage = Employee_Count / sum(Employee_Count) * 100) %>%
  mutate(Emp_GenderLong = case_when(
    Emp_GenderLong == "Female" ~ "Female employees",
    Emp_GenderLong == "Male" ~ "Male employees",
    TRUE ~ Emp_GenderLong
  ),
  Employee_Count = case_when(
    is.character(Employee_Count) ~ as.numeric(Employee_Count),
    TRUE ~ Employee_Count
  ),
  Percentage = case_when(
    is.character(Percentage) ~ as.numeric(Percentage),
    TRUE ~ Percentage
  ))

print(gender_distribution)


#Analysis 1-4: Overal Turnover rate of the company:

# Filter out records where termination date is not missing and status is terminated
terminated_employees <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), Emp_Status == "TERMINATED")

# Count the number of terminated employees
num_terminated <- nrow(terminated_employees)

# Count the number of active employees (status is not terminated)
num_active <- sum(Emp_data$Emp_Status != "Terminated", na.rm = TRUE)

# Calculate the turnover rate
turnover_rate <- (num_terminated / (num_active + num_terminated)) * 100

# Print the results
cat("Number of Terminated Employees:", num_terminated, "\n")
cat("Number of Active Employees:", num_active, "\n")
cat("Turnover Rate (%):", turnover_rate, "\n")


#Analysis 1-5: Find out how long on average do people serve in this company 

avg_service_length <- mean(Emp_data$Emp_ServiceLength, na.rm = TRUE)

cat("Average Service Length (in years):", avg_service_length, "\n")


#Analysis 1-6: Most popular cities, which city has the most employees

library(dplyr)

active_city_employee_counts <- Emp_data %>%
  filter(Emp_Status == "ACTIVE") %>%
  group_by(City_Name) %>%
  summarise(Employee_Count = n()) %>%
  arrange(desc(Employee_Count))

top_10_cities <- head(active_city_employee_counts, 10)

cat("Top 10 cities by employee count (active employees only):\n")
cat("\n")
print(top_10_cities)


#Analysis 1-6.5: Summary of 1-6

active_city_employee_counts <- Emp_data %>%
  filter(Emp_Status == "ACTIVE") %>%
  group_by(City_Name) %>%
  summarise(Employee_Count = n()) %>%
  arrange(desc(Employee_Count))

most_popular_city <- active_city_employee_counts$City_Name[1]
employee_count <- active_city_employee_counts$Employee_Count[1]

cat("The most popular city for active employees in the company is:", most_popular_city, "\n")
cat("Number of active employees in the most popular city:", employee_count, "\n")


#Analysis 1-7: Top 10 cities with most TERMINATED Employees

terminated_city_employee_counts <- Emp_data %>%
  filter(Emp_Status == "TERMINATED") %>%
  group_by(City_Name) %>%
  summarise(Employee_Count = n()) %>%
  arrange(desc(Employee_Count))

most_terminated_city <- terminated_city_employee_counts$City_Name[1]
terminated_employee_count <- terminated_city_employee_counts$Employee_Count[1]

cat("The city with the most terminated employees is:", most_terminated_city, "\n")
cat("Number of terminated employees in the most terminated city:", terminated_employee_count, "\n")

#Analysis 1-7.5: Summary of 1-7:

terminated_city_employee_counts <- Emp_data %>%
  filter(Emp_Status == "TERMINATED") %>%
  group_by(City_Name) %>%
  summarise(Employee_Count = n()) %>%
  arrange(desc(Employee_Count))

top_10_terminated_cities <- head(terminated_city_employee_counts, 10)

cat("Top 10 cities by terminated employee count:\n")
print(top_10_terminated_cities)


#Analysis 1-8: Most popular departments for ACTIVE Employees:

active_department_employee_counts <- Emp_data %>%
  filter(Emp_Status == "ACTIVE") %>%
  group_by(Department_Name) %>%
  summarise(Employee_Count = n()) %>%
  arrange(desc(Employee_Count))

top_10_departments <- head(active_department_employee_counts, 10)

cat("Top 10 departments by employee count (active employees only):\n")
cat("\n")
print(top_10_departments)


#Analysis 1-9: Most popular departments for TERMINATED Employees:

terminated_department_employee_counts <- Emp_data %>%
  filter(Emp_Status == "TERMINATED") %>%
  group_by(Department_Name) %>%
  summarise(Employee_Count = n()) %>%
  arrange(desc(Employee_Count))

top_10_terminated_departments <- head(terminated_department_employee_counts, 10)

cat("Top 10 departments by employee count (terminated employees only):\n")
cat("\n")
print(top_10_terminated_departments)


#Analysis 1-10: Most popular job titles for ACTIVE Employees:

active_job_title_counts <- Emp_data %>%
  filter(Emp_Status == "ACTIVE") %>%
  group_by(Emp_JobTitle) %>%
  summarise(Employee_Count = n()) %>%
  arrange(desc(Employee_Count))

top_10_active_job_titles <- head(active_job_title_counts, 10)

cat("Top 10 job titles by employee count (active employees only):\n")
cat("\n")
print(top_10_active_job_titles)


#Analysis 1-11: Most popular job titles for TERMINATED Employees:

terminated_job_title_counts <- Emp_data %>%
  filter(Emp_Status == "TERMINATED") %>%
  group_by(Emp_JobTitle) %>%
  summarise(Employee_Count = n()) %>%
  arrange(desc(Employee_Count))

top_10_terminated_job_titles <- head(terminated_job_title_counts, 10)

cat("Top 10 job titles by employee count (terminated employees only):\n")
cat("\n")
print(top_10_terminated_job_titles)




#Analysis 1-12: Store names with most ACTIVE employees:

active_store_employee_counts <- Emp_data %>%
  filter(Emp_Status == "ACTIVE") %>%
  group_by(Store_Name) %>%
  summarise(Employee_Count = n()) %>%
  arrange(desc(Employee_Count))

top_active_stores <- head(active_store_employee_counts, 10)

cat("Top 10 stores by active employee count:\n")
cat("\n")
print(top_active_stores)


#Analysis 1-13: Store names with most TERMINATED employees:

terminated_store_employee_counts <- Emp_data %>%
  filter(Emp_Status == "TERMINATED") %>%
  group_by(Store_Name) %>%
  summarise(Employee_Count = n()) %>%
  arrange(desc(Employee_Count))

top_terminated_stores <- head(terminated_store_employee_counts, 10)

cat("Top 10 stores by terminated employee count:\n")
cat("\n")
print(top_terminated_stores)


#Analysis 1-14:

termination_types <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), Emp_Status == "TERMINATED") %>%
  group_by(Emp_TerminType) %>%
  summarise(Termination_Count = n())

cat("Termination Types:\n")
cat("\n")
print(termination_types)


#Analysis 1-15:

termination_reason_counts <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), Emp_Status == "TERMINATED") %>%
  group_by(Emp_TerminReason) %>%
  summarise(Termination_Count = n()) %>%
  arrange(desc(Termination_Count))

print(termination_reason_counts)



#Visualizations:

# Visualize the primary age groups
ggplot(data = Emp_data) +
  aes(x = age_groups) +
  geom_bar(fill = "blue") +
  labs(x = "Age Group", y = "Count", title = "Age Group Distribution")


# Visualize the gender distribution
ggplot(data = gender_distribution) +
  aes(x = Emp_GenderLong, y = Employee_Count, fill = Emp_GenderLong) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Count", title = "Gender Distribution") +
  scale_fill_manual(values = c("Female employees" = "pink", "Male employees" = "lightblue"))

# Visualize the top 10 cities by employee count
ggplot(data = top_10_cities) +
  aes(x = reorder(City_Name, Employee_Count), y = Employee_Count) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "City", y = "Employee Count", title = "Top 10 Cities by Employee Count") +
  coord_flip()





#Conclusion 1: The highest age group looks to be between 40-50 indicating and older employee base. 
#Conclusion 2: On average, the age group 50-60 served the longest with an average of 17.3 years
#Conclusion 3: Suggesting older employees having more tenure and younger ones having the most turnover.
#Conclusion 4: Avg service length for 20-30 is 2.5 years.


#Question 2: How does age affect layoffs or resignations

#Analysis 2-1

# Create age groups for all employees
age_groups <- cut(Emp_data$Emp_Age, breaks = c(20, 30, 40, 50, 60, 70))

# Convert Emp_TerminationDate to Date format
Emp_data$Emp_TerminationDate <- as.Date(Emp_data$Emp_TerminationDate)

# Create a new data frame with age groups and termination data
termination_data <- data.frame(age_groups, Emp_TerminationDate = Emp_data$Emp_TerminationDate)

# Count the number of employees in each age range who have left the company
termination_counts <- table(termination_data$age_groups)

# Create a table of age groups and termination types for all terminated employees
term_employees <- Emp_data[!is.na(Emp_data$Emp_TerminationDate), ]

# Print the table of age groups and termination types
print(table_age_term)


##Visualizations for how many terminated:

library(ggplot2)

# Convert the table_age_term into a data frame for plotting
df <- as.data.frame(table_age_term)
df$Age_Groups <- rownames(df)

# Create the stacked bar plot using ggplot2
plot <- ggplot(df, aes(x = Age_Groups, y = Total_Unemployed, fill = factor(Total_Unemployed))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "gray", "green", "blue", "purple")) +
  labs(x = "Age Groups", y = "Count", title = "Termination Types by Age Group") +
  theme_minimal()

# Display the plot
print(plot)


#Visualizations for Type of termination:

library(ggplot2)

# Exclude "Not Applicable" and "Total_Unemployed" from the table
table_term <- table_age_term[-c(2, nrow(table_age_term)), ]

# Convert the table_term into a data frame for plotting
df_term <- as.data.frame(table_term)
df_term$Age_Groups <- rownames(df_term)

# Create the stacked bar plot using ggplot2
plot_term <- ggplot(df_term, aes(x = Age_Groups, y = Involuntary, fill = factor(Involuntary))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "gray", "green")) +
  labs(x = "Age Groups", y = "Count", title = "Type of Termination by Age Group") +
  theme_minimal()

# Display the plot
print(plot_term)


library(ggplot2)

# Exclude "Not Applicable" and "Total_Unemployed" from the table
table_term <- table_age_term[-c(2, nrow(table_age_term)), ]

# Convert the table_term into a data frame for plotting
df_term <- as.data.frame(table_term)
df_term$Age_Groups <- rownames(df_term)

# Create the stacked bar plot for "Voluntary" using ggplot2
plot_voluntary <- ggplot(df_term, aes(x = Age_Groups, y = Voluntary, fill = factor(Voluntary))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "gray", "green")) +
  labs(x = "Age Groups", y = "Count", title = "Voluntary Termination by Age Group") +
  theme_minimal()

# Display the plot
print(plot_voluntary)


#Conclusion: The majority of people that left the company, did so voluntarily and the highest amount came from the age groups 60-70.


#Question 3: What is the majority Gender that left the company and its relation to Age


# Analysis 3-1: Find out some number figures (Total Employees, Total Left, How many of Each Gender Left)

# Get the total number of employees in the dataset
total_employees <- nrow(Emp_data)
cat("Total number of employees:", total_employees, "\n")

# Count the number of males and females who left the company using Emp_GenderLong
# Calculate the total count of males and females in the dataset

term_employees <- Emp_data[Emp_data$Emp_Status == "TERMINATED", ]
gender_count <- table(term_employees$Emp_GenderLong)

# Calculate the total count of males and females in the dataset
total_count <- table(Emp_data$Emp_GenderLong)

# Add up the counts of males and females who left the company
terminated_count <- sum(gender_count)

# Print the counts
cat("Total count of males and females in the dataset:\n")
print(total_count)
cat("\nCount of males and females who left the company:\n")
print(gender_count)
cat("\nTotal count of employees who left the company:\n")
print(terminated_count)


# Analysis 3-2: After getting the figures, determine the AVG age of those genders that left

terminated_employees <- Emp_data[Emp_data$Emp_Status == "TERMINATED", ]
gender_count <- table(terminated_employees$Emp_GenderLong)
avg_age_by_gender <- aggregate(Emp_data$Emp_Age ~ Emp_data$Emp_GenderLong, terminated_employees, mean)
colnames(avg_age_by_gender) <- c("Gender", "Avg Age")
print(gender_count)
print(avg_age_by_gender)


# Analysis 3-3: Determine the number of voluntary vs. involuntary terminations by gender
voluntary_term_count <- table(terminated_employees$Emp_GenderLong, terminated_employees$Emp_TerminType)
colnames(voluntary_term_count) <- c("Involuntary", "Voluntary")
print(voluntary_term_count)


# Analysis 3-4: Determine the reasons for employee terminations
termination_reasons <- table(terminated_employees$Emp_TerminReason)
print(termination_reasons)


#Conclusion: More Females left in comparison to males with the avg ages of both genders that left being 40-43.
#Of those that left, the primary reason was retirement followed closely by resignation


#Visualizations:

# Create a bar plot for the count of males and females who left the company
barplot(gender_count, xlab = "Gender", ylab = "Count", main = "Count of Males and Females who Left the Company",
        ylim = c(0, max(gender_count) + 100))  # Adjust the y-axis number limit


# Create a grouped bar plot for the average age by gender of employees who left the company
barplot(avg_age_by_gender$`Avg Age`, names.arg = avg_age_by_gender$Gender, xlab = "Gender", ylab = "Average Age",
        main = "Average Age by Gender of Employees who Left the Company")

# Create a stacked bar plot for the number of voluntary vs. involuntary terminations by gender
barplot(t(voluntary_term_count), beside = TRUE, legend = c("Involuntary", "Voluntary"),
        col = c("red", "green"), xlab = "Gender", ylab = "Count",
        main = "Number of Voluntary vs. Involuntary Terminations by Gender")


#Conclusion: More Females than Males left resigned 915 vs 570, the avg age of males that left is 41 and 43 for females.



#Question 4: How does job position influence employee turnover and termination rates?


# Analysis 4-1: Calculate turnover rate for each department

turnover_rate_department <- sapply(unique(Emp_data$Department_Name), function(dep) {
  active_emp <- sum(Emp_data$Emp_Status == "ACTIVE" & Emp_data$Department_Name == dep)
  term_emp <- sum(Emp_data$Emp_Status == "TERMINATED" & Emp_data$Department_Name == dep)
  return(round(term_emp / (active_emp + term_emp) * 100, 2))
})

# Create a data frame to store the turnover rate by department
turnover_df_department <- data.frame(Department_Name = unique(Emp_data$Department_Name),
                                     Turnover_Rate = paste0(turnover_rate_department, "%"))

# Sort the data frame by turnover rate in descending order
turnover_df_department <- turnover_df_department[order(turnover_df_department$Turnover_Rate, decreasing = TRUE),]



# Analysis 4-2: Calculate turnover rate for each job title


turnover_rate_job <- sapply(unique(Emp_data$Emp_JobTitle), function(title) {
  active_emp <- sum(Emp_data$Emp_Status == "ACTIVE" & Emp_data$Emp_JobTitle == title)
  term_emp <- sum(Emp_data$Emp_Status == "TERMINATED" & Emp_data$Emp_JobTitle == title)
  return(round(term_emp / (active_emp + term_emp) * 100, 2))
})

# Create a data frame to store the turnover rate by job title
turnover_df_job <- data.frame(Emp_JobTitle = unique(Emp_data$Emp_JobTitle),
                              Turnover_Rate = paste0(turnover_rate_job, "%"))

# Sort the data frame by turnover rate in descending order
turnover_df_job <- turnover_df_job[order(turnover_df_job$Turnover_Rate, decreasing = TRUE),]

# Display the turnover rate by department
cat("Turnover rate by department:\n")
print(turnover_df_department)

# Display the turnover rate by job title
cat("\nTurnover rate by job title:\n")
print(turnover_df_job)


#Visualizations:

library(ggplot2)

# Create a bar plot for turnover rates by department
barplot(as.numeric(sub("%", "", turnover_df$Turnover_Rate)), 
        names.arg = turnover_df$Department_Name,
        xlab = "", ylab = "Turnover Rate (%)",
        col = "blue",
        las = 2)



#Create a bar plot for turnover rates by job title
barplot(as.numeric(sub("%", "", turnover_df$Turnover_Rate)),
        names.arg = turnover_df$Emp_JobTitle,
        xlab = "", ylab = "Turnover Rate (%)",
        main = "Turnover Rates by Job Title",
        col = "blue",
        las = 2,
        cex.names = 0.8)  # Adjust the font size (reduce to 80% of default)

#Alternative Dot Chart

# Calculate turnover rate for each job title
turnover_rates_job <- turnover_df$Turnover_Rate

# Extract numeric values from turnover rates
turnover_values_job <- as.numeric(sub("%", "", turnover_rates_job))

# Create a dot chart for turnover rates by job title
dotchart(turnover_values_job,
         labels = paste(turnover_df$Emp_JobTitle, turnover_rates_job),
         main = "Turnover Rates by Job Title",
         pch = 19, col = "blue")






#Question 5: Where are these terminations and turnover rates mostly occurring?

#Analysis 5-1:

# count terminated employees
terminated_count <- nrow(subset(Emp_data, Emp_Status == "TERMINATED"))

# Filter the data for terminated employees
terminated <- Emp_data[Emp_data$Emp_Status == "TERMINATED", ]

# Count the number of terminated employees by city
termination_counts <- table(terminated$City_Name)

# Get the turnover rate for each city
city_counts <- table(Emp_data$City_Name)
turnover_rates <- termination_counts / city_counts * 100

# Print out the city with the highest turnover rate
cat("The city with the highest turnover rate is", names(turnover_rates)[which.max(turnover_rates)], "with a turnover rate of", round(max(turnover_rates), 2), "%.")


# Print the number of terminated employees in each city sorted by highest
term_counts <- tapply(Emp_data$Emp_Status == "TERMINATED", Emp_data$City_Name, sum)
term_counts <- sort(term_counts, decreasing = TRUE)
cat("Terminated employees by city (sorted):\n")
for (i in seq_along(term_counts)) {
  cat(paste(names(term_counts)[i], ":", term_counts[i], "\n"))
}

# Print turnover rate for each city sorted by highest
turnover_rates <- tapply(Emp_data$Emp_Status == "TERMINATED", Emp_data$City_Name, function(x) sum(x)/length(x))
turnover_rates <- sort(turnover_rates, decreasing = TRUE)
cat("\nTurnover rate by city (sorted):\n")
for (i in seq_along(turnover_rates)) {
  cat(paste(names(turnover_rates)[i], ":", round(turnover_rates[i]*100, 2), "%\n"))
}


#Visualizations:

# Create a bar plot for terminated employees by city
barplot(term_counts, 
        xlab = "City", ylab = "Terminated Employees Count",
        main = "Terminated Employees by City (Sorted)",
        col = "blue",
        las = 2,
        cex.names = 0.8)  # Adjust the font size (reduce to 80% of default)

# Create a bar plot for turnover rates by city
barplot(turnover_rates * 100,
        xlab = "City", ylab = "Turnover Rate (%)",
        main = "Turnover Rates by City (Sorted)",
        col = "blue",
        las = 2,
        cex.names = 0.8)  # Adjust the font size (reduce to 80% of default)


#Conclusion: Vancouver,Victoria and Nanaimo have the highest number of terminations, New Westminister, Pitt Meadows, and Fort Nelson have the highest turnover rates.
#Vancouver:296, Victoria: 151, Nanaimo: 82    New Westminister: 17.32%, Pitt Meadows: 15.79%, Fort Nelson 14.6%.




#Question 6: Why does "Meats", "Produce" and "Customer Service" have so many terminations


#Analysis 6-1: Find out the average age of employees in the departments 

# Subset the data to include only employees in the 3 departments of interest
dept_subset <- subset(Emp_data, Department_Name %in% c("Meats", "Produce", "Customer Service"))

# Calculate the average age of employees in each department
avg_age_by_dept <- aggregate(Emp_Age ~ Department_Name, data = dept_subset, FUN = mean)

# Print the results
print(avg_age_by_dept)



#Analysis 6-2: Find out the Gender distribution in the departments

# Subset the data to include only employees in the 3 departments of interest
dept_subset <- subset(Emp_data, Department_Name %in% c("Meats", "Produce", "Customer Service"))

# Calculate the counts of male and female employees in each department
gender_counts_by_dept <- aggregate(Emp_GenderShort ~ Department_Name, data = dept_subset, FUN = function(x) c(Female = sum(x == "F"), Male = sum(x == "M")))

# Print the results
print(gender_counts_by_dept)



#Analysis 6-3: Find out of those who left, how long did the serve the company for before leaving

# Subset the data to include only employees in the 3 departments of interest
dept_subset <- subset(Emp_data, Department_Name %in% c("Meats", "Produce", "Customer Service"))

# Calculate the average service length of employees in each department
avg_service_length_by_dept <- aggregate(Emp_ServiceLength ~ Department_Name, data = dept_subset, FUN = mean)

# Print the results
print(avg_service_length_by_dept)



#Analysis 6-4: Find out if they left voluntarily or not 

# Subset the data to include only employees in the 3 departments of interest
dept_subset <- subset(Emp_data, Department_Name %in% c("Meats", "Produce", "Customer Service"))

# Tabulate the counts of employees by termination type and department
term_counts_by_dept <- with(dept_subset, table(Department_Name, Emp_TerminType))
term_counts_by_dept <- term_counts_by_dept[, c("Involuntary", "Voluntary")]

# Print the results
print(term_counts_by_dept)



#Analysis 6-5: Find out of each job title, who left voluntarily and who diddnt 

# Subset the data to include only employees in the 3 departments of interest who have a termination date
dept_subset <- subset(Emp_data, Department_Name %in% c("Meats", "Produce", "Customer Service") & !is.na(Emp_TerminationDate))

# Tabulate the counts of employees by job title and department, for only Involuntary and Voluntary terminations
term_job_counts_by_dept <- with(subset(dept_subset, Emp_TerminType != "Not Applicable"), table(Department_Name, Emp_JobTitle, Emp_TerminType))

# Print the results
print(term_job_counts_by_dept)



#Analysis 6-6:

# Subset the data to include only employees in the 3 departments of interest who have a termination date
dept_subset <- subset(Emp_data, Department_Name %in% c("Meats", "Produce", "Customer Service") & !is.na(Emp_TerminationDate))

# Exclude "Not Applicable" termination reason
dept_subset$Emp_TerminReason <- factor(dept_subset$Emp_TerminReason, exclude = "Not Applicable")

# Tabulate the counts of employees by termination reason and department
term_reason_counts_by_dept <- with(dept_subset, table(Department_Name, Emp_TerminReason))

# Print the results
print(term_reason_counts_by_dept)



#Analysis 6-7: Find out the store name where these terminations are happening most for each department

# Subset the data to include only terminated employees in the 3 departments of interest who have a termination date
dept_subset <- subset(Emp_data, Department_Name %in% c("Meats", "Produce", "Customer Service") & Emp_Status == "TERMINATED" & !is.na(Emp_TerminationDate))

# Tabulate the counts of employees by store name
term_counts_by_store <- table(dept_subset$Store_Name)

# Sort the store terminations in descending order
sorted_term_counts <- sort(term_counts_by_store, decreasing = TRUE)

# Get the top 10 stores with the highest terminations
top_10_stores <- head(names(sorted_term_counts), 10)

# Print the top 10 stores with the highest terminations
cat("Top 10 stores with the highest terminations:\n")
for (store in top_10_stores) {
  num_term <- sorted_term_counts[store]
  cat("Store", store, "had", num_term, "terminations.\n")
}


#Visualizations:

#Visualization for Average Age by Department:
library(ggplot2)

ggplot(avg_age_by_dept, aes(x = Department_Name, y = Emp_Age)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Department", y = "Average Age", title = "Average Age by Department")


#Visualization for Gender Distribution by Department:
# Extract the department names
dept_names <- gender_counts_by_dept$Department_Name

# Create a bar plot for the gender distribution by department
barplot(t(as.matrix(gender_counts_by_dept[, -1])),
        beside = TRUE,
        xlab = "Department",
        ylab = "Count",
        main = "Gender Distribution by Department",
        col = c("pink", "lightblue"),
        ylim = c(0, max(gender_counts_by_dept[, -1]) + 100),
        names.arg = dept_names)

# Add a legend
legend("topright",
       legend = c("Female", "Male"),
       fill = c("pink", "lightblue"))



#Alternative Pie-Chart for Gender Distribution by Department:
# Calculate the total count for each department
gender_counts_by_dept$total <- rowSums(gender_counts_by_dept[, -1])

# Create a pie chart with a legend
pie(gender_counts_by_dept$total,
    labels = gender_counts_by_dept$Department_Name,
    col = c("pink", "lightblue"),
    main = "Gender Distribution by Department")

# Add a legend
legend("topright",
       legend = c("Female", "Male"),
       fill = c("pink", "lightblue"))



#Visualization for Average Service Length by Department:
barplot(avg_service_length_by_dept$Emp_ServiceLength,
        names.arg = avg_service_length_by_dept$Department_Name,
        xlab = "Department",
        ylab = "Average Service Length",
        main = "Average Service Length by Department",
        col = "green")



#Visualization for counts of employees by job title, department, and termination type

# Calculate the maximum count
max_count <- max(term_job_counts_by_dept)

# Create a stacked bar plot for job titles and termination types with increased count
barplot(as.matrix(term_job_counts_by_dept[, , "Involuntary"]),
        beside = TRUE,
        col = c("red", "green"),
        xlab = "Department",
        ylab = "Count",
        main = "Termination Type by Job Title and Department",
        legend.text = c("Involuntary", "Voluntary"),
        args.legend = list(x = "topright"),
        ylim = c(0, max_count + 50))

# Add the bars for voluntary terminations
barplot(as.matrix(term_job_counts_by_dept[, , "Voluntary"]),
        beside = TRUE,
        col = c("red", "green"),
        add = TRUE)



#Visualizations for termination reasons by department

# Transpose the term_reason_counts_by_dept matrix
term_reason_counts_transposed <- t(term_reason_counts_by_dept)

# Create a stacked bar plot for termination reasons by department
barplot(term_reason_counts_transposed,
        col = c("red", "green", "blue"),
        xlab = "Department",
        ylab = "Count",
        main = "Termination Reasons by Department",
        legend.text = colnames(term_reason_counts_by_dept),
        args.legend = list(x = "topright"),
        beside = TRUE)



#Visualization for Termination counts by store and department

# Subset the data to include only the top 5 stores with the most terminations for each department
top_stores_subset <- subset(dept_subset, Store_Name %in% top_5_stores)

# Create separate bar plots for each department
for (dept in unique(top_stores_subset$Department_Name)) {
  dept_data <- subset(top_stores_subset, Department_Name == dept)
  
  # Tabulate the counts of terminations by store for the department
  term_counts_by_store_dept <- table(dept_data$Store_Name)
  
  # Sort the stores by termination counts in descending order
  sorted_stores <- names(sort(term_counts_by_store_dept, decreasing = TRUE))
  
  # Keep only the top 5 stores
  top_5_stores_dept <- sorted_stores[1:5]
  
  # Subset the term_counts_by_store_dept table to include only the top 5 stores
  term_counts_top_stores <- term_counts_by_store_dept[top_5_stores_dept]
  
  # Create a bar plot for the top 5 stores with terminations in the department
  barplot(term_counts_top_stores,
          col = rainbow(length(top_5_stores_dept)),
          xlab = "Store",
          ylab = "Termination Count",
          main = paste("Top 5 Stores with Most Terminations in", dept),
          names.arg = top_5_stores_dept)
}



#Visualization for top 10 stores with highest terminations:

# Subset the data to include only the top 10 stores with the highest number of terminations
top_10_stores <- names(term_counts_by_store)[order(term_counts_by_store, decreasing = TRUE)][1:10]
term_counts_top_10_stores <- term_counts_by_store[top_10_stores]

# Create a bar plot for the top 10 stores with the highest terminations
barplot(term_counts_top_10_stores,
        col = rainbow(length(term_counts_top_10_stores)),
        xlab = "Store Name",
        ylab = "Termination Count",
        main = "Top 10 Stores with Highest Terminations",
        ylim = c(0, max(term_counts_top_10_stores) + 10),
        names.arg = top_10_stores,
        cex.names = 0.8,
        las = 2)





#Conclusion: These departments consisted mainly of older women who were at the age of retirement and did so voluntarily.
#NOtes:
#These departments might have a high rate of termination due to the avg age in the Meats and Produce Department being 50
#Females also have a higher presence in these departments which leads to the thought of older women not wanting to do the labour in these departments anymore
#The service lenghts for these departments being on avg 13 years for Meats and produce and 6 years for customer service
#This makes sense as customer service had a younger age demographic, 32 and evenly distributed gender roles.
#The vast majority of terminations were voluntary with retirement and resignation dominatingin numbers, further supporting the conclusion
#Majority of this occurred in Store 37. Interestingly, store 35 had the most amount of terminations.



#Question 7: Why does Store 35 have the highest amount of terminations

#Analysis 7-1

# Subset the data to include only terminated employees in Store 35
store_35_term_subset <- subset(Emp_data, Store_Name == 35 & !is.na(Emp_TerminationDate))

# Tabulate the counts of terminated employees by termination type, and omit "Not Applicable" termination type
store_35_term_counts <- table(store_35_term_subset$Emp_TerminType)
store_35_term_counts <- store_35_term_counts[!names(store_35_term_counts) %in% "Not Applicable"]

# Calculate the total number of voluntary and involuntary terminations
total_voluntary_term <- store_35_term_counts["Voluntary"]
total_involuntary_term <- store_35_term_counts["Involuntary"]

# Calculate the total number of terminated employees
total_term <- sum(store_35_term_counts)

# Print the results
cat("Store 35 total terminations:", total_term, "\n")
cat("Termination types; Voluntary:", total_voluntary_term, "Involuntary:", total_involuntary_term, "\n")



#Analysis 7-2:


# Subset the data to include only terminated employees in Store 35
store_35_term_subset <- subset(Emp_data, Store_Name == 35 & !is.na(Emp_TerminationDate) & Emp_TerminType != "Not Applicable")

# Create a data frame with the age and termination reason of terminated employees in Store 35
store_35_term_age_reason <- data.frame(Age = as.numeric(as.character(store_35_term_subset$Emp_Age)),
                                       Termination_Reason = store_35_term_subset$Emp_TerminReason)

# Print the results
cat("Age and Termination Reason of terminated employees in Store 35:\n")
print(store_35_term_age_reason)



#Analysis 7-3:

# Subset the data to include only terminated employees in Store 35
store_35_term_subset <- subset(Emp_data, Store_Name == 35 & !is.na(Emp_TerminationDate) & Emp_TerminType != "Not Applicable")

# Create a data frame with the age, gender, and termination reason of terminated employees in Store 35
store_35_term_age_reason <- data.frame(
  Age = as.numeric(as.character(store_35_term_subset$Emp_Age)),
  Gender = store_35_term_subset$Emp_GenderLong,
  Termination_Reason = store_35_term_subset$Emp_TerminReason
)

# Display counts of termination reasons by gender
table(store_35_term_age_reason$Gender, store_35_term_age_reason$Termination_Reason)



#Analysis 7-4:

# Subset the data to include only terminated employees in Store 35
store_35_term_subset <- subset(Emp_data, Store_Name == 35 & !is.na(Emp_TerminationDate) & Emp_TerminType != "Not Applicable")

# Create a data frame with the job title and termination reason of terminated employees in Store 35
store_35_term_job_reason <- data.frame(
  Job_Title = store_35_term_subset$Emp_JobTitle,
  Termination_Reason = store_35_term_subset$Emp_TerminReason
)

# Display counts of termination reasons by job title
table(store_35_term_job_reason$Job_Title, store_35_term_job_reason$Termination_Reason)



#Analysis 7-5:

# Subset data for store 35
store_35 <- Emp_data[Emp_data$Store_Name == 35, ]

# Calculate average service length for terminated employees in store 35
avg_service_length <- mean(store_35$Emp_ServiceLength[store_35$Emp_Status == "TERMINATED"])

# Print result
cat("The average service length of terminated employees in store 35 is", round(avg_service_length, 1), "years.")



#Analysis 7-6

# Subset data for store 35
store_35 <- Emp_data[Emp_data$Store_Name == 35, ]

# Subset data for terminated employees in store 35
term_emp <- store_35[store_35$Emp_Status == "TERMINATED", ]

# Calculate count of terminated employees in store 35
term_count <- nrow(term_emp)

# Calculate count of terminated employees by department and job title
dept_count <- table(term_emp$Department_Name)
title_count <- table(term_emp$Emp_JobTitle)

# Print results
cat("There were", term_count, "terminated employees in store 35.")
cat("\n\nOf those, the breakdown by department is:\n")
print(dept_count)
cat("\n\nAnd the breakdown by job title is:\n")
print(title_count)


#Conclusion: Store 35 had a very old employee base in its 60s the majority of which were women who retired.

#Visualizations

#Analysis 7-1
library(ggplot2)

# Create a data frame for termination counts
term_counts_df <- data.frame(Termination_Type = names(store_35_term_counts),
                             Count = store_35_term_counts,
                             stringsAsFactors = FALSE)

# Sort the data frame by count in descending order
term_counts_df <- term_counts_df[order(term_counts_df$Count, decreasing = TRUE), ]

# Create a lollipop plot
ggplot(term_counts_df, aes(x = Count, y = Termination_Type)) +
  geom_segment(aes(x = 0, xend = Count, y = Termination_Type, yend = Termination_Type), color = "gray50") +
  geom_point(size = 3, color = "steelblue") +
  labs(title = "Termination Types in Store 35",
       x = "Count",
       y = "Termination Type") +
  theme_minimal()


#Analysis 7-2:

library(ggplot2)

# Create a bar plot for age and termination reason
ggplot(store_35_term_age_reason, aes(x = Termination_Reason, fill = factor(Age))) +
  geom_bar(position = "fill") +
  labs(title = "Age and Termination Reason in Store 35",
       x = "Termination Reason",
       y = "Proportion",
       fill = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")



#Question 8: Why does Meats, Dairy and Produce have a trend of employees leaving?


#Analysis 8-1:

# Filter the data to only include terminated and active employees in the Meats, Dairy, and Produce departments
dept_names <- c("Meats", "Dairy", "Produce")
Emp_data_dept <- Emp_data[Emp_data$Department_Name %in% dept_names,]

# Calculate the average age of terminated and active employees in each department
avg_age <- aggregate(Emp_Age ~ Department_Name + Emp_Status, Emp_data_dept, mean)

# Print the results
print(avg_age)



#Analysis 8-2:

# Filter the data to only include terminated and active employees in the Meats, Dairy, and Produce departments
dept_names <- c("Meats", "Dairy", "Produce")
Emp_data_dept <- Emp_data[Emp_data$Department_Name %in% dept_names,]

# Calculate the gender distribution of terminated employees in each department
gender_dist_term <- aggregate(list(Frequency = Emp_data_dept[Emp_data_dept$Emp_Status == "TERMINATED",]$Emp_GenderShort), by = list(Department_Name = Emp_data_dept[Emp_data_dept$Emp_Status == "TERMINATED",]$Department_Name), FUN = table)
gender_dist_term$Percentage <- round(gender_dist_term$Frequency / sum(gender_dist_term$Frequency) * 100, 2)
gender_dist_term$Emp_Status <- "TERMINATED"

# Calculate the gender distribution of active employees in each department
gender_dist_active <- aggregate(list(Frequency = Emp_data_dept[Emp_data_dept$Emp_Status == "ACTIVE",]$Emp_GenderShort), by = list(Department_Name = Emp_data_dept[Emp_data_dept$Emp_Status == "ACTIVE",]$Department_Name), FUN = table)
gender_dist_active$Percentage <- round(gender_dist_active$Frequency / sum(gender_dist_active$Frequency) * 100, 2)
gender_dist_active$Emp_Status <- "ACTIVE"

# Combine the two data frames
gender_dist <- rbind(gender_dist_term, gender_dist_active)

# Print the results
print(gender_dist)



#Analysis 8-3:

# Filter the data to only include terminated and active employees in the Meats, Dairy, and Produce departments
dept_names <- c("Meats", "Dairy", "Produce")
Emp_data_dept <- Emp_data[Emp_data$Department_Name %in% dept_names,]

# Calculate the average service length of terminated employees in each department
avg_service_length_term <- aggregate(Emp_ServiceLength ~ Department_Name, Emp_data_dept[Emp_data_dept$Emp_Status == "TERMINATED",], mean)

# Calculate the average service length of active employees in each department
avg_service_length_active <- aggregate(Emp_ServiceLength ~ Department_Name, Emp_data_dept[Emp_data_dept$Emp_Status == "ACTIVE",], mean)

# Add labels to distinguish between terminated and active employees
avg_service_length_term$Emp_Status <- "TERMINATED"
avg_service_length_active$Emp_Status <- "ACTIVE"

# Combine the two data frames
avg_service_length <- rbind(avg_service_length_term, avg_service_length_active)

# Print the results
print(avg_service_length)



#Analysis 8-4:

# Calculate the average age of employees in each department
avg_age <- aggregate(Emp_Age ~ Department_Name, Emp_data, mean)

# Print the results
print(avg_age)



#Visualization:

#Analysis 1-1 Pie Chart:

# Subset the data to include only terminated and active employees in the Meats, Dairy, and Produce departments
Department_Names <- c("Meats", "Dairy", "Produce")
Emp_data_dept <- Emp_data[Emp_data$Department_Name %in% Department_Names,]

# Calculate the average age of terminated and active employees in each department
avg_age <- aggregate(Emp_Age ~ Department_Name + Emp_Status, Emp_data_dept, mean)

# Create a data frame for pie chart
pie_data <- data.frame(Department = avg_age$Department_Name,
                       Emp_Status = avg_age$Emp_Status,
                       Avg_Age = avg_age$Emp_Age)

# Create the pie chart with rounded average ages
library(ggplot2)

ggplot(pie_data, aes(x = "", y = Avg_Age, fill = Emp_Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ Department) +
  theme_void() +
  labs(title = "Average Age of Terminated and Active Employees",
       fill = "Employee Status") +
  geom_text(aes(label = round(Avg_Age)), position = position_stack(vjust = 0.5))



#Analysis 1-1 Grouped Bar Plot

# Create a grouped bar plot for average age by department and employee status
library(ggplot2)

ggplot(avg_age, aes(x = Department_Name, y = Emp_Age, fill = Emp_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Department", y = "Average Age", title = "Average Age of Terminated and Active Employees") +
  scale_fill_manual(values = c("red", "lightblue"), labels = c("Active", "Terminated")) +
  theme_minimal()



#Analysis 1-3: Grouped Bar Plot

# Create a grouped bar plot for average service length by department and employee status
library(ggplot2)

ggplot(avg_service_length, aes(x = Department_Name, y = Emp_ServiceLength, fill = Emp_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Department", y = "Average Service Length (Years)", title = "Average Service Length of Terminated and Active Employees") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Terminated", "Active")) +
  theme_minimal()


#Analysis 1-4: Bar Plot

# Create a bar plot for average age by department
library(ggplot2)

ggplot(avg_age, aes(x = Department_Name, y = Emp_Age)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Department", y = "Average Age", title = "Average Age of Employees by Department") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()





#Conclusion: These departments have an older demographic that on average has served for more than 10 years leading to a high rate of retiring
#Notes: Concurrently, Accounts Payable, Accounting and Audits also have a similarly aged demographic.
#The reasoning on the similar age groups yet differing terminations rates could be due to better job stability and pay for those in white collar job positions



#Question 9: Why does the Information Technology department have a high turnover rate %?

#Analysis 9-1: 


library(dplyr)

# Subset the data to include only terminated employees
term_employees <- Emp_data %>%
  filter(Emp_Status == "TERMINATED")

# Calculate the number of terminated employees by department
term_counts <- term_employees %>%
  group_by(Department_Name) %>%
  summarise(Terminated_Count = n())

# Calculate the total number of employees in each department
total_counts <- Emp_data %>%
  group_by(Department_Name) %>%
  summarise(Total_Count = n())

# Merge the termination and total counts data frames
turnover_data <- merge(term_counts, total_counts, by = "Department_Name", all = TRUE)

# Calculate the turnover rate for each department
turnover_data$Turnover_Rate <- (turnover_data$Terminated_Count / turnover_data$Total_Count) * 100

# Print the turnover rate for each department
print(turnover_data)


#Analysis 9-2:

# Print column names and data types for term_employees
str(term_employees)

#Analysis 9-3:

# Filter the term_employees data frame for the IT department
it_employees <- term_employees %>%
  filter(Department_Name == "Information Technology")

# Examine the unique termination reasons for the IT department
unique_termination_reasons <- unique(it_employees$Emp_TerminReason)
print(unique_termination_reasons)

#Analysis 9-4:

# Print the summary statistics for age in the IT department
summary(it_employees$Emp_Age)



#Analysis 9-5:

# Examine the service length within the IT department
service_length <- it_employees %>%
  select(Emp_ServiceLength)

# Summary statistics of service length
summary(service_length)


#Analysis 9-6:

# Extract unique termination types for the IT department
unique_termination_types <- unique(it_employees$Emp_TerminType)
print(unique_termination_types)

#Analysis 9-7:

# Count the number of terminated employees by gender in the IT department
gender_counts <- table(it_employees$Emp_GenderShort)
print(gender_counts)

#Analysis 9-8:

# Extract the termination years
termination_years <- format(it_employees$Emp_TerminationDate, "%Y")

# Count the number of terminations by year
year_counts <- table(termination_years)
print(year_counts)

#Analysis 9-9:

# Subset the original data for the IT department
it_department <- subset(Emp_data, Department_Name == "Information Technology")

# Print the data for the IT department
print(it_department)

# Print the unique termination reasons for the IT department
unique_termination_reasons <- unique(it_department$Emp_TerminReason)
print(unique_termination_reasons)

# Print the summary statistics for age in the IT department
summary(it_department$Emp_Age)

# Examine the service length within the IT department
summary(it_department$Emp_ServiceLength)

# Extract unique termination types for the IT department
unique_termination_types <- unique(it_department$Emp_TerminType)
print(unique_termination_types)

# Count the number of terminated employees by gender in the IT department
gender_counts <- table(it_department$Emp_GenderShort)
print(gender_counts)

# Extract the termination years
termination_years <- format(it_department$Emp_TerminationDate, "%Y")

# Count the number of terminations by year
year_counts <- table(termination_years)
print(year_counts)

# Examine the unique termination reasons for the IT department
unique_termination_reasons <- unique(it_department$Emp_TerminReason)
print(unique_termination_reasons)


#Visualizations:

# Violin plot of age distribution by department
library(ggplot2)
ggplot(Emp_data, aes(x = Department_Name, y = Emp_Age)) +
  geom_violin(fill = "lightblue", alpha = 0.8) +
  labs(x = "Department", y = "Age", title = "Age Distribution by Department")


# Create a bar plot of termination reasons
ggplot(data = it_department, aes(x = Emp_TerminReason)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Termination Reason", y = "Count", title = "Termination Reasons in IT Department")



#Conclusion: Based on the analysis of the IT department data, it can be concluded that all terminated employees in this department retired voluntarily after serving an average of 19 years.
#The termination reasons were primarily retirement, and the terminated employees were predominantly male. The age distribution of terminated employees ranged from 57 to 60 years, with an average age of 58.5 years.
#The termination events occurred in the year 2009. Overall, these findings suggest a voluntary retirement trend within the IT department, with employees choosing to retire after a long tenure with the company.


#Question 10: What departments are doing well in terms of turnover. Why are they doing well compared to others


# Analysis 10-1: Calculate turnover rates for each department.

library(dplyr)

term_employees <- Emp_data %>%
  filter(Emp_Status == "TERMINATED")

term_counts <- term_employees %>%
  group_by(Department_Name) %>%
  summarise(Terminated_Count = n())

total_counts <- Emp_data %>%
  group_by(Department_Name) %>%
  summarise(Total_Count = n())

turnover_data <- merge(term_counts, total_counts, by = "Department_Name", all = TRUE)

turnover_data$Turnover_Rate <- (turnover_data$Terminated_Count / turnover_data$Total_Count) * 100

print(turnover_data)



# Analysis 10-2:  Identify unique termination reasons within the Processed Foods department.

processed_foods_dept <- subset(Emp_data, Department_Name == "Processed Foods")

unique_termination_reasons <- unique(processed_foods_dept$Emp_TerminReason)

print(unique_termination_reasons)



# Analysis 10-3: Count the number of employees by gender in the Processed Foods department.

processed_foods_dept <- subset(Emp_data, Department_Name == "Processed Foods" & Emp_Status == "ACTIVE")

gender_counts <- table(processed_foods_dept$Emp_GenderLong)

print(gender_counts)



# Analysis 10-4: Identify unique job titles within the Processed Foods department.

processed_foods_dept <- subset(Emp_data, Department_Name == "Processed Foods")

unique_job_titles <- unique(processed_foods_dept$Emp_JobTitle)

print(unique_job_titles)



# Analysis 10-5: Count the number of employees by job title in the Processed Foods department.

processed_foods_dept <- subset(Emp_data, Department_Name == "Processed Foods")

job_title_counts <- table(processed_foods_dept$Emp_JobTitle)

print(job_title_counts)



# Analysis 10-6:  Analyze the age distribution of employees in the Processed Foods department.

processed_foods_dept <- subset(Emp_data, Department_Name == "Processed Foods")

age_distribution <- table(processed_foods_dept$Emp_Age)
age_df <- data.frame(Age = as.numeric(names(age_distribution)), Count = as.numeric(age_distribution))

print(age_df, row.names = FALSE)



# Analysis 10-7: Calculate the average service length (in years) by job title in the Processed Foods department.

avg_service_length <- aggregate(Emp_ServiceLength ~ Emp_JobTitle, processed_foods_dept, mean)

print(avg_service_length)



#Analysis 10-8: Determine the number of unique cities where employees in the Processed Foods department are located.


processed_foods_dept <- subset(Emp_data, Department_Name == "Processed Foods")

unique_cities <- unique(processed_foods_dept$City_Name)

num_cities <- length(unique_cities)

print(num_cities)
print(unique_cities)



#Analysis 10-9: Count the number of terminations by city in the Processed Foods department.

processed_foods_dept <- subset(Emp_data, Department_Name == "Processed Foods" & Emp_Status == "TERMINATED")

termination_counts <- table(processed_foods_dept$City_Name)

print(termination_counts)



#Analysis 10-10: Calculate turnover rates by city in the Processed Foods department and identify the top 20 cities with the highest termination counts.

library(dplyr)

termination_counts <- Emp_data %>%
  filter(Department_Name == "Processed Foods" & Emp_Status == "TERMINATED") %>%
  group_by(City_Name, Department_Name) %>%
  summarise(Termination_Count = n())

total_counts <- Emp_data %>%
  filter(Department_Name == "Processed Foods") %>%
  group_by(City_Name, Department_Name) %>%
  summarise(Total_Count = n())

termination_data <- merge(termination_counts, total_counts, by = c("City_Name", "Department_Name"), all = TRUE)

termination_data$Turnover_Rate <- (termination_data$Termination_Count / termination_data$Total_Count) * 100

sorted_termination_data <- termination_data %>%
  arrange(desc(Termination_Count)) %>%
  head(20)

print(sorted_termination_data)




#Visualizations:


# Bubble Plot for Turnover rates by departmnets 

library(ggplot2)

# Sort the data by turnover rate in descending order
turnover_data_sorted <- turnover_data %>%
  arrange(desc(Turnover_Rate))

# Create a bubble plot
ggplot(turnover_data_sorted, aes(x = Department_Name, y = Total_Count, size = Turnover_Rate, fill = Department_Name)) +
  geom_point(shape = 21, color = "black") +
  scale_size_continuous(range = c(5, 15)) +
  xlab("Department") +
  ylab("Total Count") +
  ggtitle("Turnover Rates by Department") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



# Lollipop Plot for turnover rate by department 

library(ggplot2)

# Sort the data by turnover rate in descending order
turnover_data_sorted <- turnover_data %>%
  arrange(desc(Turnover_Rate))

# Create a lollipop plot
ggplot(turnover_data_sorted, aes(x = Department_Name, y = Turnover_Rate, fill = Department_Name)) +
  geom_segment(aes(xend = Department_Name, yend = 0), color = "black") +
  geom_point(shape = 21, color = "black", size = 3) +
  xlab("Department") +
  ylab("Turnover Rate (%)") +
  ggtitle("Turnover Rates by Department") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



#Lollipop plot for employee number by job title

library(ggplot2)

# Create a data frame with job titles and corresponding counts
job_data <- data.frame(Job_Title = names(job_title_counts), Count = as.numeric(job_title_counts))

# Sort the data frame in descending order of counts
job_data <- job_data[order(job_data$Count, decreasing = TRUE), ]

# Create the lollipop plot using ggplot
ggplot(job_data, aes(x = Job_Title, y = Count)) +
  geom_segment(aes(x = Job_Title, xend = Job_Title, y = 0, yend = Count), color = "red") +
  geom_point(color = "red", size = 3) +
  coord_flip() +
  labs(title = "Number of Employees by Job Title in Processed Foods Department", x = "Job Title", y = "Count") +
  theme_minimal()



#Lollipop plot for avg service length of job titles in processed foods department

library(ggplot2)

# Create a data frame with job titles and average service lengths
service_data <- data.frame(Job_Title = avg_service_length$Emp_JobTitle, Avg_Service_Length = avg_service_length$Emp_ServiceLength)

# Sort the data frame in descending order of average service lengths
service_data <- service_data[order(service_data$Avg_Service_Length, decreasing = TRUE), ]

# Create the lollipop plot using ggplot
ggplot(service_data, aes(x = Avg_Service_Length, y = Job_Title)) +
  geom_segment(aes(x = 0, xend = Avg_Service_Length, y = Job_Title, yend = Job_Title), color = "skyblue") +
  geom_point(color = "skyblue", size = 3) +
  labs(title = "Average Service Length by Job Title in Processed Foods Department", x = "Average Service Length (years)", y = "Job Title") +
  theme_minimal()




#Turnover rates by city in proces foods department

# Limit decimal points to two
sorted_termination_data$Turnover_Rate <- round(sorted_termination_data$Turnover_Rate, 2)

# Plot the turnover rates
barplot(sorted_termination_data$Turnover_Rate, names.arg = sorted_termination_data$City_Name, 
        xlab = "City", ylab = "Turnover Rate (%)", 
        main = "Turnover Rates by City in Processed Foods Department",
        ylim = c(0, max(sorted_termination_data$Turnover_Rate) + 1),
        col = "skyblue", border = "black", las = 2)














#Conclusion: Based on the analysis conducted, the Processed Foods department has relatively low turnover rates compared to other departments, with employees primarily terminating due to retirement, resignation, and layoff.
#The department has a balanced gender distribution and consists mainly of Processed Foods Managers and Shelf Stockers.
#Additionally, the department has employees spanning a wide age range, with higher average service lengths for managers.
#The department is located in multiple cities, with Vancouver having the highest termination count and turnover rate.

#Notes: It can be inferred that Processed Foods has a low turnover rate due to the shelf stockers being aged in the mid 20s
#With a service length < 6 years. Indicating a fresh new workforce that is yet to retire or resign


#Question 11: What year had the most amount of terminations for this company, and why. 
#Note: 1900 was excluded over suspicion of an error in the dataset
names(Emp_data)

#Analysis 11-1: 


year_termination_counts <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), lubridate::year(Emp_TerminationDate) != 1900) %>%
  mutate(Termination_Year = lubridate::year(Emp_TerminationDate)) %>%
  group_by(Termination_Year) %>%
  summarise(Termination_Count = n())

year_summary <- data.frame(Year = year_termination_counts$Termination_Year, Termination_Count = year_termination_counts$Termination_Count)
print(year_summary)

#Note: 1900 was excluded over suspicion of an error in the dataset



#Analysis 11-2: 

age_average_by_year <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), lubridate::year(Emp_TerminationDate) != 1900) %>%
  mutate(Termination_Year = lubridate::year(Emp_TerminationDate)) %>%
  group_by(Termination_Year) %>%
  summarise(Age_Average = mean(Emp_Age, na.rm = TRUE))

print(age_average_by_year)



#Analysis 11-3:

service_length_by_year <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), lubridate::year(Emp_TerminationDate) != 1900) %>%
  mutate(Termination_Year = lubridate::year(Emp_TerminationDate)) %>%
  group_by(Termination_Year) %>%
  summarise(Avg_Service_Length = mean(Emp_ServiceLength))

print(service_length_by_year)



#Analysis 11-4:

gender_distribution <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), lubridate::year(Emp_TerminationDate) != 1900) %>%
  mutate(Termination_Year = lubridate::year(Emp_TerminationDate)) %>%
  group_by(Termination_Year, Emp_GenderLong) %>%
  summarise(Termination_Count = n()) %>%
  group_by(Termination_Year) %>%
  mutate(Percentage = Termination_Count / sum(Termination_Count) * 100) %>%
  ungroup()

print(gender_distribution)



#Analysis 11-5:

city_termination_counts <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), lubridate::year(Emp_TerminationDate) != 1900) %>%
  mutate(Termination_Year = lubridate::year(Emp_TerminationDate)) %>%
  group_by(Termination_Year, City_Name) %>%
  summarise(Termination_Count = n()) %>%
  arrange(Termination_Year, desc(Termination_Count))

top_city_per_year <- city_termination_counts %>%
  group_by(Termination_Year) %>%
  slice_max(Termination_Count)

print(top_city_per_year)



#Analysis 11-6:

department_termination_counts <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), lubridate::year(Emp_TerminationDate) != 1900) %>%
  mutate(Termination_Year = lubridate::year(Emp_TerminationDate)) %>%
  group_by(Termination_Year, Department_Name) %>%
  summarise(Termination_Count = n()) %>%
  arrange(Termination_Year, desc(Termination_Count))

for (year in 2006:2015) {
  year_departments <- department_termination_counts %>%
    filter(Termination_Year == year) %>%
    top_n(1, Termination_Count) %>%
    select(Termination_Year, Department_Name, Termination_Count)
  
  print(year_departments)
}



#Analysis 11-7:

job_title_termination_counts <- Emp_data %>%
  filter(!is.na(Emp_TerminationDate), lubridate::year(Emp_TerminationDate) != 1900) %>%
  mutate(Termination_Year = lubridate::year(Emp_TerminationDate)) %>%
  group_by(Termination_Year, Emp_JobTitle) %>%
  summarise(Termination_Count = n()) %>%
  arrange(Termination_Year, desc(Termination_Count))

for (year in 2006:2015) {
  year_job_titles <- job_title_termination_counts %>%
    filter(Termination_Year == year) %>%
    top_n(1, Termination_Count) %>%
    select(Termination_Year, Emp_JobTitle, Termination_Count)
  
  print(year_job_titles)
}




#Visualizations:


# Heatmap plot for year-wise termination counts
library(ggplot2)


ggplot(year_summary, aes(x = Year, y = Termination_Count, fill = Termination_Count)) +
  geom_tile(color = "white", linewidth = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Year-wise Termination Counts", x = "Year", y = "Termination Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))



#Lollipop plot for AVG Age By Year


library(ggplot2)
library(dplyr)

ggplot(age_average_by_year, aes(x = Termination_Year, y = Age_Average)) +
  geom_segment(aes(x = Termination_Year, xend = Termination_Year, y = 0, yend = Age_Average), color = "skyblue") +
  geom_point(color = "skyblue", size = 3) +
  geom_text(aes(label = round(Age_Average, 1)), vjust = -1.5, color = "black") +
  labs(title = "Average Age by Year", x = "Year", y = "Average Age") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.major.y = element_blank()) +
  coord_flip() +
  annotate("text", x = age_average_by_year$Termination_Year, y = -0.5, label = age_average_by_year$Termination_Year, color = "black", size = 3, hjust = -0.1) +
  ylim(0, max(age_average_by_year$Age_Average) * 1.2)





#Bubble plot for service length by year

library(ggplot2)
library(dplyr)

# Set the size of the bubbles
bubble_size <- 50

# Create the bubble plot
ggplot(service_length_by_year, aes(x = Termination_Year, y = Avg_Service_Length, size = Avg_Service_Length)) +
  geom_point(color = "skyblue", fill = "red", alpha = 0.7, shape = 21) +
  scale_size_continuous(range = c(1, bubble_size), guide = FALSE) +
  labs(title = "Average Service Length by Year", x = "Year", y = "Average Service Length") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))





#Stacked bar plot for gender distribution

library(ggplot2)
library(dplyr)

# Create the plot
ggplot(gender_distribution, aes(x = Termination_Year, y = Percentage, fill = Emp_GenderLong)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Gender Distribution of Terminations by Year", x = "Year", y = "Percentage") +
  scale_fill_manual(values = c("Female" = "skyblue", "Male" = "lightgreen")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))




#bar plot for termiantions by city per year
library(ggplot2)

# Plot the data
ggplot(top_city_per_year, aes(x = factor(Termination_Year), y = Termination_Count, fill = City_Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top City with Highest Termination Count by Year", x = "Year", y = "Termination Count", fill = "City") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))





#Bar plot for departments with highest terminations per year

library(ggplot2)

# Plot the data
ggplot(department_termination_counts, aes(x = factor(Termination_Year), y = Termination_Count, fill = Department_Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Department with Highest Termination Count by Year", x = "Year", y = "Termination Count", fill = "Department") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))




# Create a heatmap for departments with highest terminations per year

library(ggplot2)

ggplot(department_termination_counts, aes(x = factor(Termination_Year), y = Department_Name, fill = Termination_Count)) +
  geom_tile(color = "white") +
  labs(title = "Department with Highest Termination Count by Year", x = "Year", y = "Department", fill = "Termination Count") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))




#Bar plot for job titles with highest termination count per year

library(ggplot2)

# Plotting the job title with the highest termination count for each year
ggplot(job_title_termination_counts, aes(x = factor(Termination_Year), y = Termination_Count, fill = Emp_JobTitle)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Job Title with Highest Termination Count by Year", x = "Year", y = "Termination Count", fill = "Job Title") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "right") +
  coord_flip()



#Lollipop plot for job titles with highest termination count per year

library(ggplot2)

# Plotting the job title with the highest termination count for each year (lollipop plot)
ggplot(job_title_termination_counts, aes(x = factor(Termination_Year), y = Termination_Count, fill = Emp_JobTitle)) +
  geom_segment(aes(xend = factor(Termination_Year), yend = 0), color = "gray") +
  geom_point(shape = 21, size = 4, color = "black") +
  labs(title = "Job Title with Highest Termination Count by Year", x = "Year", y = "Termination Count", fill = "Job Title") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "right")




#Conclusion: 

#Based on the analysis of termination data from 2006 to 2015, several insights can be inferred. The number of terminations generally increased from 2006 to 2014, with a slight decrease in 2015.
#The average age at termination decreased over the years, except for a slight increase in 2015. 
#The average service length at termination varied across the years without a clear trend. 
#Females had a higher percentage of terminations compared to males in most years. 
#Vancouver consistently had the highest number of terminations, and the Meats department and Meat Cutter job title had the most terminations in multiple years.
#These findings can provide valuable information for understanding employee turnover patterns and identifying areas for improvement in the organization's workforce management.