# ======================== Install Library ========================
# This part can be skipped if library already installed
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggrepel")

# ======================== Load Library ========================
library(dplyr)
library(ggplot2)
library(ggrepel)

# ======================== Load Data ========================

# Load the employee data from csv file into variables. (table)
employee_data <- read.csv("employee_attrition.csv")
# Start a new window for easier graph viewing
# The reason is because I can resize the window and the graph
# will follows it.
windows()


# ======================== Data Cleaning ========================

# Copy the data to another variable just in case we want
# to revert course the change.
original_data <- employee_data
# employee_data <- original_data
View(original_data)

# Normalize column name
# We normalize all column names to use snake_case format
# And we change some column names to use more meaningful name
colnames(employee_data)[1] <- "employee_id"
colnames(employee_data)[2] <- "record_date"
colnames(employee_data)[3] <- "birth_date"
colnames(employee_data)[4] <- "hired_date"
colnames(employee_data)[5] <- "termination_date"
colnames(employee_data)[13] <- "gender"
colnames(employee_data)[14] <- "termination_reason"
colnames(employee_data)[15] <- "termination_voluntary"
colnames(employee_data)[16] <- "status_year"
colnames(employee_data)[17] <- "status"
colnames(employee_data)[18] <- "department_area"

# Convert characters into factor for level indication
employee_data$city_name <- factor(employee_data$city_name)
employee_data$department_name <- factor(employee_data$department_name)
employee_data$job_title <- factor(employee_data$job_title)
employee_data$gender <- factor(employee_data$gender)
employee_data$termination_reason <- factor(employee_data$termination_reason)
employee_data$termination_voluntary <- factor(
    employee_data$termination_voluntary
)
employee_data$status <- factor(employee_data$status)
employee_data$department_area <- factor(employee_data$department_area)

# Convert date string into actual dates.
employee_data$record_date <- as.Date(
    employee_data$record_date, format = "%m/%d/%Y %H:%M"
)
employee_data$birth_date <- as.Date(
    employee_data$birth_date, format = "%m/%d/%Y"
)
employee_data$hired_date <- as.Date(
    employee_data$hired_date, format = "%m/%d/%Y"
)
employee_data$termination_date <- as.Date(
    employee_data$termination_date, format = "%m/%d/%Y"
)

# Remove unnecessary column
employee_data <- subset(
    employee_data, select = -c(
        gender_short
    )
)

# Filter out multiple record date to only the latest one.
employee_data <- employee_data %>%
    arrange(employee_id, record_date) %>%
    group_by(employee_id) %>%
    slice(n())

# Summarise the preprocessed data
summary(employee_data)

# ======================== Question 1 ===================================
# Which department has the highest retention rate?
# =======================================================================

# Extra feature 1
# This function is used to calculate the retention rate/ratio
# for every department.
ret_rate <- function(active, terminate, total) {
    if (terminate == 0) {
        return(1);
    }
    if (active == 0) {
        return(0);
    }
    return(terminate / total);
}

# Explore the data
# Check what department has the highest retention and the lowest.
employee_data %>%
    group_by(department_name) %>%
    mutate(
        active = sum(status != "TERMINATED"),
        terminated = sum(status == "TERMINATED"),
        retention_rate = ret_rate(active, terminated, n()),
    ) %>%
    summarise(
        total = n(),
        active = mean(active),
        terminated = mean(terminated),
        retention_rate = mean(retention_rate)
    ) %>%
    View

# Plot a bar chart
ggplot(q1_explore, aes(x = department_name, y = retention_rate)) +
    geom_bar(
        stat = "identity",
        fill = "lightblue",
        color = "darkblue"
    ) +
    labs(
        title = "Retention Rate by Department",
        x = "Department",
        y = "Retention Rate"
    )

# ********* Analysis 1.1 *********

# We make a new variable
# Listing all of the lowest or in this case
# a zero percent of retention rate from previous exploration.
zero_retention_department <- c(
    "Accounting",
    "Accounts Payable",
    "Accounts Receivable",
    "Audit",
    "Compensation",
    "Employee Records",
    "HR Technology",
    "Information Technology",
    "Investment",
    "Labor Relations",
    "Legal",
    "Training"
)

# We want to filter out the department with 0% retention rate
# from previous exploration. We will also only select
# specific column for easier lookup.
employee_data %>%
    filter(
        department_name %in% zero_retention_department
    ) %>%
    filter(
        status == "TERMINATED"
    ) %>%
    select(
        employee_id,
        age,
        department_name,
        termination_reason,
    ) %>%
    View

# Plot the above code into a nice scatter plot
employee_data %>%
    filter(
        department_name %in% zero_retention_department
    ) %>%
    filter(
        status == "TERMINATED"
    ) %>%
    select(
        employee_id,
        age,
        department_name,
        termination_reason,
    ) %>%
    group_by(department_name, termination_reason) %>%
    summarise(
        total = n(),
        age = median(age),
    ) %>%
    ggplot(aes(y = total, x = termination_reason, label = department_name)) +
        geom_point(
            color = "darkblue",
        ) +
        labs(
            title = "Termination Reason Total",
            x = "Termination Reason",
            y = "Total"
        ) +
        geom_label_repel(
            aes(label = department_name),
            box.padding = 0.2,
            point.padding = 0.45,
        )

# ********* Analysis 1.2 *********

# We want to check the total of each department with their own length_of_service
employee_data %>%
    filter(
        department_name %in% zero_retention_department
    ) %>%
    filter(
        status == "TERMINATED"
    ) %>%
    group_by(department_name, length_of_service) %>%
    summarise(
        total = n(),
    ) %>%
    View

# Same thing as above, but we visualize it with ggplot
# We are using another scatter plot to visualize the data
employee_data %>%
    filter(
        department_name %in% zero_retention_department
    ) %>%
    filter(
        status == "TERMINATED"
    ) %>%
    group_by(department_name, length_of_service) %>%
    summarise(
        total = n(),
    ) %>%
    ggplot(aes(y = total, x = length_of_service, label = department_name)) +
        geom_point(
            color = "darkblue",
        ) +
        labs(
            title = "Total terminated employee by length of service",
            x = "Length of Service (in Years)",
            y = "Total Employee"
        ) +
        geom_label_repel(
            aes(label = department_name),
            box.padding = 0.2,
            point.padding = 0.45,
        )

# ======================== Question 2 ===================================
# How influenced someone to take a heavy job?
# =======================================================================

# Simple snippet to view all the job
employee_data %>%
    group_by(job_title) %>%
    summarise(
        total = n(),
    ) %>%
    View

# Create a variable that fits into "Heavy" work type
heavy_work_job <- c(
    "Baker",
    "Meat Cutter",
    "Shelf Stocker"
)

# ********* Analysis 2.1 *********

# Visualize the data with every job title and gender
employee_data %>%
    filter(job_title %in% heavy_work_job) %>%
    group_by(job_title, gender) %>%
    summarise(
        total = n(),
    ) %>%
    View

# Ease of access to colour our nice graph
# This is predetermined, not a good idea
# but it's fast and I need to be fast since deadline came fast
gender_col <- c(
    "lightblue",
    "pink",
    "lightblue",
    "pink",
    "lightblue",
    "pink"
)

# Visualize the data, same thing as the above code
# that tries to visualize it via table, but we use ggplot
# We use facet_wrap to group the graph per job title
# that's why we need the above gender_col variables.
employee_data %>%
    filter(job_title %in% heavy_work_job) %>%
    group_by(job_title, gender) %>%
    summarise(
        total = n(),
    ) %>%
    ggplot(
        aes(
            y = total,
            x = gender,
            fill = factor(gender_col, labels = c("Male", "Female"))
        )
    ) +
        geom_bar(stat = "identity") +
        facet_wrap(~ job_title, ncol = 3) +
        labs(
            x = "Gender",
            y = "Count",
            title = "Total gender per job title",
            fill = "Gender"
        )

# ********* Analysis 2.2 *********

# Visualize the data
# We will categorize the age into age group for easier understanding
employee_data %>%
    filter((job_title %in% heavy_work_job) && status != "TERMINATED") %>%
    mutate(
        # age_group code are taken and adapted from (budugulo, 2020).
        age_group = case_when(
            age <= 16            ~ "0-16",
            age > 16 & age <= 30 ~ "17-30",
            age > 30 & age <= 45 ~ "31-45",
            age > 45 & age <= 60 ~ "46-60",
            age > 60             ~ "61+"
        ),
        age_group = factor(
            age_group, levels = c("0-16", "17-30", "31-45", "46-60", "61+")
        )
    ) %>%
    group_by(job_title, age_group) %>%
    summarise(
        total = n(),
    ) %>%
    ggplot(aes(x = age_group, y = total, fill = age_group)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ job_title, ncol = 3) +
        labs(
            x = "Age Group",
            y = "Count",
            title = "Total employee per age group",
            fill = "Age Group"
        ) +
        geom_text(aes(label = total), nudge_y = 10)

# ********* Analysis 2.3 *********

employee_data %>%
    filter(job_title %in% heavy_work_job) %>%
    filter(status == "TERMINATED") %>%
    mutate(
        length_group = case_when(
            length_of_service <= 5                            ~ "0-5",
            length_of_service > 5 && length_of_service <= 10  ~ "6-10",
            length_of_service > 10 && length_of_service <= 15 ~ "11-15",
            length_of_service > 15 && length_of_service <= 20 ~ "16-20",
            length_of_service > 20                            ~ "21+",
        ),
        length_group = factor(
            length_group, levels = c("0-5", "6-10", "11-15", "16-20", "21+")
        )
    ) %>%
    group_by(job_title, length_group) %>%
    summarise(
        total = n(),
    ) %>%
    ggplot(aes(x = length_group, y = total, fill = length_group)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ job_title, ncol = 3) +
        labs(
            x = "Length of Service",
            y = "Count",
            title = "Total employee per length of service",
            fill = "Length of Service"
        ) +
        geom_text(aes(label = total), nudge_y = 5)

# ======================== Question 3 ===================================
# TODO: Replace this with actual question that I want to do
# =======================================================================

# ********* Analysis 3.1 *********

# Take the length_of_service and subtract it with age
employee_with_hired_age <- employee_data %>%
    mutate(
        hired_age = age - length_of_service
    )

# Take the job title and when they are hired and group them up together
# to see the relation between the fresh grad job and their age
employee_with_hired_age %>%
    filter(hired_age <= 25) %>%
    group_by(job_title, hired_age) %>%
    summarise(
        total = n(),
    ) %>%
    ggplot(aes(x = hired_age, y = total, fill = hired_age)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ job_title, ncol = 3) +
        labs(
            x = "Hired Age",
            y = "Count",
            title = "Total employee per hired age",
            fill = "Hired Age"
        ) +
        geom_text(aes(label = total), nudge_y = 10)

# ********* Analysis 3.2 *********

# Take the job title and gender and group them up together
# to see the relation between the fresh grad job and their gender
employee_with_hired_age %>%
    filter(hired_age <= 25) %>%
    group_by(job_title, gender) %>%
    summarise(
        total = n(),
    ) %>%
    ggplot(aes(x = gender, y = total, fill = gender)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ job_title, ncol = 3) +
        labs(
            x = "Gender",
            y = "Count",
            title = "Total employee per gender",
            fill = "Gender"
        ) +
        geom_text(aes(label = total), nudge_y = 30)

# ********* Analysis 3.3 *********

# Take the job title, length of service and termination reason
# to see the relation between three of them.
# We also want to filter out to only include terminated employee
# only because we want to see how long before fres grad
# decide to resign themselves.
employee_with_hired_age %>%
    filter(hired_age <= 25) %>%
    filter(status == "TERMINATED") %>%
    group_by(job_title, length_of_service, termination_reason) %>%
    summarise(
        total = n(),
    ) %>%
    ggplot(aes(x = length_of_service, y = total)) +
        geom_bar(stat = "identity") +
        facet_grid(termination_reason ~ job_title) +
        labs(
            x = "Length of Service (in Years)",
            y = "Count",
            title = "Employee terminated by their length of service ",
            fill = "Length of Service"
        ) +
        geom_label_repel(
            aes(label = total),
            box.padding = 0.2,
            line_height = 0.5,
        )
