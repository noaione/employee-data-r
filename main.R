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

# The employee data
employee_data <- read.csv("employee_attrition.csv")
# Start a new window
windows()


# ======================== Data Cleaning ========================

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

# ******** Exploration ********
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

# Spawn new window, because VS Code sucks
windows()
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

# ********* Analysis 1 *********

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

# ********* Analysis 2 *********

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
