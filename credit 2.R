install.packages("mice")
library(mice)
install.packages("caret") 
install.packages("ISLR") 
install.packages("tidyverse") 
install.packages("plotrix") 
install.packages("dplyr")

library(caret)
library(ISLR)
library(tidyverse)
library(plotrix)
library(dplyr)

##handling missing data
imputed_loan <- mice(Data_Set, method = "cart")
loan <- complete(imputed_loan)
loan
 
##convert data
m <- c(
       '< 1 year' = 0,
       '1 year' = 1,
       '2 years' = 2,
       '3 years' = 3,
       '4 years' = 4,
       '5 years' = 5,
       '6 years' = 6,
       '7 years' = 7,
       '8 years' = 8,
       '9 years' = 9,
       '10+ years' = 10)
Data_Set$emp_length <- m[as.character(Data_Set$emp_length)]

##handling outliers
loan$annual_inc_log <- log(loan$annual_inc)
data$total_bal_ex_mort_log <-log(data$total_bal_ex_mort)
## chart installment

ggplot(data, aes(x = loan_status, y = installment)) +
  geom_boxplot(fill = "#336699", alpha = 0.6) +
  labs(x = "Loan Status", y = "Instalment", title = " The installment of charge off and fully paid") +
  theme_bw()
## purpose

ggplot(data, aes(x = purpose, y = loan_amnt, fill = loan_status)) + geom_boxplot() + labs(title = "Distribution of Loan Amounts by Loan Purpose", x = "Loan Purpose", y = "Loan Amount") + theme_bw()

ggplot(data, aes(x = purpose, y = loan_amnt, fill = loan_status)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Loan Amounts by Loan Purpose", x = "Loan Purpose", y = "Loan Amount") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
## interest rate
ggplot(data, aes(x = loan_status, y = rate)) + geom_boxplot(fill = "#336699") + labs(x = "Loan Status", y = "Interest Rate", title = " Distribution of interest rate by loan status")

ggplot(data, aes(x = loan_amnt, y = rate, color = loan_status)) +
  geom_point() +
  labs(x = "Loan Amount", y = "Interest Rate", title = "Relationship between Loan Amount and Interest Rate by Loan Status") +
  theme_bw()
ggplot(data, aes(x = term, y = rate, color = loan_status)) +
  geom_boxplot(fill = "#336699", alpha = 0.6) +
  labs(x = "Loan Term", y = "Interest Rate", title = "Relationship between Loan Term and Interest Rate by Loan Status") +
  theme_bw()
## term
ggplot(data, aes(x = term, y = loan_amnt, fill = loan_status)) +
  geom_boxplot() +
  labs(title = "Distribution of Loan Amounts by Loan Status and Term", x = "Loan Term", y = "Loan Amount", fill = "Loan status") +
  theme_bw()

##avg_cur_bal

ggplot(data, aes(x = avg_cur_bal, y = loan_amnt, color = loan_status)) +
  geom_point(alpha = 0.5, shape = 16) +
  labs(x = "Average Current Balance", y = "Loan Amount", title = "Relationship between Loan Status, Average Current Balance, and Loan Amount") +
  facet_wrap(~loan_status) +
  scale_x_continuous(labels = scales::comma) +
  theme_bw()
## morgate account vs home and loan amount
library(ggplot2)

ggplot(data, aes(x = home_ownership, y = mort_acc, fill = loan_status)) +
  geom_col(position = "dodge") +
  labs(x = "Home Ownership", y = "Number of Mortgage Accounts", title = "Relationship between Home Ownership, Number of Mortgage Accounts, and Loan Status") +
  theme_bw()

ggplot(data, aes(x = loan_amnt, y = mort_acc, color = loan_status)) +
  geom_point(alpha = 0.5, size = 1.5) +
  labs(x = "Number of Mortgage Accounts", y = "Loan Amount", title = "Relationship between Mortgage Accounts, Loan Amount, and Loan Status") +
  theme_bw()
ggplot(data = data, aes(x = mort_acc, y = loan_amnt, fill = loan_status)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  scale_x_continuous(breaks = seq(0, max(data$mort_acc), by = 1)) +
  labs(x = "Number of Mortgage Accounts", y = "Loan Amount (USD)", 
       title = "Average Loan Amount by Number of Mortgage Accounts and Loan Status") +
  theme_bw()
## open acc with mean loan amount
  # Create a new column for grouping open_acc values
  data$open_acc_group <- cut(data$open_acc, breaks = c(0, 5, 10, 15, 20, 30, Inf), 
                             labels = c("1-5", "6-10", "11-15", "16-20", "21-30", "31+"))
  
  # Calculate the mean loan amount for each group and loan status
  mean_loan_amount <- aggregate(loan_amnt ~ open_acc_group + loan_status, data, mean)
  
  # Plot the relationship using a grouped bar chart
ggplot(mean_loan_amount, aes(x = open_acc_group, y = loan_amnt, fill = loan_status)) +
    geom_col(position = "dodge") +
    labs(x = "Number of Open Credit Lines", y = "Mean Loan Amount (USD)",title = "Relationship between Loan Amount, number of open credit lines and loan status", fill = "Loan Status") +
    scale_x_discrete(labels = c("1-5", "6-10", "11-15", "16-20", "21-30", "31+")) +
    scale_fill_manual(values = c("#FFCC66", "#3399FF")) +
    theme_minimal()
##open acc and annual income


library(ggplot2)

# Create a new column for grouping open_acc values
data$open_acc_group <- cut(data$open_acc, breaks = c(0, 5, 10, 15, 20, 30, Inf), 
                           labels = c("1-5", "6-10", "11-15", "16-20", "21-30", "31+"))

# Calculate the mean annual_inc_log for each group and loan status
mean_annual_inc_log <- aggregate(annual_inc_log ~ open_acc_group + loan_status, data, mean)

# Plot the relationship using a grouped bar chart
ggplot(mean_annual_inc_log, aes(x = open_acc_group, y = annual_inc_log, fill = loan_status)) +
  geom_col(position = "dodge") +
  labs(x = "Number of Open Credit Lines", y = "Mean Log Annual Income",title = "Annual income and number of open credit lines by loan status", fill = "Loan Status") +
  scale_x_discrete(labels = c("1-5", "6-10", "11-15", "16-20", "21-30", "31+")) +
  scale_fill_manual(values = c("#FFCC66", "#3399FF")) +
  theme_minimal()


##total_acc and emp_length
  
ggplot(data, aes(x = emp_length, y = loan_amnt, fill = loan_status)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#00BFFF", "#ADD8E6")) +
  labs(x = "Employment Length", y = "Loan Amount", fill = "Loan Status", title = "Loan Amount by Employment Length and Loan Status") +
  theme_minimal()
ggplot(data, aes(x = emp_length, y = loan_amnt, fill = loan_status)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#00BFFF", "#ADD8E6")) +
  labs(x = "Employment Length", y = "Loan Amount", fill = "Loan Status", title = "Loan Amount by Employment Length and Loan Status") +
  theme_minimal()

ggplot(data, aes(x = emp_length, y = loan_amnt, fill = loan_status)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Employment Length", y = "Loan Amount", fill = "Loan Status", title = "Loan Amount by Employment Length and Loan Status") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 10, 1))
   ggplot(data, aes(x = emp_length, fill = loan_status)) + geom_bar(position = "dodge") + labs(x = "Employment Status", y = "Proportion of loans", title = " Employment Status and Loan Status") + scale_fill_manual(values = c("#0072B2", "#E69F00")) + theme_classic() + scale_x_continuous(breaks = seq(0, 10, 1))
##months delinq
   library(ggplot2)
   
   ggplot(loan, aes(y = mths_since_last_delinq, x = annual_inc_log, color = loan_status)) +
     geom_point() +
     labs(x = "Months Since Last Delinquency", y = "Log of Annual Income",
          title = "Relationship between Mths_since_last_delinq, Annual_inc_log, and Loan Status") +
     scale_color_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   
   library(ggplot2)
   
   ggplot(loan, aes(x = loan_status, y = mths_since_last_delinq, fill = loan_status)) +
     geom_violin() +
     labs(x = "Loan Status", y = "Months Since Last Delinquency",
          title = "Relationship between Mths_since_last_delinq and Loan Status") +
     scale_fill_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   library(ggplot2)
   
   ggplot(loan, aes(x = loan_status, y = mths_since_last_delinq, fill = loan_status)) +
     geom_boxplot() +
     labs(x = "Loan Status", y = "Months Since Last Delinquency",
          title = "Relationship between Mths_since_last_delinq and Loan Status") +
     scale_fill_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
  
   library(ggplot2)
   
   ggplot(loan, aes(x = mths_since_last_delinq, y = installment, color = loan_status)) +
     geom_point() +
     labs(x = "Months Since Last Delinquency", y = "Installment",
          title = "Relationship between Mths_since_last_delinq and Installment by Loan Status") +
     scale_color_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   library(ggplot2)
   
   ggplot(loan, aes(x = mths_since_last_delinq, y = loan_amnt, color = loan_status)) +
     geom_point() +
     labs(x = "Months Since Last Delinquency", y = "Loan Amount",
          title = "Relationship between Mths_since_last_delinq and Loan Amount by Loan Status") +
     scale_color_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   
   library(ggplot2)
   
   ggplot(loan, aes(y = mths_since_last_delinq, x = loan_amnt, color = loan_status)) +
     geom_point(alpha = 0.5) +
     facet_wrap(~loan_status, ncol = 2) +
     labs(x = "Months Since Last Delinquency", y = "Loan Amount",
          title = "Relationship between Mths_since_last_delinq and Loan Amount by Loan Status") +
     scale_color_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   library(ggplot2)
   
   ggplot(loan, aes(y = mths_since_last_delinq, x = avg_cur_bal, color = loan_status)) +
     geom_point(alpha = 0.5) +
     labs(x = "Months Since Last Delinquency", y = "Average Current Balance",
          title = "Relationship between Mths_since_last_delinq and Avg_cur_bal by Loan Status") +
     scale_color_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   library(ggplot2)
   
   ggplot(loan, aes(x = loan_status, y = mths_since_last_delinq, fill = loan_status)) +
     geom_bar(stat = "summary", fun = "sum") +
     labs(x = "Loan Status", y = "Sum of Months Since Last Delinquency",
          title = "Relationship between Mths_since_last_delinq and Loan Status") +
     scale_fill_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   
   library(ggplot2)
   
   ggplot(loan, aes(x = loan_status, y = mths_since_last_delinq, fill = loan_status)) +
     geom_bar(stat = "summary", fun = "length") +
     labs(x = "Loan Status", y = "Count of Observations",
          title = "Relationship between Mths_since_last_delinq and Loan Status") +
     scale_fill_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   
   library(dplyr)
   library(ggplot2)
   
   loan_summary <- loan %>%
     group_by(loan_status) %>%
     summarize(mean_mths_since_last_delinq = mean(mths_since_last_delinq, na.rm = TRUE),
               mean_loan_amount = mean(loan_amnt, na.rm = TRUE))
   
   ggplot(loan_summary, aes(x = loan_status)) +
     geom_bar(aes(y = mean_mths_since_last_delinq, fill = "Mths_since_last_delinq"), 
              stat = "identity", position = "dodge", alpha = 0.7) +
     geom_bar(aes(y = mean_loan_amount, fill = "Loan_amount"), 
              stat = "identity", position = "dodge", alpha = 0.7) +
     scale_fill_manual(name = "", 
                       values = c("Mths_since_last_delinq" = "#0072B2", "Loan_amount" = "#E69F00")) +
     labs(x = "Loan Status", y = "Mean Value", 
          title = "Relationship between Mths_since_last_delinq and Loan Amount by Loan Status") +
     theme_classic()
   ggplot(loan, aes(x = mths_since_last_delinq, y = loan_amnt)) +
     geom_point(aes(color = loan_status), alpha = 0.5) +
     stat_summary(aes(color = loan_status), fun = mean, geom = "line", size = 1) +
     labs(x = "Months since last delinquency", y = "Loan Amount",
          title = "Relationship between Mean Loan Amount and Months Since Last Delinquency") +
     facet_wrap(~ loan_status, nrow = 2) +
     scale_color_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   ggplot(loan, aes(x = loan_status, y = mths_since_last_delinq, fill = loan_status)) +
     geom_bar(stat = "summary", fun = "mean") +
     labs(x = "Loan Status", y = "Mean of Mths_since_last_delinq",
          title = "Relationship between Mths_since_last_delinq and Loan Status") +
     scale_fill_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   
   ggplot(loan,aes(x = mths_since_last_delinq, fill = loan_status)) +
     geom_bar(position = "dodge") +
     scale_fill_manual(values = c("#0072B2", "#E69F00", "#56B4E9")) +
     labs(title = "Proportion of Loans by Mths_since_last_delinq and Loan Status",
          x = "Mths_since_last_delinq",
          y = "Proportion of Loans") +
     theme_classic()
   ggplot(loan, aes(x = mths_since_last_delinq, fill = loan_status)) +
     geom_bar() +
     labs(x = "Months Since Last Delinquency", y = "Count",
          title = "Loan Status by Months Since Last Delinquency") +
     scale_fill_manual(values = c("#0072B2", "#E69F00", "#56B4E9")) +
     theme_classic()
   
   ggplot(loan, aes(x = loan_status, y = mths_since_last_delinq, fill = loan_status)) +
     geom_bar(stat = "mean", fun = "length", position = "dodge") +
     labs(x = "Loan Status", y = "Count of Observations",
          title = "Relationship between Mths_since_last_delinq and Loan Status") +
     scale_fill_manual(values = c("#0072B2", "#E69F00")) +
     theme_classic()
   library(dplyr)
   library(ggplot2)
   
   loan %>%
    
     ggplot(aes(x = mths_since_last_delinq, fill = loan_status)) +
     geom_bar(position = "fill") +
     scale_fill_manual(values = c("#0072B2", "#E69F00", "#56B4E9")) +
     labs(title = "Proportion of Loans by Mths_since_last_delinq and Loan Status",
          x = "Mths_since_last_delinq",
          y = "Proportion of Loans") +
     theme_classic()
   
##logistic

   
   
   
   loan$loan_status <-ifelse(loan$loan_status == 'Fully Paid',1,0)
   
   
   
   set.seed(123)
   trainIndex <- createDataPartition(loan$loan_status, p = 0.7, list = FALSE)
   trainData <- loan[trainIndex,]
   testData <- loan[-trainIndex,]
   
   # Specify the logistic regression model
   logistic_model <- glm(loan_status ~ annual_inc_log+term+int_rate+pub_rec_bankruptcies+installment+avg_cur_bal+num_tl_op_past_12m+mort_acc, data=trainData, family=binomial)
   logistic_model <- glm(loan_status ~., data=trainData, family=binomial)
   
   # Fit the model
   summary(logistic_model)
   
   # Predict on the testing set
   predicted <- predict(logistic_model, testData, type="response")
   
   # Evaluate the performance of the model
   library(pROC)
   roc(testData$loan_status, predicted)
view(logistic_model)


# Evaluate the model performance using AUC
library(pROC)
roc(testData$loan_status, rf_pred)


ggplot(data1, aes(x = num_tl_op_past_12m, y = annual_inc_log, color = loan_status)) +
  geom_point() +
  labs(x = "Number of accounts opened in past 12 months", y = "Log of Annual Income",
       title = "Relationship between Num_tl_op_past_12m, Annual_inc_log, and Loan Status") +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  theme_classic()
library(dplyr)

data1_summary <- data1 %>%
  group_by(loan_status) %>%
  summarize(avg_num_tl_op_past_12m = mean(num_tl_op_past_12m),
            avg_annual_inc_log = mean(annual_inc_log))

ggplot(data1_summary, aes(x = loan_status, y = avg_num_tl_op_past_12m, fill = loan_status)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(avg_num_tl_op_past_12m, 1)), vjust = -0.5, color = "white", size = 4) +
  labs(x = "Loan Status", y = "Average Number of Accounts Opened in Past 12 Months",
       title = "Relationship between Num_tl_op_past_12m, Annual_inc_log, and Loan Status") +
  theme_classic() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"))

ggplot(data1_summary, aes(x = loan_status, y = avg_annual_inc_log, fill = loan_status)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(avg_annual_inc_log, 1)), vjust = -0.5, color = "white", size = 4) +
  labs(x = "Loan Status", y = "Average Log of Annual Income",
       title = "Relationship between Num_tl_op_past_12m, Annual_inc_log, and Loan Status") +
  theme_classic() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"))

library(ggplot2)
library(dplyr)

credit1_summary <-data1 %>%
  group_by(num_tl_op_past_12m, loan_status) %>%
  summarize(avg_annual_inc_log = mean(annual_inc_log))

ggplot(credit1_summary, aes(x = num_tl_op_past_12m, y = avg_annual_inc_log, fill = loan_status)) +
  geom_bar(position = position_dodge(width = 1), stat = "identity") +
  labs(x = "Number of accounts opened in past 12 months", y = "Log of Annual Income",
       title = "Relationship between Num_tl_op_past_12m, Annual_inc_log, and Loan Status") +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 50, 1))
# Create income ranges
income_ranges <- cut(credit$annual_inc_log, breaks = seq(7, 12, by = 0.5))

# Create a summary data frame
loan_summary <- data1 %>%
  group_by(loan_status, annual_inc_log) %>%
  summarize(n = n())

# Plot the bar chart
ggplot(loan_summary, aes(x = annual_inc_log, y = n, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loan Status by Income Range",
       x = "Log of Annual Income",
       y = "Count of Loans",
       fill = "Loan Status") +
  theme_classic()
ggplot(data = loan, aes(x = emp_length, fill = pub_rec_bankruptcies > 0)) +
  geom_bar(position = "fill") +
  labs(x = "Employment Length", y = "Proportion of Loans with Public Record Bankruptcies") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  ggtitle("Relationship between Employment Length and Public Record Bankruptcies")library(ggplot2)

ggplot(data = loan, aes(x = emp_length, y = pub_rec_bankruptcies)) +
  geom_boxplot() +
  labs(x = "Employment Length", y = "Public Record Bankruptcies") +
  ggtitle("Relationship between Employment Length and Public Record Bankruptcies")
library(ggplot2)

# Aggregate data by emp_length, pub_rec_bankruptcies, and loan_status
agg_data <- trainData %>%
  group_by(emp_length, pub_rec_bankruptcies, loan_status) %>%
  summarize(n = n()) %>%
  mutate(prop = n / sum(n))

# Create a grouped bar plot
ggplot(agg_data, aes(x = emp_length, y = prop, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ pub_rec_bankruptcies) +
  xlab("Employment Length") +
  ylab("Proportion of Loans") +
  ggtitle("Relationship between pub_rec_bankruptcies, emp_length, and loan_status")


