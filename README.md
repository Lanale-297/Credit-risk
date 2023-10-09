# Lloyds bank challenge
Lloyds Banking Group is launching a new loans product and needs to determine which customers they should lend to or not
Overview:
Lloyds Banking Group is launching a new loans product and needs to determine which customers they should lend to or not. They have approached you to design this process. To ensure they are lending responsibly, it is important that the process is easy to understand and fair/treats customers fairly.
Step Up Challenge:
Lloyds Banking Group have sent you a dataset of customers who had a loan, the status of the product (default or not), as well as other credit and product information. Can you predict which customers will default on their loan and explain the key reasons why?
# **In Project**
EDA for understand customer behaviour
Removed MCAR data and Balanced the annual income distribution 
Use Grouped Bar Chart with Overlaid Line to reduce the impact of outliers and show default rate in difference group income. 
Turning the loan status to default rate which will be showed as a line graph and compare across other factors (Loan amount, employment length)
Calculated the customer affordability rate by deviding the customer income and loan amount
After using EDA as a feature selection, the logistic regression and random forest regression were built to hand classification dataset. 
Use Confusion matrix, ROC, AUC and gini coeffience to evaluate. 

Confusion matrix show

**  4312 instances were correctly predicted as "no default" (True Negatives).
** 18 instances were correctly predicted as "default" (True Positives).
** 14 instances were predicted as "default," but they actually did not default (False Positives).
** 1154 instances were predicted as "no default," but they actually defaulted (False Negatives).

Both models exhibit similar precision, recall, and f1-score for class 0 (non-default), indicating a good ability to identify instances where borrowers do not default.

Both models struggle with predicting instances of class 1 (default), with low precision, recall, and f1-score. The recall for class 1 is very low in both cases, suggesting that both models miss a significant number of actual instances of default.

The macro avg and weighted avg metrics are comparable between the two models, reflecting the similar overall performance.
Gini coeffience is applied in this case as this is imbalance dataset. Based on the result the logistic model had slight higher True Positive Rate. 

**The answer **
Key reasons for predicting loan default include:

Income-to-Ratio (inc_ratio): Customers with a higher income-to-ratio are more likely to default on their loans.

Number of Open Accounts (open_acc): Individuals with fewer open accounts are more susceptible to default.

Number of Trade Lines Opened in the Last 12 Months (num_tl_op_past_12m): Higher activity in opening trade lines in the last 12 months is associated with an increased risk of default.

Public Record Bankruptcies (pub_rec_bankruptcies): Customers with a history of public record bankruptcies are at a higher risk of default.

Housing Status (RENT, MORTGAGE, OWN): The type of housing also plays a role, with renters having a slightly higher likelihood of default compared to those with a mortgage or who own their homes.

These insights provide a comprehensive understanding of the factors influencing loan default, enabling us to make informed decisions and implement risk mitigation strategies for responsible lending practices.
