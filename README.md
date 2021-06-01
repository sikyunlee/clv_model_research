# clv_model_research
## Goal: Building BTYD models and comparing with traditional Cohort LTV predictions and deriving actionable insights from the results

### Introduction
For decades, the BTYD (Buy Til' You Die) model has been released through whitepapers, ranging from BG-NBD (Beta Gamma Negative Binomial Distribution), PNBD (Pareto Negative Binomial Distribution) and up to the latest model being the PGGG (Pareto Gamma Gamma Gamma) model. In this repo, the goal is to build the popular model (BGNBD) and the gold standard model (PNBD) model to compare with the traditional Cohort-based CLV estimating model. Additionally, some way to deliver actionable insights is provided at the end.

### The Models
The BTYD Family models assume that customers that are in non-contractual situations will have their own unique purchasing signal and would eventually die off (i.e.: churn or lapse) from purchasing. To model these signals, the popular model - BGNBD assumes that the customers' lifetime of purchasing follows a Beta Gamma distribution while the frequency of purchasing is based on a Negative Binomial Distribution. These parameters are estimated according to each customer's purchasing signals across the entire customer base to create a model that predicts: 
  (i) expected number of transaction (purchases) a customer will make
  (ii) probability that a customer will be alive (not lapsed and still purchasing) at a certain period of time
  (iii) using their average purchasing value, multiply this by their expected number of transaction to get their expected Customer Lifetime Value (CLV) for a certain duration of a period. 
  
For the purpose of research and simple model building, all models were assumed to predict only 1 year of CLV. PNBD model was assumed simiarly as well but did use a calibration (training) and holdout (validation) period breaks to tune the model. 

For the Cohort-based model, a few assumptions were made:
 - Customers that join at a certain period (e.g.: date, month, year, etc.) is under "a cohort group." 
 - These customers that join (i.e.: start to purchase) will have similar buying behaviors among others in the same cohort group
 - Cohort-based CLV can be calculated by the average spend of all historical transactions by all the customers acquired in a certain period

### Results
Both BG-NBD and PNBD have a much lower level of error (Squared Sum of Error) compared to the cohort-based model. In fact, BG-NBD and PNBD are pretty close to each other when looking at the aggregate results.

As seen in error comparison in respect to the actuals, BG-NBD and PNBD comes much closer to the actual 1-year holdout period spend compared to the cohort-based estimates. BG-NBD and PNBD could be further improved with the following:
 - Have a calibration/holdout period to train and validate the model (for BG-NBD, this was not done for the sake of quickly/simply building the model) 
 - Larger (or longer) data that includes more transactions from the customers. Arguably, BTYD models have a weakness when not enough data (or not enough transaction) is available from a customer's transaction history. Havign more data could improve the accuracy of the model.

### Conclusion
It makes much more sense to employ the BTYD model compared to traditional ways of calculating CLV (some call it LTV) as it shows a lower error rate even when the model is built very roughly and quickly as these ones. Some of these applications as shown in the insights section of the code and visualizations are:
 - Identify which customers are high/low-CLV customers and who are active/inactive customers to segment customers into groups for targeted campaigns.
 - Look at repeat purchase behavior to see the lifecycle of the customer (and the product if information is given) to see a product-market fit.
 - Look at mean and median CLV across cohorts (e.g.: by year or year and months) to see how customers acquired in those cohorts differ from other cohorts in terms of mean and median CLV. This helps to look at if their acquisition is getting better or not.
 - Look at some behaviors of the customers in terms of number of purchases and their labels (e.g.: high/active CLV customers, etc.) This helps to see what differentiates a high and low CLV customer and/or what's the difference between active and inactive customer (i.e.: churned/lapsed) in terms of buying patterns. 
