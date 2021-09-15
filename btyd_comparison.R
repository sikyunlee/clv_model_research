#Load library and data
library(lubridate)
library(BTYDplus)
library(BTYD)
library(plyr)
library(reshape2)
library(ggplot2)
library(dplyr)

data <- read.table('CDNOW_master.txt', sep = "", header=FALSE, na.strings = "", stringsAsFactors = FALSE)
data_tbl <- dplyr::as_tibble(data) %>%
  dplyr::rename(cust = V1) %>%
  dplyr::mutate(date = as.Date(as.character(V2), format = '%Y%m%d')) %>%
  dplyr::select(-V2) %>%
  dplyr::rename(orders = V3) %>%
  dplyr::rename(sales = V4) %>%
  dplyr::glimpse()

#Re-transform tibble back to data.frame as BTYDplus package runs better with this format
data_df <- as.data.frame(data_tbl)

###
#Build PNBD Model
###

#Prep the data for PNBD model build
elog <- data_df[,c('cust','date','sales')]
elog <- BTYD::dc.MergeTransactionsOnSameDate(elog)
#head(elog)
#summary(elog)

#Divide data between calibration and holdout periods and print using ()
(end.of.cal.period = as.Date('1998-03-24'))
#(end.of.cal.period <- min(elog$date) + as.numeric((max(elog$date) - min(elog$date))/2))
#(end.of.cal.period <- min(elog$date) + as.numeric((max(elog$date) - min(elog$date))/1))
#Split data into calibration and holdout and make matrices
data <- BTYD::dc.ElogToCbsCbt(elog, per='week',
                              T.cal = end.of.cal.period,
                              #T.cal = max(elog$date),
                              #T.tot = max(elog$date),
                              merge.same.date = TRUE,
                              statistic = 'freq')

str(data)

#Get customer by sufficient statistic matrix with sufficient stats being:
#freq, recency and total time observed (in this case in the calibration period, half of the data periods)

#extract calibration matrix
cal2.cbs <- as.matrix(data[[1]][[1]])
str(cal2.cbs)

#Estimate parameters for Pareto Neg. Binomial Dist Model
#Estimate Parameters for the model using calibration matrix
#initial parms
(params2 <- BTYD::pnbd.EstimateParameters(cal2.cbs))

#Get log likelihood for the above parameters estimation
(LL <- BTYD::pnbd.cbs.LL(params2, cal2.cbs))

#Make series of estimates (optimization) to get params to converge to get optimal params for gamma distribution
p.matrix <- c(params2, LL)
for (i in 1:5) {
  params2 <- BTYD::pnbd.EstimateParameters(cal2.cbs, params2)
  LL <- BTYD::pnbd.cbs.LL(params2, cal2.cbs)
  p.matrix.row <- c(params2, LL)
  p.matrix <- rbind(p.matrix, p.matrix.row)
}

#get final optimzied p.matrix
p.matrix

#use final set of parameter values as params2
(params2 <- p.matrix[dim(p.matrix)[1], 1:4])

#Estimate the # of txn a new customer will make in 52 weeks
BTYD::pnbd.Expectation(params2, t=52) #results should be about X times in 52 weeks

#Calibrate and predict for all customers
x<- cal2.cbs[,'x'] #recall cal2.cbs is calibration period all cust
t.x <- cal2.cbs[,'t.x'] #recency
T.cal <- cal2.cbs[,'T.cal'] #total observed time in calibration period

#Create a customers table to append CLV values to for the results
pnbd_rdf <- data$cust.data

#Estimate txn in a T.star-long duration for customers, in this case 1 year
#recall these would differ b/c the joining period is different while the split point of time is same for all cust
pnbd_rdf$pnbd_expected_txn <- BTYD::pnbd.ConditionalExpectedTransactions(params2, T.star = 52, x, t.x, T.cal)

#get predicted CLV scores by aov * expected_txn by each customer
get_avg_sales <- dplyr::as_tibble(elog) %>%
  dplyr::group_by(cust) %>%
  dplyr::summarise(avg_sales = mean(sales)) %>%
  dplyr::glimpse()

pnbd_tbl <- dplyr::as_tibble(pnbd_rdf) %>%
  dplyr::left_join(get_avg_sales, by = c("cust" = "cust")) %>%
  dplyr::mutate(predicted_clv = pnbd_expected_txn * avg_sales) %>%
  dplyr::glimpse()

pnbd_rdf <- as.data.frame(pnbd_tbl) # OK I think this is good enough for Git post

#Estimate P(Alive) at the end of calibration period
T.cal <- 39
BTYD::pnbd.PAlive(params2, x, t.x, T.cal)

#Visualize dist. of P(ALIVE) for customers
params3 <- BTYD::pnbd.EstimateParameters(cal2.cbs)
p.alives <- BTYD::pnbd.PAlive(params3, cal2.cbs[,'x'], cal2.cbs[,'t.x'], cal2.cbs[,'T.cal'])
#Save above result to pnbd_rdf
pnbd_rdf$palive <- p.alives

ggplot2::ggplot(as.data.frame(p.alives), ggplot2::aes(x=p.alives)) +
  ggplot2::geom_histogram(color = 'grey', fill='blue') +
  ggplot2::ylab('Number of Customers') +
  ggplot2::xlab('Probability of Customer being Alive') +
  ggplot2::theme_minimal()


  #Assess how well the model does against the holdout period
  x.star <- data[[2]][[2]][,1]
  cal2.cbs <- cbind(cal2.cbs, x.star)

  holdoutdates <- attributes(data[[2]][[1]])[[2]][[2]]
  holdoutlength <- round(as.numeric(max(as.Date(holdoutdates)) - min(as.Date(holdoutdates)))/7)

  #Plot predicted vs seen conditional frequency and get matrix comp with values
  T.star <- holdoutlength
  censor <- 10 #bin all order numbers here and above
  comp <- BTYD::pnbd.PlotFreqVsConditionalExpectedFrequency(params2, T.star, cal2.cbs, x.star, censor)
  rownames(comp) <- c("actual", "expected", "bin")
  comp #It's pretty close!

#Get final output customers table
head(pnbd_rdf)

###
#Buid BG-NBD Model
###

customer_rdf <- BTYDplus::elog2cbs(data_df,
                                   unit = 'days',
                                   T.cal = as.Date('1998-03-24'),
                                   T.tot = max(data_df$date))
#Set prediction horizon to 1 year for all customers but this could change
customer_rdf$sales_avg = customer_rdf$sales / (customer_rdf$x + 1)

bgnbd_rdf <- customer_rdf
bgnbd_rdf$T.star <- 365 #T.star is the 365 day prediction horizon (same for all customers)

#Use Beta-Geometric Negative Binomial Distribution Model (BGNBD) to predict
params_bgnbd <- BTYD::bgnbd.EstimateParameters(bgnbd_rdf)
bgnbd_rdf$predicted_bgnbd <- BTYD::bgnbd.ConditionalExpectedTransactions(params = params_bgnbd,
                                                                         T.star = bgnbd_rdf$T.star,
                                                                         x = bgnbd_rdf$x,
                                                                         t.x = bgnbd_rdf$t.x,
                                                                         T.cal = bgnbd_rdf$T.cal)
#Calculate CLV by multiplying the predicted number of transactions by the average monetary value spend
bgnbd_rdf$predicted_clv <- bgnbd_rdf$sales_avg * bgnbd_rdf$predicted_bgnbd
#This multiplies the predicted_bgnbd number of predicted transaction counts by the average spend a customer will have

#Get Palive after calibration period
bgnbd_rdf$palive <- BTYD::bgnbd.PAlive(params_bgnbd, x=bgnbd_rdf$x, t.x=bgnbd_rdf$t.x, T.cal=bgnbd_rdf$T.cal)


#Visualize the CLV results
hist(bgnbd_rdf$predicted_clv, xlim = c(0,500), breaks = 3000)
plot(density(bgnbd_rdf$predicted_clv), xlim = c(0,300))

#Get final output customers table
head(bgnbd_rdf)

###
#Get Analytics based LTV using Churn rate and Total spendings by customer
###
#Get first order dates
first_order_date <- dplyr::as_tibble(data_df) %>%
  dplyr::group_by(cust) %>%
  dplyr::summarise(total_spend = sum(sales), first_order_date = min(date), last_order_date = max(date)) %>%
  dplyr::mutate(age = as.numeric(last_order_date - first_order_date)) %>%
  dplyr::mutate(cohort_year_month = as.character(paste0(lubridate::year(first_order_date),"-",lubridate::month(first_order_date)))) %>%
  dplyr::glimpse()
cohort_sum_df <- dplyr::as_tibble(data_df) %>%
  dplyr::left_join(first_order_date, by = c("cust" = "cust")) %>%
  dplyr::mutate(order_year_month = as.character(paste0(lubridate::year(first_order_date),"-",lubridate::month(first_order_date)))) %>%
  dplyr::group_by(order_year_month) %>%
  dplyr::summarise(cohort_total_spend = sum(sales), cohort_cust_count = n_distinct(cust)) %>%
  dplyr::mutate(avg_spend_by_cohort = cohort_total_spend / cohort_cust_count) %>%
  dplyr::glimpse()

#Calculate LTV based on spend/churn equation
cohort_ltv_df <- first_order_date %>%
  left_join(cohort_sum_df, by = c("cohort_year_month" = "order_year_month")) %>%
  mutate(clv = total_spend / age) %>%
  mutate(clv = if_else(clv == Inf, total_spend, clv)) %>%
  glimpse()

###
#Compare 3 Models' Performance in respect to last 1 year actual spending
###
#Get last 1 year spending for all customers
#set an all encompassing holdout period date (starting from week 39)
holdout_date <- as.Date(max(data_df$date) - (7*14))
holdout_date

holdout_spend <- dplyr::as_tibble(data_df) %>%
  filter(date > holdout_date) %>%
  group_by(cust) %>%
  summarise(total_holdout_spend = sum(sales)) %>%
  mutate(total_holdout_spend = if_else(is.na(total_holdout_spend), 0, total_holdout_spend)) %>%
  glimpse()


######
#Compare between Analytic based Cohort LTV, BGNBD, PNBD
######
compare_df <- cohort_ltv_df %>%
  select(cust, clv, total_spend) %>%
  left_join(bgnbd_rdf, by = c("cust" = "cust")) %>%
  rename(bgnbd_clv = predicted_clv) %>%
  left_join(pnbd_rdf, by = c("cust" = "cust")) %>%
  rename(pnbd_clv = predicted_clv) %>%
  select(cust, clv, total_spend, bgnbd_clv, pnbd_clv) %>%
  left_join(holdout_spend, by = c("cust" = "cust")) %>%
  mutate(total_holdout_spend = if_else(is.na(total_holdout_spend), 0, total_holdout_spend)) %>%
  mutate(clv = if_else(is.na(clv), 0, clv)) %>%
  mutate(pnbd_clv = if_else(is.na(pnbd_clv), 0, pnbd_clv)) %>%
  mutate(abs_error_cohort_clv = abs(total_spend - clv)) %>%
  mutate(abs_error_bgnbd_clv = abs(total_spend - bgnbd_clv)) %>%
  mutate(abs_error_pnbd_clv = abs(total_spend - pnbd_clv)) %>%
  mutate(sq_error_cohort_clv = (total_spend - clv)^2) %>%
  mutate(sq_error_bgnbd_clv = (total_spend - bgnbd_clv)^2) %>%
  mutate(sq_error_pnbd_clv = (total_spend - pnbd_clv)^2) %>%
  mutate(rank = rank(cust, ties.method = 'first')) %>%
  mutate(sum_sq_error_cohort_clv = cumsum(sq_error_cohort_clv)) %>%
  mutate(sum_sq_error_bgnbd_clv = cumsum(sq_error_bgnbd_clv)) %>%
  mutate(sum_sq_error_pnbd_clv = cumsum(sq_error_pnbd_clv)) %>%
  mutate(cum_sum_total_spend = cumsum(total_spend)) %>%
  mutate(cum_sum_clv = cumsum(clv)) %>%
  mutate(cum_sum_bgnbd_clv = cumsum(bgnbd_clv)) %>%
  mutate(cum_sum_pnbd_clv = cumsum(pnbd_clv)) %>%
  glimpse()

mae_result <- compare_df %>%
  summarise(mae_cohort = mean(abs_error_cohort_clv, na.rm=TRUE),
            mae_bgnbd = mean(abs_error_bgnbd_clv, na.rm=TRUE),
            mae_pnbd = mean(abs_error_pnbd_clv, na.rm=TRUE)) %>%
  glimpse()

#Plot the errors in a line plot with x-axis as cumulative # of customers and y-axis as SSE
ggplot2::ggplot(compare_df, ggplot2::aes(x=rank)) +
  ggplot2::geom_line(ggplot2::aes(y = sum_sq_error_cohort_clv), color = 'black') +
  ggplot2::geom_line(ggplot2::aes(y = sum_sq_error_bgnbd_clv), color = 'red') +
  ggplot2::geom_line(ggplot2::aes(y = sum_sq_error_pnbd_clv), color = 'blue')

ggplot2::ggplot(compare_df, ggplot2::aes(x=rank)) +
  ggplot2::geom_line(ggplot2::aes(y = log10(cum_sum_total_spend)), colour = 'green', show.legend=TRUE) +
  ggplot2::geom_line(ggplot2::aes(y = log10(cum_sum_clv)), colour = 'black', show.legend=TRUE) +
  ggplot2::geom_line(ggplot2::aes(y = log10(cum_sum_bgnbd_clv)), colour = 'red', show.legend=TRUE) +
  ggplot2::geom_line(ggplot2::aes(y = log10(cum_sum_pnbd_clv)), colour = 'blue', show.legend=TRUE) +
  ggplot2::labs(x="Cumulative # of Customers",
                y="Log Cumulative Sum of CLV") +
  ggplot2::scale_color_manual(name = "Legend", guide = "legend",
                              labels = c("log10(cum_sum_total_spend)",
                                         "log10(cum_sum_clv)",
                                         "log10(cum_sum_bgnbd_clv)",
                                         "log10(cum_sum_pnbd_clv)"))


####
#Other Utils: Evaluate w/ RMSE and MLSE --> may need editing a bit
####
### Evaluate Accuracy of PNBD Prediction Model
#RMSE
rmse <- function(expected, actual) {
  return(sqrt(mean((expected - actual)^2)))
}

#Mean Sq. Log Error
msle <- function(expected, actual) {
  return(mean((log1p(expected) - log1p(actual))^2))
}

str(cal2.cbs)
cal2.cbs[,'x']
predict <- BTYD::pnbd.ConditionalExpectedTransactions(params2, T.star = 38,
                                                      x = cal2.cbs[,'x'],
                                                      t.x = cal2.cbs[,'t.x'],
                                                      T.cal = cal2.cbs[,'T.cal'])
cal2.cbs[,'x.star'] #actual txn for each customer
rmse(actual = cal2.cbs[,'x.star'], expected = predict)
msle(actual = cal2.cbs[,'x.star'], expected = predict)

###
#Actionable Insights: Identify customers by alive/churn and high/low CLV
###
# Insight 1: Identify who is high and active CLV customer
segment <- compare_df %>%
  mutate(active = if_else(bgnbd_palive >= 0.7, 'active', 'churned')) %>%
  mutate(high_clv = if_else(percent_rank(bgnbd_clv) >= 0.7,'high', 'low')) %>%
  glimpse()

plot_seg <- segment %>%
  mutate(type = case_when(
    active == 'active' & high_clv == 'high' ~ 'active_high',
    active == 'active' & high_clv != 'high' ~ 'active_low',
    active != 'active' & high_clv == 'high' ~ 'churned_high',
    active != 'active' & high_clv != 'high' ~ 'churned_low'
  )) %>%
  glimpse()

ggplot2::ggplot(plot_seg) + ggplot2::geom_bar(ggplot2::aes(x = type, color = type)) +
  ggplot2::xlab('Types of CLV / Active Customer Segment') +
  ggplot2::ylab('Number of Customers') +
  ggplot2::ggtitle('Customers by Segments')

# Insight 2: Repeat Txn

plot_data_a <- data_df %>%
  dplyr::group_by(cust) %>%
  dplyr::summarise(aov = mean(sales),
                   num_orders = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(num_orders = as.integer(ifelse(num_orders > 10, 10, num_orders))) %>%
  dplyr::group_by(num_orders) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::collect() %>%
  dplyr::arrange(num_orders) %>%
  dplyr::mutate(pct = round(n * 100 / sum(n), 1)) %>%
  dplyr::mutate(high = cumsum(pct)) %>%
  dplyr::mutate(low = dplyr::lag(high)) %>%
  dplyr::mutate(low = tidyr::replace_na(low, 0),
                type = 'Historical') %>%
  tibble::glimpse()


fig <- plotly::plot_ly(
  data = plot_data_a,
  x = ~num_orders, y = ~pct, type = 'bar', name = 'Repeat Purchase Pct(%) Customers')
fig

ggplot2::ggplot(plot_data_a, ggplot2::aes(num_orders, pct)) +
  ggplot2::geom_col() +
  ggplot2::xlab('Number of Orders made by a Customer') +
  ggplot2::ylab('Percentage out of Total Customers') +
  ggplot2::ggtitle('Repeat Purchase Percentage of Customer Base')

# Insight 3: Acquisition by Cohort and CLV
#With cust, CLV, first order date, get cohorts table
#Using cohorts table, summarise(agg) average CLV per cohort and plot

get_cohort_date <- dplyr::as_tibble(data_df) %>%
  dplyr::group_by(cust) %>%
  dplyr::summarise(join_date = min(date)) %>%
  dplyr::mutate(cohort_date = as.character(paste0(lubridate::year(join_date),"-",lubridate::month(join_date)))) %>%
  dplyr::glimpse()

get_cust_clv_cohort_date <- compare_df %>%
  dplyr::left_join(get_cohort_date, by = c("cust" = "cust")) %>%
  dplyr::group_by(join_date) %>%
  dplyr::summarise(avg_clv = mean(bgnbd_clv), median_clv = median(bgnbd_clv), cust_cnt = n()) %>%
  dplyr::glimpse()

ggplot2::ggplot(get_cust_clv_cohort_date, ggplot2::aes(x=join_date)) +
  ggplot2::geom_line(ggplot2::aes(y=avg_clv, color = 'Avg. CLV')) +
  ggplot2::geom_line(ggplot2::aes(y=median_clv, color = 'Median CLV')) +
  ggplot2::geom_line(ggplot2::aes(y=cust_cnt, color = 'Acquired Customers')) +
  ggplot2::xlab('Cohort Join Date') +
  ggplot2::ylab('BGNBD Predicted CLV (USD)') +
  ggplot2::ggtitle('CLV of Acquired Customers over time')

# Insight 4: Scatter plot of num_orders and segment label to see about when do customers churn
num_orders <- dplyr::as_tibble(data_df) %>%
  dplyr::group_by(cust) %>%
  dplyr::summarise(num_orders = n(), avg_sales = mean(sales)) %>%
  dplyr::glimpse()

scatter_data <- plot_seg %>%
  dplyr::left_join(num_orders, by = c("cust" = "cust")) %>%
  dplyr::select(cust, bgnbd_clv, num_orders, bgnbd_palive, avg_sales, type) %>%
  dplyr::glimpse()

ggplot2::ggplot(scatter_data, ggplot2::aes(x=bgnbd_palive, y=num_orders, shape=type, alpha=type, size=type, color=type)) +
  ggplot2::geom_point() +
  ggplot2::ggtitle('Scatter Plot of Customers by CLV and Prob. Alive') +
  hrbrthemes::theme_ipsum()

ggplot2::ggplot(scatter_data, ggplot2::aes(x=avg_sales, y=bgnbd_palive, shape=type, color=type)) +
  ggplot2::geom_point() +
  ggplot2::ggtitle('Scatter Plot of Customers by Prob. Alive and Avg. Sales') +
  ggplot2::ylab('Prob. Alive') +
  ggplot2::xlab('Avg. Sales Value') +
  hrbrthemes::theme_ipsum()
