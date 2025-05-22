library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stats)
library(DescTools)

df <- read_csv("df_cleaned.csv")

glimpse(df)

## convert chars to factor
df <- df %>%
  mutate(across(where(is.character),as.factor))

#######################
##     QUESTIONS     ##
#######################

## Which 3 countries are most orders shipped to ?
top3_countries <- df %>% count(order_country,name="order_count") %>% 
  arrange(desc(order_count)) %>% head(3)
top3_countries


## Most ordered product in each of the top 3 destination countries
most_ord_prods <- df %>% 
  filter(order_country %in% top3_countries$order_country) %>% 
  group_by(order_country) %>% count(product_name, sort=TRUE) %>% 
  slice_max(n,n=1)
most_ord_prods
  
## How does payment type vary across customer segment ?
table(df$type) # since the level "Payment" is unclear, i will replace it with "Unknown"
df <- df %>% mutate(type = case_when(type == "Payment" ~ "Unknown",
                               TRUE ~ type))

top_payment_type <- df %>% 
  group_by(customer_segment,type) %>% 
  summarize(count_in_each_segment = n(),.groups = "drop_last") %>% 
  mutate(percent_used = (count_in_each_segment/sum(count_in_each_segment)) * 100) %>% 
  arrange(customer_segment,desc(percent_used)) %>% slice_max(percent_used,n=1)
top_payment_type

## which shipping mode has the highest last delivery risk ?
late_risk <- df %>% filter(late_delivery_risk == 1) %>% 
  count(shipping_mode,name = "count") %>% 
  mutate(total_sum = sum(count),percent_risk = (count/total_sum) * 100 ) %>% 
  arrange(desc(percent_risk))
late_risk
  
## How do sales profit vary by order region and market ?
profit_summary <- df %>% group_by(market,order_region) %>% 
  summarize(total_sales = sum(sales),
            total_profit = sum(benefit_per_order),.groups = "drop_last") %>% 
  mutate(market_sales = sum(total_sales),
         market_profit = sum(total_profit),
         percent_profit = (total_profit/market_profit) * 100,
         rank_in_region = rank(desc(total_profit))) %>% 
  arrange(market,rank_in_region)
profit_summary


##Are there seasonal patterns in sales when examining the order dates ?
df$order_year <- as.factor(year(df$order_date_date_orders))
df$order_month <- as.factor(month(df$order_date_date_orders, label = TRUE))

yearly_sales <- df %>% filter(!is.na(order_year)) %>% 
  group_by(order_year,order_month) %>% 
  summarize(Total_sales = sum(sales)) %>% 
  arrange(order_year,Total_sales)

top_sales_per_Yr <- yearly_sales %>% slice_max(Total_sales,n=1) %>% 
  mutate(Total_sales = scales::dollar(Total_sales))
top_sales_per_Yr


## Trend in late delivery by day of week
df$weekday <- wday(df$shipping_date_date_orders,label = TRUE)
df$shipping_delay <- df$days_for_shipping_real - df$days_for_shipment_scheduled
df$shipped_late <- ifelse(df$shipping_delay > 0,1,0)
df$delivered_late <- ifelse(df$delivery_status == "Late Delivery",1,0)
df$shipped_on_time <- ifelse(df$delivery_status == "Shipping On Time",1,0)


late_delivery_analysis <- df %>% group_by(weekday) %>% 
  summarize(total_orders = n(),
            shipped_late = sum(shipped_late, na.rm=TRUE),
            shipped_on_time = sum(shipped_on_time,na.rm=TRUE),
            late_delivery = sum(delivered_late,na.rm = TRUE),
            late_rate = mean(delivered_late, na.rm=TRUE),
            avg_shipping_delay = mean(shipping_delay[shipping_delay > 0],na.rm=TRUE)) %>% 
  arrange(desc(late_rate))
late_delivery_analysis
            

## Suspected fraud orders by order_region and country
fraud_orders <- df %>% filter(order_status == "Suspected_fraud") %>% 
  count(order_region,order_country,name="Suspected_fraud_count") %>% 
  arrange(desc(Suspected_fraud_count))


##############################
##     STATISTICAL TESTS    ##
##############################

## Do benefit_per_order (a.k.a profits) vary significantly across market?
test_1 <- summary(aov(benefit_per_order ~ market, data = df))
#--since p-value is < 0.05,difference in profit across markets is significant


## Is payment type dependent on customer segment ?
test_2 <- chisq.test(table(df$type, df$customer_segment))

test_2$expected # values are > 5 so test is reliable
# pull the p-value
test_2$p.value
# -- since p-value is 0.0014, payment type is highly dependent on customer segment

# check the strength of the association
CramerV(table(df$type,df$customer_segment)) # 0.009862739
# even though the association is statistically significant, it is extremely weak in practical terms.





## is late delivery dependent of day of week ?
test_3 <- chisq.test(table(df$weekday,df$delivered_late))

test_3$expected # values are all greated than 5

# -- most late deliveries happen on Saturdays, and since p-value = 9.44e-05
# this is statistically significant, and association with late delivery is likely
# not due to chance
CramerV(table(df$weekday, df$delivered_late)) # 0.01587152
# extremely weak practical association even though Saturdays have the highest 
# late delivery rate, the overall impact is minimal.


