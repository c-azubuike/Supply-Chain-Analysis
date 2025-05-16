library(readr)
library(dplyr)
library(ggplot2)

df <- read_csv("df_cleaned.csv")
glimpse(df)

## convert chars to factor
df <- df %>%
  mutate(across(where(is.character),as.factor))

#######################
##     QUESTIONS     ##
#######################

## Which 3 countries are most orders shipped to ?
top3_countries <- df %>% group_by(order_country) %>% 
  summarize(order_count = n_distinct(order_id)) %>% 
  arrange(desc(order_count)) %>% head(3)

## Most ordered product in each of the top 3 destination countries
most_ord_prods <- df %>% 
  filter(order_country %in% top3_countries$order_country) %>% 
  group_by(order_country) %>% count(product_name, sort=TRUE) %>% 
  slice_max(n,n=1) %>% ungroup() #using ungroup to return the grouped object to a tibble
  
## How does payment type vary across customer segment ?
table(df$type) # since the level "Payment" is unclear, i will replace it with "Unknown"
df <- df %>% mutate(type = case_when(type == "Payment" ~ "Unknown",
                               TRUE ~ type))

top_payment_type <- df %>% 
  group_by(customer_segment,type) %>% 
  summarize(count_in_each_segment = n(),.groups = "drop_last") %>% 
  mutate(percent_used = (count_in_each_segment/sum(count_in_each_segment)) * 100) %>% 
  arrange(customer_segment,desc(percent_used)) %>% slice_max(percent_used,n=1) 

## which shipping mode has the highest last delivery risk ?
late_risk <- df %>% filter(late_delivery_risk == 1) %>% 
  count(shipping_mode,name = "count") %>% 
  mutate(total_sum = sum(count),percent_risk = (count/total_sum) * 100 ) %>% 
  arrange(desc(percent_risk))
  
## How do sales profit vary by order region and market ?
profit_summary <- df %>% group_by(market,order_region) %>% 
  summarize(total_sales = sum(sales),
            total_profit = sum(benefit_per_order),.groups = "drop_last") %>% 
  mutate(market_sales = sum(total_sales),
         market_profit = sum(total_profit),
         percent_profit = (total_profit/market_profit) * 100,
         rank_in_region = rank(desc(total_profit))) %>% 
  arrange(market,rank_in_region)











