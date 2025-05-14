library(dplyr)
library(readr)
library(janitor)
library(stringr)


## read the data
df <- read_csv("DataCoSupplyChainDataset.csv")
dim(df)
glimpse(df)
head(df)

#######
## standardize column names
######
names(df)
df <- clean_names(df)

#######
## filter for customers from the U.S
#######
table(as.factor(df$customer_country)) # counts customers from each country

df <- df %>% mutate(customer_country=
              ifelse(customer_country=="EE. UU.","USA",customer_country))

df <- df %>% filter(customer_country == "USA")
dim(df)

#######
## apply a  standard (title) format to all non-numeric columns
#######
df <- df %>% mutate(across(where(~ !is.numeric(.)),str_to_title))

#######
## check for missing values
#######
sum(is.na(df)) # total number of missing values in the entire data set
colSums(is.na(df)) # missing values per column

#######
## drop columns with high number of missing values
#######
df <- df %>% select(-c(product_description,order_zipcode))
# find the other columns with missing values
names(df)[colSums(is.na(df)) > 0]

#######
## explore missing values in customer_lname and customer_zipcode
#######
df[is.na(df$customer_lname),]
df %>% filter(is.na(customer_lname)) %>% select(customer_fname,customer_lname)
df[is.na(df$customer_zipcode),]

#######
## drop the 9 rows (from customer_lname and zipcode) with missing values
#######
df[!complete.cases(df),]
df <- df[complete.cases(df),]

#######
## check for duplicate rows/columns
#######
df[duplicated(df),] # rows

#######
## verify that date columns contain dates
#######
df %>% select(contains("date"))
# convert cols to datetime
df <- df %>% mutate(across(contains("date"),~ as.POSIXct(.,format="%m/%d/%Y %H:%M")))

#######
## select char cols and check for empty strings and extra white space
#######
char_cols <- df %>% 
  select(where(is.character)) # select all the character cols

## replace empty strings with NA
chars_cols <- char_cols %>% 
  mutate(across(everything(),~na_if(.,"")))

## trim unnecessary white spaces
# some col values contain different encoding
unique(char_cols$order_city) 
unique(char_cols$order_country)

## change encoding
char_cols <- char_cols %>% 
  mutate(across(everything(),~iconv(.x,from="Latin1",to="UTF-8")))
  
# trim white space
char_cols <- char_cols %>% mutate(across(everything(), ~str_trim(.)))

#######
## drop char columns that have no variability/ high cardinality
#######
char_col_var <- sapply(char_cols,n_distinct)
char_col_names <- names(char_cols)

sort(char_col_var,decreasing = FALSE)
var_cols <- char_col_names[char_col_var <=1 | char_col_var > 200]
char_cols <- char_cols %>% select(-all_of(var_cols))
char_cols <- char_cols %>% select(-product_image) # unimportant column

char_cols$customer_state <- str_to_upper(char_cols$customer_state)

#######
## convert char cols to factor
#######
char_cols <- char_cols %>% mutate(across(everything(), ~ as.factor(.)))

str(char_cols)



#######
## clean numerical cols
#######
num_cols <- df %>% select(where(is.numeric))
num_cols

## check for duplicate columns
dupes <- duplicated(t(num_cols))
num_cols[,dupes]

## remove duplicates
dupe_num_cols <- names(num_cols[,dupes])
num_cols <- num_cols %>% select(-all_of(dupe_num_cols))

str(num_cols)              


#######
## combine the cleaned columns
#######
other_cols <- df %>% select(-where(is.character), -where(is.numeric))

df_cleaned <- bind_cols(char_cols,num_cols,other_cols)


write_csv(df_cleaned,"df_cleaned.csv")

