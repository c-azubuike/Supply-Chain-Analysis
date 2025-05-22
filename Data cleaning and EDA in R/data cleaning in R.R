library(dplyr)
library(readr)
library(janitor)
library(stringr)
library(hunspell)
library(corrplot)



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
## drop unimportant columns with high number of missing values
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

# since removing these rows will not affect the data set, they will be dropped

#######
## drop the 9 rows (from customer_lname and zipcode) with missing values
#######
df[!complete.cases(df),]
df <- df[complete.cases(df),]

sum(is.na(df))

#######
## check for duplicate rows
#######
df[duplicated(df),] # rows

#######
## verify/set that date columns to data datatype
#######
df %>% select(contains("date"))
# convert cols to datetime
df <- df %>% mutate(across(contains("date"),~ as.POSIXct(.,format="%m/%d/%Y %H:%M")))
df %>% select(contains("date"))

#######
## select char cols and check for empty strings and extra white space
#######
char_cols <- df %>% 
  select(where(is.character)) # select all the character cols

## replace empty strings with NA
char_cols <- char_cols %>% 
  mutate(across(everything(),~na_if(.,"")))

## trim unnecessary white spaces
# some col values contain different encoding
unique(char_cols$order_city) 
unique(char_cols$order_country)

## change encoding
char_cols <- char_cols %>% 
  mutate(across(everything(),~iconv(.,from="Latin1",to="UTF-8")))

unique(char_cols$order_city) 
unique(char_cols$order_country)
# trim white space
char_cols <- char_cols %>% mutate(across(everything(), ~str_trim(.)))


#######
## check for misspelled words
#######
misspelled_df <- char_cols %>% mutate(across(everything(),~hunspell_find(.)))

# since each cell of "misspelled_df" is a list, count each cell to get the number
# of misspelled words
misspelled_lengths <- misspelled_df %>% 
  mutate(across(everything(),~lengths(.)))

misspelled_summary <- misspelled_lengths %>% 
  summarize(across(everything(),sum))

## verify misspelled word in each flagged column
cat_name_mask <- which((lengths(misspelled_df$category_name) > 0))
table(as.factor(char_cols$category_name[cat_name_mask])) 
# category_name contains words with special characters, but are not misspelled

# customer_city
cust_city <- char_cols$customer_city[which(lengths(misspelled_df$customer_city) > 0)]
unique(as.factor(cust_city)) %>% sort()

# since customer city,country can contain special characters like accent markrs
# both columns will not be evaluated

# similarly, since customer first_name, last_name, email and password can contain
# special character and have different variations, these columns will be left as is


#######
## drop char columns that have no variability/ high cardinality
#######
char_col_var <- sapply(char_cols,n_distinct)
char_col_names <- names(char_cols)

sort(char_col_var,decreasing = FALSE)
var_cols <- char_col_names[char_col_var <=1 | char_col_var > 200]
char_cols <- char_cols %>% select(-all_of(var_cols))
names(char_cols) # verify

char_cols <- char_cols %>% select(-product_image) # unimportant column
names(char_cols) # verify
head(char_cols)
char_cols$customer_state <- str_to_upper(char_cols$customer_state)

#######
## convert char cols to factor
#######
char_cols <- char_cols %>% mutate(across(everything(), ~ as.factor(.)))
head(char_cols)
str(char_cols)




#######
## clean numerical cols
#######
num_cols <- df %>% select(where(is.numeric))

## check cardinality and drop cols with no variability
sapply(num_cols,n_distinct)
num_cols <- num_cols %>% select(-product_status)

#######
## check for duplicate columns
#######

## build a correlation matrix
corr_matrix <- cor(num_cols)
corrplot(corr_matrix,method="color",addCoef.col = "black",number.cex = 0.5)

diag(corr_matrix) <- NA # set diagonal to NA to remove correlation with self
which(abs(corr_matrix) == 1,arr.ind = TRUE)

# verify result using duplicated
dupes <- duplicated(t(num_cols))
num_cols[,dupes]

## drop duplicated columns/remove id columns
num_cols <- num_cols[,!dupes]

id_columns <- num_cols %>% select(contains("_id"))
num_cols <- num_cols %>% select(-names(id_columns))
names(num_cols)



#######
## combine the cleaned columns with the datetime column
#######
other_cols <- df %>% select(-where(is.character), -where(is.numeric))


df_combined <- bind_cols(char_cols,num_cols,other_cols)


#######
## Check for NA values introduced by the datetime columns
#######
sum(is.na(df_combined))
colSums(is.na(df_combined))

# drop the NA values 
df_combined <- df_combined[complete.cases(df_combined),]
sum(is.na(df_combined)) # verify changes


write_csv(df_combined,"df_cleaned.csv")

