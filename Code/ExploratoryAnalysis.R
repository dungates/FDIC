library(readr)
library(purrr)
library(tidyr)
library(tidyverse)
library(lubridate)

df <- read_csv("/Users/dunk/FDIC/ACdata.csv")

# Variable Name	Source	Description	
# FIPS_county		BEA		Federal Information Processing Standard (FIPS) code identifying the state and county (or county equivalent) of the observation										
# year				Year of the observation, ranging from 1994 to 2018										
# cert		Call		Certificate number identifying a financial institution										
# county_state		BEA		Name of the state and county in which the observation is located										
# state				Name of the state in which the observation is located										
# FIPS_state				FIPS code identifying the state in which the observation is located										
# pop		BEA		Population of the given county-year										
# bankname		Call		Name of the financial institution										
# cpi_2018		BLS		The Consumer Price Index for urban consumers, scaled so that the CPI in 2018 is equal to 100										
# deposits		SOD		Deposits reported by the financial institution in the given county-year										
# branch_count		SOD		Number of physical locations of the financial institution in the given county-year										
# cb		FDIC		=1 indicates that a financial institution meets the FDIC definition of a community bank in the given year, =0 indicates that a financial institution does not meet this definition										
# asset		Call		Total amount of all bank assets (loans, securities, etc.)										
# all_loans		Call		Dollar amount of all loans currently outstanding from financial institution										
# equity		Call		Dollar amount of financial institution equity capital										
# real_estate_loans		Call		Dollar amount of all loans from financial institution secured by real estate										
# ag_loans		Call		Dollar amount of all loans from financial institution to finance agricultural production or other loans to farmers										
# ci_loans		Call		Dollar amount of all loans from financial institution for commercial and industrial purposes										
# consumer_loans		Call		Dollar amount of all loans from financial institution to individuals for household, family, or other personal expenditures										
# credit_card_loans		Call		Dollar amount of all loans from financial institution made via credit cards										
# na_asset		Call		Dollar amount of all loans currently outstanding from financial institution with no payments made in the last 90 days										
# na_real_estate_loans		Call		Dollar amount of all loans from financial institution secured by real estate with no payments made in the last 90 days										
# na_ag_loans		Call		Dollar amount of all loans from financial institution to finance agricultural production or other loans to farmers with no payments made in the last 90 days										
# na_ci_loans		Call		Dollar amount of all loans from financial institution for commercial and industrial purposes with no payments made in the last 90 days										
# na_consumer_loans		Call		Dollar amount of all loans from financial institution to individuals for household, family, or other personal expenditures with no payments made in the last 90 days										
# na_credit_card_loans		Call		Dollar amount of all loans from financial institution made via credit cards with no payments made in the last 90 days										
# dollarloans_nonfarm_bus_LT1M		Call		Dollar amount of all loans originated at under $1 million from financial institution secured by nonfarm, nonresidential properties										
# dollarloans_ci_LT1M		Call		Dollar amount of all loans originated at under $1 million from financial institution for commercial and industrial purposes										
# dollarloans_ag_bus_LT500K		Call		Dollar amount of all loans originated at under $500,000 from financial institution to finance agricultural production										
# numloans_nonfarm_bus_LT1M		Call		Number of all loans originated at under $1 million from financial institution secured by nonfarm, nonresidential properties										
# numloans_ci_LT1M		Call		Number of all loans originated at under $1 million from financial institution for commercial and industrial purposes										
# numloans_ag_bus_LT500K		Call		Number of all loans originated at under $500,000 from financial institution to finance agricultural production										
# percap_pers_inc		BEA		Dollar amount of all income received from labor, land, and capital, including income transfers, divided by the county population										
# jobs		BEA		Number of individuals employed in county, including all self-employed individuals and excluding unpaid workers										
# farm_inc		BEA		Dollar amount of all income received by individuals and partnerships that operate farms, excluding income received by corporate farms										
# farm_jobs		BEA		Number of individuals who are owners of farms										
# nonfarm_inc		BEA		Dollar amount of income received by nonfarm business owners										
# nonfarm_jobs		BEA		Number of individuals who are owners of nonfarm establishments										
# owner_inc		BEA		Dollar amount of income received by all business owners										
# owner_jobs		BEA		Number of individuals who are owners of farm or nonfarm 

# Initial plot of all variables as histogram
df %>% keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram()
boxplot(df)

# Initial yearly plot
df %>% keep(is.numeric) %>% 
  gather(key, value, -year) %>%
  ggplot(aes(as.Date(as.character(year), format = "%Y"), value)) + geom_line() + facet_wrap(~ key, scales = "free") + scale_x_date()
ggsave("/Users/dunk/FDIC/ExploratoryPlot.png", width = 30, height = 30, units = "cm")

# Regression using dummy variable
# options("na.action" = "na.exclude")
summary(lm(real_estate_loans ~ pop * cb, data = df))

ggplot(df, aes(x = pop, y = deposits)) + geom_point() + facet_wrap( ~ state, scales = "free_x")

df %>% group_by()




