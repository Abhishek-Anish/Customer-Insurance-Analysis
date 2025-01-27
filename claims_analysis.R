library(tidyverse)

claims_df <- read.csv("claims.csv")

head(claims_df)


#_________________________________________________________________________________________________
# Q1
# What impact does vehicle class have on the profitability of customers?
df1<-claims_df%>% group_by(vehicle_class)%>% summarise(n_customers=n(),
                                                       Average_CLV = mean(customer_lifetime_value),
                                                       Median_CLV = median(customer_lifetime_value),
                                                       pct_P=mean(customer_lifetime_value>0))

print(df1)

# This code adjusts the figure output size in the notebook
options(repr.plot.width=11, repr.plot.height=8)
ggplot(df1, aes(x = vehicle_class, y = Average_CLV, fill = vehicle_class)) +
  geom_col() +
  labs(title = "Average Customer Lifetime Value by Vehicle Class",
       x = "Vehicle Class",
       y = "Average Customer Lifetime Value",
       fill="Vehicle Class")+theme_light()
#_________________________________________________________________________________________________

# Q2
# How does the total claim amount vary according to the type of residence?

(df2 <- claims_df %>%
    group_by(residence_type) %>%
    summarise(n_customers=n(),
              Profit_Ratio = mean(customer_lifetime_value>0),
              Avg_claims_amount=mean(total_claims_amount),
              Min_claims_amount=min(total_claims_amount),
              Max_claims_amount=max(total_claims_amount)))

options(repr.plot.width=11, repr.plot.height=8)
ggplot(claims_df,aes(x=residence_type,y=total_claims_amount,fill=residence_type))+
  geom_boxplot()+
  labs(title = "Boxplot of Total Claims amount by Residence type",
       x = "Type of residence",
       y = "Total Claims Amount",
       fill="Residence Type") +coord_flip()
#_________________________________________________________________________________________________

# Q3
# How does the distribution of monthly premiums differ among Corporate, Personal, 
# and Special policyholders, and to what extent do customers across these policy types 
# opt for premiums at or below the $100 mark?

ggplot(claims_df, aes(x = monthly_premium)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  facet_wrap(~policy)+
  labs(title = "Histogram of Monthly Premiums by each Policy ",
       x = "Monthly Premium",
       y = "Frequency") +
  theme_minimal()

(df3<- claims_df %>% group_by(policy) %>% summarise(n_customers=n(),
                                                    avg_monthly_premium = mean(monthly_premium),
                                                    max_monthly_premium = max(monthly_premium),
                                                    sd_monthly_premium = sd(monthly_premium),
                                                    pct_less_100 = mean(monthly_premium <= 100)))
#_________________________________________________________________________________________________

# Q4 
# Are there any geographical patterns in number of claims made and profitability?
(df4<- claims_df %>% group_by(customer_state,total_claims)%>% 
    summarise(Avg_CLV_Loss = mean(customer_lifetime_value<0),
              n_customers = n(),
              Median_CLV = median(customer_lifetime_value),
              Average_CLV = mean(customer_lifetime_value))) %>%  
  arrange(desc(Avg_CLV_Loss))

ggplot(df4,aes(x=customer_state,y=total_claims,fill=Avg_CLV_Loss))+
  geom_tile()+
  scale_fill_viridis_b()+
  labs(title = "Number of Claims vs Customer States",
       subtitle="Loss ratio",
       x="Number of Claims",
       y="States",
       fill="Loss Ratio")

#_________________________________________________________________________________________________

# Q5
# Is there a strong correlation between income and monthly premium paid? 
# How does it vary across the different marital status and gender? 

(df5<- claims_df %>% group_by(marital_status,gender)%>%
    summarise(avg_monthly_premium = mean(monthly_premium),
              n_customers=n(),
              median_monthly_premium=median(monthly_premium),
              max_monthly_premium = max(monthly_premium),
              min_monthly_premium=min(monthly_premium))%>%
    arrange(desc(avg_monthly_premium)))

ggplot(claims_df, aes(x = income, y = monthly_premium, color=gender)) +
  geom_point() +
  facet_wrap(~ marital_status, nrow=3)+
  labs(title = "Income vs. Monthly Premium",
       subtitle="Considering Marital status and gender",
       x="Income",
       y="Monthly Premium",
       color="Gender")+coord_flip()

#_________________________________________________________________________________________________
# Q6
# What is the relationship between customer state and highest education level in determining 
# the average duration of policies active?

(df6 <- claims_df %>%
    group_by(customer_state, highest_education) %>%
    summarise(avg_months_policy_active = mean(months_policy_active)) %>%
    arrange(desc(avg_months_policy_active)))

ggplot(df6, aes(x = customer_state, y = highest_education, fill = avg_months_policy_active)) +
  geom_tile() +
  scale_fill_viridis_b() +
  labs(title = "Average Months Policy Active by State and Education Level",
       x = "Customer State",
       y = "Highest Education",
       fill = "Avg. Months Policy Active") +
  theme_minimal()

#_________________________________________________________________________________________________
# Q7
# How do different customer acquisition channels impact customer value and claim behavior?
(df7 <- claims_df %>%
    group_by(sales_channel) %>%
    summarise(Average_CLV = mean(customer_lifetime_value),
              Median_CLV = median(customer_lifetime_value),
              Average_Claims = mean(total_claims),
              Average_Claim_Amount = mean(current_claim_amount),
              Median_Claim_Amount = median(current_claim_amount),
              n = n()) %>% arrange(desc(Average_CLV)))

ggplot(df7, aes(x = sales_channel, y = Average_CLV)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Customer Lifetime Value by Sales Channel",
       x = "Sales Channel",
       y = "Average Customer Lifetime Value")

#_________________________________________________________________________________________________

# Q8
# Is there a correlation between customer retention and profitability across all the policy tiers 
# offered?
ggplot(claims_df, aes(x = months_policy_active, y = customer_lifetime_value,color=coverage)) +
  geom_point() +
  facet_wrap(~coverage)+
  labs(title = "Customer Retention vs. Profitability",
       x = "Months Policy Active",
       y = "Customer Lifetime Value",
       color="Policy Tiers")
