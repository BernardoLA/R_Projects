###****************###
# PF Concept
# Bernardo Leivas
# Business Case
# 24/05/2023
###***************###

###### load the data & Data formatting ######

## Load necessary packages

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

#*** Get the path and easily change the backslash into forwardslash with this code below!
# # set the path
# path <- readClipboard()
# 
# # change the backslash to forward slash
# path <- str_replace_all(path,"\\\\", "/")
#***

path <- "C:/Users/be_al/OneDrive - UvA/Desktop/PF_Concept/Assignment"
# get data
main_data <- read_csv2(file.path(path, "imported_data.csv")) 

# adjust class of variables

# to factor columns
to_factor <- c("invoice_year_month", "debtor_number", "order_no", "country_manager_code", "debtor_segment", "tier1_description", "tier2_description")

# to numeric columns
to_numeric <- c("eur_cost_amount", "eur_sales_amount")

main_data[to_factor] <- lapply(main_data[to_factor], factor)
main_data[to_numeric] <- lapply(main_data[to_numeric], as.numeric)

# change the levels of the invoice month 
main_data$invoice_year_month <- factor(main_data$invoice_year_month,labels = c("Jan","Feb", "Mar", "Apr", "May", "Jun"))

# Change order of levels and labels in main_data$debtor_segment
main_data$debtor_segment <- relevel(main_data$debtor_segment, ref = "Small")

# Create a profit column
main_data <- main_data %>% 
  mutate(profit = eur_sales_amount - eur_cost_amount)

# check the recoding
str(main_data)


###### Calculations ###### 

#####*** 1) What was total results in the entire period? ***#####


# total sales, costs, profit
data_aggr <- main_data %>%
  filter(!is.na(eur_sales_amount)) %>% # remove the observations that we don't have information about sales or costs 
  select(eur_sales_amount, eur_cost_amount, profit) %>% 
  summarise(total_sales = sum(eur_sales_amount), 
            total_costs = sum(eur_cost_amount ),
            total_profit = sum(profit))

# get the data in tidy format to visualize it
tidy_data_aggr <- tidyr::gather(data_aggr, key = "type", value = "value")

# make as factor to put in the correct order in the plot
tidy_data_aggr$type <- factor(tidy_data_aggr$type, levels = c("total_sales", "total_costs", "total_profit"))

# make the the negative value for the plot
tidy_data_aggr_plot <- tidy_data_aggr %>% 
  mutate(value = round(ifelse(type=="total_costs", -value, value)/1000000,2)) # plot in millions of euros and round to 2 decimals

# create the plot
overall_balance <- ggplot(tidy_data_aggr_plot, aes(x = type, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("??? ", value), y = ifelse(type == "total_costs", value -0.2, value + 0.2))) +
  scale_fill_manual(values = c("blue", "red", "blue"),
                    guide = "none",
                    name = "") +
  scale_x_discrete(labels = c("Sales", "Costs", "Profit")) +
  labs(y = "", x = "", title = "Sales, Costs and Profit realized in the entire period (in millions of euros).") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
overall_balance
# save the plot
ggsave("aggr_results.png", plot = overall_balance)

#####***2) What was the total result by Country ***#####

# total sales, costs, profit per country
data_aggr <- main_data %>%
  filter(!is.na(eur_sales_amount)) %>% # remove the observations that we don't have information about sales or costs 
  select(country_manager_code, eur_sales_amount, eur_cost_amount, profit) %>%
  group_by(country_manager_code) %>% # group by country the results
  summarise(total_sales = sum(eur_sales_amount), 
            total_costs = sum(eur_cost_amount ),
            total_profit = sum(profit))

# get the data in tidy format to visualize it
tidy_data_aggr <- data_aggr %>% 
  pivot_longer(cols = c("total_sales", "total_costs", "total_profit"), names_to = "type", values_to = "value")

# make as factor to put in the correct order in the plot
tidy_data_aggr$type <- factor(tidy_data_aggr$type, levels = c("total_sales", "total_costs", "total_profit"))

# make the the negative value for the plot
tidy_data_aggr_plot <- tidy_data_aggr %>% 
  mutate(value = round(ifelse(type=="total_costs", -value, value)/1000000,2)) # plot in millions of euros and round to 2 decimals


# create a plot per country
balance_country <- ggplot(tidy_data_aggr_plot, aes(x = type, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("??? ", value), y = ifelse(type == "total_costs", value - 0.1, value + 0.1))) +
  scale_fill_manual(values = c("blue", "red", "blue"),
                    guide = "none",
                    name = "") +
  scale_x_discrete(labels = c("Sales", "Costs", "Profit")) +
  # theme_minimal() +
  labs(y = "", x = "", title = "Sales, Costs and Profit realized in the entire period per country (in millions of euros).") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ country_manager_code) 
  

# save results
ggsave("country_results.png", plot = balance_country, width = 10)


#####*** 3) Were there differences within the period for each country? ***#####

# Profit across the period per country
data_aggr <- main_data %>%
  filter(!is.na(eur_sales_amount)) %>% # remove the observations that we don't have information about sales or costs 
  group_by(country_manager_code, invoice_year_month) %>% # group by country and month the results
  summarise( total_profit = round(sum(profit)/1000,0))


# create a plot per country and period
balance_country_year <- ggplot(data_aggr, aes(x = invoice_year_month, y = total_profit, fill = country_manager_code)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0("??? ", total_profit), y = total_profit + 0.2, group = country_manager_code), position = position_dodge(width = 1), vjust = -0.5, size = 5) +
  labs(y = "", 
       x = "",
       fill = "Countries",
       title = "Sales, Costs, and Profit realized per month and per country (in thousands of euros).") +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = guide_legend(title = "Countries", label.theme = element_text(size = 12), title.theme = element_text(size = 14))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14))


# save the graph
ggsave("country_month_results.png", balance_country_year, width = 10)


#####*** 4) Is one segment more profitable than the others ***#####

# 4.1) Get profit per segment and country
data_aggr <- main_data %>%
  filter(!is.na(eur_sales_amount)) %>% # remove the observations that we don't have information about sales or costs 
  group_by(country_manager_code, debtor_segment) %>% # group by country and month the results
  summarise( total_profit = round(sum(profit)/1000,0))

# get number of orders per segment and country
data_aggr_n <- main_data %>%
  filter(!is.na(eur_sales_amount)) %>% # remove the observations that we don't have information about sales or costs 
  group_by(country_manager_code, debtor_segment) %>% # group by country and month the results
  summarise(counts = n())

# create a plot per country and segment - Profit
balance_country_segment <- ggplot(data_aggr, aes(x = debtor_segment, y = total_profit, fill = country_manager_code)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0("??? ", total_profit), y = total_profit + 0.2, group = country_manager_code), position = position_dodge(width = 1), vjust = -0.5, size = 5) +
  labs(y = "", 
       x = "",
       fill = "Countries",
       title = " Profit realized per segment and per country (in thousands of euros).") +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = guide_legend(title = "Countries", label.theme = element_text(size = 16), title.theme = element_text(size = 18))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18))
balance_country_segment

# save plot
ggsave("country_segment_results.png", balance_country_segment, width = 10)

# create a plot per country and segment - Counts
balance_country_segment <- ggplot(data_aggr_n, aes(x = debtor_segment, y = counts, fill = country_manager_code)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = counts, y = counts + 0.2, group = country_manager_code), position = position_dodge(width = 1), vjust = -0.5, size = 5) +
  labs(y = "", 
       x = "",
       fill = "Countries",
       title = "Number of orders placed per segment and per country.") +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = guide_legend(title = "Countries", label.theme = element_text(size = 16), title.theme = element_text(size = 18))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18))
balance_country_segment
# save plot
ggsave("country_segment_results_COUNTS.png", balance_country_segment, width = 10)


## 4.2)create a plot with the distribution of profit per segment
# the area under the curve of the density plot sums up to one 1, 
# reason why the y scale is unusual and not in %. 
dist_seg <- ggplot(main_data %>% filter(profit < 1000 & profit > -1000), 
       aes(x = profit, 
           fill = debtor_segment)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of profit made per order.",
       x = "",
       y = "") +
  facet_wrap(~country_manager_code, ncol = 1) +
  guides(fill = guide_legend(title = "Segments", label.theme = element_text(size = 16), title.theme = element_text(size = 18))) +
  # scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_x_continuous(labels = scales :: dollar_format(prefix = "???")) +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18))

# save the plot
ggsave("dist_segments.png",dist_seg, width = 10)

# 4.3) Plot with the outliers
out_data <- main_data %>% 
  filter(profit > 10000 & debtor_segment == "Medium") %>% 
  group_by(country_manager_code) %>% 
  summarise(profit = sum(profit))

#visualize the results
out_profit <- ggplot(out_data, aes(x = country_manager_code, y = profit, fill = country_manager_code)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_text(aes(label = paste0("??? ", round(profit,0)), vjust = -0.5),size = 6) +
  labs(title = "Profit coming from outliers (orders > ??? 10,000) for Medium segment.",
       x = "",
       y = "") +
  guides(fill = "none") +
  theme_minimal() +
  scale_y_continuous(labels = scales :: dollar_format(prefix = "??? ")) +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18, margin = margin(b = 30))) 
  out_profit
# save plot
ggsave("out_profit.png", out_profit, width = 10)

# save results

#####*** 5) Are there differences between Tier 1 products ***#####

# Get profit per tier 1 products  and country
data_aggr <- main_data %>%
  filter(!is.na(eur_sales_amount)) %>% # remove the observations that we don't have information about sales or costs 
  group_by(country_manager_code, tier1_description) %>% # group by country and month the results
  summarise( total_profit = round(sum(profit)/1000,0))

# create a plot per country and segment
balance_country_tier1 <- ggplot(data_aggr, aes(x = reorder(tier1_description, total_profit), y = total_profit, fill = country_manager_code)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = total_profit), position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Countries", label.theme = element_text(size = 16), title.theme = element_text(size = 18))) +
  scale_y_continuous(labels = function(x) paste0("??? ", x)) + # Add Euro sign to x-axis labels
  labs(y = "", 
       x = "",
       fill = "Countries",
       title = " Profit realized across Tier 1 categories per country (in thousands of euros).") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  coord_flip() 
balance_country_tier1
#save the plot
ggsave("country_tier1_results.png", balance_country_tier1, width = 12)

#####***6) Why are non-product category more profitable than others?***#####

## 6.1) Do they have a profit per order that is on average higher?
data_aggr <- main_data %>%
  filter(!is.na(eur_sales_amount)) %>% # remove the observations that we don't have information about sales or costs 
  group_by(tier1_description, country_manager_code) %>% # group by country and month the results
  summarise(avg_margin = round(mean(profit),0),
            total_sum = round(sum(order_line_quantity)/1000,0),
            total_n = n())

# create plot for profit margin
balance_margin<- ggplot(data_aggr, aes(x = reorder(tier1_description, avg_margin), y = avg_margin, fill = country_manager_code)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = paste0("???", avg_margin)), position = position_stack(vjust = 0.5), size = 5) +
  guides(fill = guide_legend(title = "Countries", label.theme = element_text(size = 16), title.theme = element_text(size = 18))) +
  scale_y_continuous(labels = function(x) paste0("??? ", x)) + # Add Euro sign to x-axis labels
  labs(y = "", 
       x = "",
       fill = "Countries",
       title = "Average profit per order across Tier 1 categories per country.") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  coord_flip() 
balance_margin
# save plot
ggsave("balance_margin.png", balance_margin, width = 10)

# ANSWER: The difference does not seem to come from a higher profit margin. 
# Even though it's relative high, it cannot explain the position as top1
# in terms of profit. specially for CBE1 and CNO1, so it has to be the number of sales.

## 6.2) Is it the numbers of orders?
balance_n <- ggplot(data_aggr, aes(x = reorder(tier1_description, total_n), y = total_n, fill = country_manager_code)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = total_n), position = position_stack(vjust = 0.5)) +
  # scale_y_continuous(labels = function(x) paste0("??? ", x)) + # Add Euro sign to x-axis labels
  guides(fill = guide_legend(title = "Countries", label.theme = element_text(size = 16), title.theme = element_text(size = 18))) +
  labs(y = "", 
       x = "",
       fill = "Countries",
       title = "Total number of orders per Tier 1 categories and country in the period.") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  coord_flip()

# save plot
ggsave("balance_n.png", balance_n, dpi = 300, width = 10, height = 6, units = "in")

## 6.3) And are these orders also high volume orders?
balance_volume <- ggplot(data_aggr, aes(x = reorder(tier1_description, total_sum), y = total_sum, fill = country_manager_code)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = total_sum), position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Countries", label.theme = element_text(size = 16), title.theme = element_text(size = 18))) +
  labs(y = "", 
       x = "",
       fill = "Countries",
       title = "Total volume ordered (divided by 1000) per tier 1 categories and country in the period.") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()
# save plot
ggsave("balance_volume.png", balance_volume , dpi = 300, width = 10, height = 6)
# ANSWER: As expected the top 5 profit makers are also in the top 5 of volume sold,
# even though there are some small differences ranking-wise.
# this indicates that 
corr_profit_qty <- lm(data = main_data, profit ~ order_line_quantity)
summary(corr_profit_qty)
## there is indeed a high correlation, high-volume orders and higher profit! 

#####*** 7) Lastly, let's then zoom in into this top 5 tiers and check how tier 2 products are performing ***#####

# 7.1) Calculation per Tier 1 categories
# vector with top 5 tier 1 categories to be selected
tier1_categories <- c("NON-PRODUCT","HG GIFTS", "BAGS","HG LIFESTYLE", "DRINKWARE")

# create a function to plot the results per Tier 1 categor
myfunction2 <- function(filter){
  # get the data 
  data_aggr <- main_data %>%
    filter(!is.na(eur_sales_amount) & tier1_description %in% c("NON-PRODUCT","HG GIFTS", "BAGS","HG LIFESTYLE", "DRINKWARE")) %>% # remove the observations that we don't have information about sales or costs 
    group_by(tier1_description,tier2_description, country_manager_code) %>% # group by country and month the results
    summarise(total_sum = round(sum(order_line_quantity)/1000,0)) %>%
    ungroup() %>% #join with the filter variable 
    # right_join(filter_data, by = "tier2_description") %>%  # join to filter the data and select only tear 2 sales a above 100 (thousand)
  filter(tier1_description == filter)
  
  # visualize results
  balance_tier2_volume <- ggplot(data_aggr, aes(x = reorder(tier2_description, total_sum), y = total_sum, fill = country_manager_code)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    geom_text(aes(label = total_sum), position = position_stack(vjust = 0.5)) +
    labs(y = "", 
         x = "",
         fill = "Countries",
         title = paste0("Tier 1 Category -",filter,"\nTotal quantity ordered (divided by 1000) per tier 2 products (among top 5 tier 1 categories) and country in the period.")) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          plot.title = element_text(hjust = 0.5)) +
    coord_flip()
  
  #save plot
  ggsave(paste0(filter,"balance_tier2_volume.png"), balance_tier2_volume, dpi = 300, width = 8, height = 6, units = "in")  
}

# apply the function to each tier 1 category
lapply(tier1_categories, myfunction2)

# Calculate the aggregate value
# create a filter to remove tier 2 categories very short on sales
# this filter variable is used only for the aggregate plot
# right below it.
filter_data <- main_data %>%
  filter(!is.na(eur_sales_amount) & tier1_description %in% c("NON-PRODUCT","HG GIFTS", "BAGS","HG LIFESTYLE", "DRINKWARE")) %>% # look into the tier 2 categories that are part of the top 5 tier 1 categories
  group_by(tier2_description) %>% # group by country and month the results
  summarise(counts = n()) %>% 
  filter(counts > 99) # filter for tier 2 categories with at least 100 orders placed in the period.
                      # This is just to make the plot clearer!

# 7.1) Calculation aggregated
# get the data 
data_aggr <- main_data %>%
  filter(!is.na(eur_sales_amount) & tier1_description %in% c("NON-PRODUCT","HG GIFTS", "BAGS","HG LIFESTYLE", "DRINKWARE")) %>% # remove the observations that we don't have information about sales or costs 
  group_by(tier1_description,tier2_description, country_manager_code) %>% # group by country and month the results
  summarise(profit_per_order = round(mean(profit),0),
            profit = round(sum(profit)/1000),0,
            counts = n()) %>%
  ungroup() %>%  #join with the filter variable 
  right_join(filter_data, by = "tier2_description") %>%
  mutate(counts = counts.x) %>% 
  select(tier1_description, tier2_description, country_manager_code, profit, counts, profit_per_order)


# Create function to create the plots for Counts and Profit per Tier 2 category
  # visualize results - Profit
balance_tier2_profit <- ggplot(data_aggr, aes(x = reorder(tier2_description, profit), y = profit, fill = country_manager_code)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = profit), position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Countries", label.theme = element_text(size = 16), title.theme = element_text(size = 18))) +
  labs(y = "", 
       x = "",
       fill = "Countries",
       title = paste0("Profit per Tier 2 categories (in thousands of euros).")) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  scale_y_continuous(labels = function(x) paste0("??? ",x)) + 
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()
  

  #save plot
  ggsave("profit_balance_tier2.png", balance_tier2_profit, width = 12)  
  
# visualize results - Counts
balance_tier2_counts <- ggplot(data_aggr, aes(x = reorder(tier2_description, counts), y = counts, fill = country_manager_code)) +    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = counts), position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Countries", label.theme = element_text(size = 16), title.theme = element_text(size = 18))) +
  labs(y = "", 
        x = "",
        fill = "Countries",
       title = paste0("Total number of orders per Tier 2 categories and country in the period.")) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  # scale_y_continuous(labels = function(x) paste0("??? ",x)) + 
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()
  
balance_tier2_counts
#save plot
ggsave("counts_balance_tier2.png", balance_tier2_counts, width = 12) 
  
# 7.3) Average profit made per group
# visualize results - Average Profit
balance_tier2_profit_per_order <- ggplot(data_aggr, aes(x = reorder(tier2_description, profit_per_order), y = profit_per_order, fill = country_manager_code)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = paste0("???", profit_per_order)), position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Countries", label.theme = element_text(size = 16), title.theme = element_text(size = 18))) +
  labs(y = "", 
       x = "",
       fill = "Countries",
       title = paste0("Average profit per Tier 2 categories .")) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  scale_y_continuous(labels = function(x) paste0("??? ",x)) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

balance_tier2_profit_per_order 
#save plot
ggsave("balance_tier2_profit_per_order.png", balance_tier2_profit_per_order, width = 12)  

#####***8) Lastly,Who are the top 10 accounts and the bottom 10 accounts. They are different across countries  ***#####

## 8.1) Create general function to set the data per Country

# 8.1.1) create a vector with the countries
country_names <- c("CBE1", "CES1", "CNO1")

# 8.1.2) Create the function: interested in the saved graphs for top10 an bottom 10
myfunction <- function(country){
  
# get the top 10 accounts
top10 <- main_data %>%
  filter(country_manager_code == country & !is.na(eur_sales_amount)) %>% 
  group_by(debtor_number, 
           country_manager_code) %>%
  summarise(profit = round(sum(profit)/1000,0)) %>% 
  arrange(desc(profit)) %>% 
  ungroup() %>% # ungroup the data
  slice(1:10)

# vizualize the results for top 10
top10_plot <- ggplot(top10, aes(x = reorder(debtor_number, profit), y = profit)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste0("??? ",profit)), 
            hjust = -0.2, 
            size = 5) +  # Adjust the Hjust parameter to position the labels above the bars
  theme_minimal() +
  labs(x = "", y = "", title = paste0("Top 10 customers with highest profit in ", country, " (in thousands of euros).")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  coord_flip()


# save top 10 plot
ggsave(paste0("top10_",country,".png"), top10_plot, width = 12)


# get the bottom_10 accounts
bottom_10 <- main_data %>%
  filter(country_manager_code == country & !is.na(eur_sales_amount)) %>% 
  group_by(debtor_number, 
           country_manager_code) %>%
  summarise(profit = round(sum(profit),0)) %>% 
  arrange(profit) %>% 
  ungroup() %>% # ungroup the data
  slice(1:10)

bottom10_plot <- ggplot(bottom_10, aes(x = reorder(debtor_number, desc(profit)), y = profit)) +
  geom_bar(stat = "identity", aes(fill = factor(profit > 0))) +
  geom_text(aes(label = paste0("??? ", profit)), 
            size = 5, 
            position = position_dodge(width = 0.5),
            hjust = ifelse(bottom_10$profit > 0, -0.2, 1.1),
            color = "black") +
  scale_fill_manual(values = c("red", "blue"), guide = "none") +
  theme_minimal() +
  labs(x = "", y = "", title = paste0("Bottom 10 customers with lowest profit in ", country, " (rounded to no decimals).")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  coord_flip()


# save bottom 10 plot
ggsave(paste0("bottom10_",country,".png"), bottom10_plot, width = 12)

}

# 8.1.3) Apply function to each country
lapply(country_names, myfunction)


