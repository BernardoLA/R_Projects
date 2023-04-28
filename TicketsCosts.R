#*****************************************************# 
# Bernardo Leivas
# 26/04/2023
#
######*************Start**************##########

##Load the necessary packages
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(RColorBrewer)

###### Question 1 - automate this process ######

## Set the path to the folder containing the CSV files
path <- "C:/Users/be_al/OneDrive - UvA/Desktop/Travix/Business_Case_1/TM_datafiles"

## Create an empty data frame to store the combined data
combined_data <- data.frame()

## Create a list with all the files we need to append
csv_files <- list.files(path, pattern = "*.csv", full.names = TRUE)

## 1) This code does the following:
#  1.1) grabs each file from the csv_file list and read all but the first 4 rows.
#  1.2) it then creates a column with the file name, which is a data. To keep it organized.
#  1.3) After doing this for all files it binrows compile all files together. 
combined_data <- bind_rows(lapply(csv_files, function(x) {
  read_csv2(x, skip = 4,  na = c("")) %>% 
    mutate(Date_of_enter = str_extract(x, "(?<=/)[^/]+(?=\\.csv)"))
}))

###### Question 2 - Loss and Benefits coming from changes & cancellations ########

## assign the english names to the columns for convenience
english_names <- c("Country identifier", "Order identifier", "Departure date",	"Amount",	"Category", "Product Ticket", "number Ticket", "issuance date",	"Booking identifier",	"Route", "Airline code",	"Booking channel",	"Booking sub-channel",	"Date_of_last_segment",	"New_Changed_Cancelled", "Destination", "Date of entering")
names(combined_data) <- str_replace(english_names, " ", "_") 


## preparding the data for calculations
data_for_calculations <- combined_data %>%
  # create two different columns for the revenue and costs based on Amount
  mutate(
    revenue = if_else(str_detect(Amount, "-"), as.character(0), as.character(Amount)),
    costs = if_else(str_detect(Amount, "-"),str_replace(Amount,"-",""), as.character(0))) %>% 
  # convert to numeric the new columns
  mutate(
    revenue = as.numeric(revenue),
    costs = as.numeric(costs))


###### 2) What are the total Loss and Benefits within the period under consideration? ######
## Have to make some last adjustments.
data_type_transaction <- data_for_calculations %>% 
  select(Airline_code, New_Changed_Cancelled, revenue, costs) %>% # select necessary variables
  filter(New_Changed_Cancelled != "N") %>% # include only cancellations and changes
  summarise(revenue_sum = sum(revenue, na.rm = T)/1000000, costs_sum = -sum(costs, na.rm = T)/1000000) %>% # calculate total revenue, cost.
  mutate(total_lost = revenue_sum + costs_sum) # calculate the total loss

###### 2.1) Transform to a better format to plot the results ######
tidy_data_transaction <- tidyr::gather(data_type_transaction, key = "variable", value = "value")
tidy_data_transaction <- tidy_data_transaction %>% 
  mutate(value = round(value,2))

# 2.1.1) create the graphic with Revenues, Cost and Total Loss 
overall_balance <- ggplot(tidy_data_transaction, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red", "blue", "red"), 
                    guide = FALSE, # remove legend
                    name = "") + # remove legend title
  theme_minimal() +
  scale_y_continuous(limits = c(-5, 2.5),labels = scales::dollar_format(prefix = "$")) + # add dollar sign to y-axis
  labs(x = "", y = "") + # remove x and y axis titles
  scale_x_discrete(labels = c("Costs", "Revenue", "Final Balance")) + # change x-axis labels
  theme(axis.text.x = element_text(size = 12)) + # center title and increase x-axis label size
  guides(fill = FALSE) + # remove legend
  geom_text(aes(label = scales::dollar(value), y = ifelse(value < 0, value - 0.3, value + 0.3)), 
            position = position_dodge(width = 1), size = 3) # add value labels
  
# Export the results
ggsave("1.png", plot = overall_balance, dpi = 300, width = 8, height = 6, units = "in")

 
###### 2.2) Calculate the total costs & revenue for cancelations and changes by company and Destination ######
data_type_transaction_by_company <- data_for_calculations %>% 
  select(Airline_code, New_Changed_Cancelled, Destination, revenue, costs) %>% # select necessary variables
  filter(New_Changed_Cancelled != "N") %>% # filter for changes and cancellations
  mutate(Airline_code = if_else(is.na(Airline_code), "Unknown Company", Airline_code), # recode NAs to Unkonw for both
         Destination = if_else(is.na(Destination), "Unknown Destination", Destination)) %>% # destination and Airline name
  group_by(Airline_code) %>%
  summarise(total_revenue_per_company_type = sum(revenue, na.rm = T), # calculate the total revenue per company
            total_costs_per_company_type = sum(costs,na.rm = T)) %>% # calculate total costs per company 
  mutate(Total_Lost = round((total_costs_per_company_type - total_revenue_per_company_type)/1000,0)) %>% # calculate total loss
  arrange(desc(Total_Lost)) # arrange in descending order


# 2.2.1) replace the company codes for their actual names
path2 <- "C:/Users/be_al/OneDrive - UvA/Desktop/Travix/Business_Case_1"
company_names <- read_csv2(file.path(path2, '/Company_names.csv'))

Final_Data <- data_type_transaction_by_company %>% 
  left_join(company_names, by = c("Airline_code" = "Airline_code")) %>% # join the names based on same ID
  relocate(Airline_name, .before = Airline_code) %>% # place the names in first column
  select(-Airline_code) # We can finally see that there two drivers: Aero and Transaero


# 2.2.2) How much these two companies together correspond of the total loss
sum_top2 <- Final_Data %>% slice(1:2) %>% summarise(sum_top2 = sum(Total_Lost)) # get the total for the 2 Airlines
sum_top2/tidy_data_transaction$value[3]/10 # divide it by the total loss. of all companiesthey correspond to roughly 57%
                                           # FYI: it divides by 10 because it had to divide by a 1000 the top2 to get it in
                                           # in the same unit as the total loss. In addition, we had to multiply by 100 to make
                                           # percentage.Together both correspond to 57%.
## 2.2.3) Let's visualize these results 
top_losses <- ggplot(Final_Data %>%  slice(1:20), aes(x = reorder(Airline_name, Total_Lost), y = Total_Lost, fill = Total_Lost)) +
  geom_col() +
  geom_text(aes(label = paste0("$", Total_Lost)), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_fill_gradient(low = 'yellow', high = 'red', labels = scales::dollar_format(prefix = "$"), guide = guide_colorbar(title = NULL)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs( x = "", y = "", title = NULL) +
  theme_minimal()+
  coord_flip()
top_losses

## 2.2.4) export graph
ggsave("2.png", plot = top_losses, dpi = 300, width = 8, height = 6, units = "in")



###### 2.3) Why these two companies account for such a figure? What distinguishes them from the other companies? #######
## Could it be the number of Cancelations and Changes? It Could be: but important to notice that
## only a few are really costy. 

# 2.3.1) How much the first 200 C/C correspond to the total?
total_cost <- tidy_data_transaction$value[3]*-1 # total cost
total_cost_200 <- data_for_calculations %>% 
  left_join(company_names, by = c("Airline_code" = "Airline_code")) %>% # get names
  mutate(total_loss = costs - revenue) %>%  # calculate total loss
  select(Airline_name, Departure_date, Route, Destination, New_Changed_Cancelled, revenue, costs, total_loss) %>% # get variables
  filter(New_Changed_Cancelled != "N") %>% # filter for cancellations and changes
  arrange(desc(costs)) %>% # arrange in desc order
  slice(1:200) %>% # slice only the top 200 to look at
  summarise(sum_200 = sum(total_loss)/1000000) # calculate the total loss for the top 200

total_cost/total_cost_200*100 # ~ 86%. It is very suggestive then that almost 50 of the most expensive
# cancelations/changes goes to these two.

# 2.3.2) Let's have a look at the top 200 cancelations/changes then per company.
most_expensive_entries <- data_for_calculations %>% 
  left_join(company_names, by = c("Airline_code" = "Airline_code")) %>% # join company names
  mutate(total_loss = costs - revenue) %>% # calculate total loss per row
  select(Airline_name, Departure_date, Route, Destination, New_Changed_Cancelled, revenue, costs, total_loss) %>%
  filter(New_Changed_Cancelled != "N") %>% # select only changes and cancellations 
  arrange(desc(total_loss)) %>% # sort in descending order
  slice(1:200) %>% 
  group_by(Airline_name) %>% # group by airline
  summarize(counts_per_airline = n()) %>% # count how many times each airline had Cancel. or Chan.
  mutate(percentage_counts = round(counts_per_airline/sum(counts_per_airline),2)) %>% 
  arrange(desc(percentage_counts))
most_expensive_entries


# 2.3.3) Let's visualize this results for a better overview across the top 20 Airline Companies.
top_entries <- ggplot(most_expensive_entries %>% slice(1:20), aes(x = percentage_counts, y = reorder(Airline_name, counts_per_airline), fill = counts_per_airline)) +
  geom_col() + 
  theme_minimal() +
  guides(fill = FALSE) +
  scale_x_continuous(limits = c(0.00,0.25), labels = scales::percent_format()) + # adjust scale
  labs(x = NULL, y = NULL) + # remove x and y axis titles
  geom_text(aes(label = scales::percent(percentage_counts), x = percentage_counts, y = Airline_name), hjust = -0.1) +
  scale_fill_gradientn(colors = c("#E5F5FF", "#CCE5FF", "#B2D4FF", "#99C4FF", "#7FB4FF", "#66A3FF", "#4C93FF", "#3383FF", "#1972FF", "#0062E5")) # specify custom color palette with inverted hues
top_entries

ggsave("4.png", plot = top_entries, dpi = 300, width = 8, height = 6, units = "in")



# However, it begs the question:
# why these are more expensive for these countries?                              
###### 2.4) Could it be that these companies are flying to most expensive destinations? ######

## 2.4.1) First of all, what are the most expansive destinations?
costy_destinations <- data_for_calculations %>% 
  left_join(company_names, by = c("Airline_code" = "Airline_code")) %>% # get airline names
  mutate(total_loss = costs - revenue,
         Destination = if_else(is.na(Destination),"Unknown Destination", Destination)) %>% # recode unknown Destinations
  select(Airline_name, Departure_date, Route, Destination, New_Changed_Cancelled, revenue, costs, total_loss) %>% # select variables
  filter(New_Changed_Cancelled != "N") %>% # get only C/C
  arrange(desc(costs)) %>% # arrange in descending order
  slice(1:200) %>% # get only the top 200
  group_by(Destination) %>% # group by destination
  summarise(loss_per_destination = round(sum(total_loss)/1000,2)) %>% # calculate the loss per destination
  arrange(desc(loss_per_destination))

## 2.4.2) Let's visualize these results for the top 10 most expensive destinations
## create custom color palette with shades of blue
my_palette <- colorRampPalette(brewer.pal(n = 9, name = "Blues"))(10)


top_20_destinations <- ggplot(costy_destinations %>% slice(1:20), aes(x = loss_per_destination, y = reorder(Destination, loss_per_destination), fill = loss_per_destination)) +
  geom_col() + 
  theme_minimal() +
  guides(fill = FALSE) +
  scale_fill_gradientn(colors = my_palette, na.value = "grey50") + # use custom color palette
  labs(x = NULL, y = NULL) + # remove x and y axis titles
  geom_text(aes(label = paste0("$", round(loss_per_destination))), nudge_x = 20) + # add labels near bars
  scale_x_continuous(labels = function(x) paste0("$", x)) # add dollar sign to x-axis labels

## We can clearly see 3 major destinations JFK, BKK, HKT.
ggsave("5.png", plot = top_20_destinations, dpi = 300, width = 8, height = 6, units = "in")

  
###### 2.5) Finally, are Aeroflotand Transaero having more cancellations from the top costly destinations?######
# The difference from the costy_destinations data is that here we group the 200 costly C/C by
# the combination Airline and Destination.
# This data is merged later.

company_destinations <- data_for_calculations %>%  
  left_join(company_names, by = c("Airline_code" = "Airline_code")) %>%
  mutate(total_loss = costs - revenue,
         Destination = if_else(is.na(Destination),"Unknown Destination", Destination)) %>% 
  select(Airline_name, Departure_date, Route, Destination, New_Changed_Cancelled, revenue, costs, total_loss) %>%
  filter(New_Changed_Cancelled != "N") %>%
  arrange(desc(costs)) %>% 
  slice(1:200) %>% 
  group_by(Airline_name, Destination) %>% 
  summarise(loss_per_airline_destination = sum(total_loss)) %>% 
  arrange(desc(loss_per_airline_destination))

## 2.5.1) For plot clarity we will look at only the top 10 destinations
top_10_destinations <- costy_destinations$Destination[1:10]

## 2.5.2) filter for the top 20 spenders in the top 10 destinations (among the top 200 costy C/C, 86% of the total cost)
top_20 <- Final_Data %>% 
  slice(1:20) %>% # get the 20 companies with the highest losses
  select(Airline_name) %>% # keep only the airline id
  left_join(company_destinations, by = c("Airline_name"="Airline_name")) %>% # get the company costs per destination with the data we just created
  filter(Destination %in% top_10_destinations) %>% # filter for the top_10 Destinations
  mutate(loss_per_airline_destination = loss_per_airline_destination/1000) %>% # get the units right for the plot
  group_by(Airline_name) %>% # The next two groupings are for sorting in the plot
  mutate(sorting = sum(loss_per_airline_destination)) %>% # we use it to sort per Airline costs in the y-axis
  ungroup() %>% 
  group_by(Destination) %>% 
  mutate(sorting_legend = sum(loss_per_airline_destination)) %>% # we use it to sort per Destination in the legend
  arrange(desc(sorting_legend))
  
## 2.5.3) Again we visualize the results
top_plot <- ggplot(top_20, aes(x = loss_per_airline_destination, y = reorder(Airline_name, sorting))) +
  geom_col(aes(fill = Destination)) +
  labs(x = NULL, y = NULL) +
  scale_fill_discrete(limits = top_20$Destination[order(-top_20$sorting_legend)]) +    
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal()
  
ggsave("3.png", plot = top_plot, dpi = 300, width = 8, height = 6, units = "in")  
  
######*************END**************########