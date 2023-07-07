
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(broom)

#####***** CASE 2 *****#####
# get data path
path <- "C:/Users/be_al/OneDrive - UvA/Desktop/Travix/Business_Case_2/files_to_import"

# create list with all files
csv_files <- list.files(path, "*.csv", full.names = TRUE)

# use lapply to read all files and save them as separate datasets
all_datasets <- lapply(csv_files, function(file) {
  # read csv file
  dataset <- read.csv2(file)
  
  # convert the columns to numeric
  dataset[,8:26] <- lapply(dataset[,8:26],as.numeric)
  # change group to factor
  dataset$airporttax <- as.numeric(dataset$AirportTax)
  
  # change group to factor
  dataset$period <- as.factor(dataset$Groups)
  dataset$CreditCardTypeName <- as.factor(dataset$CreditCardTypeName)
  
  #drop unnecessary variables
  dataset <- dataset %>% 
              select(-c(Groups,AirportTax)) 
    
  # change names to lower case
  names(dataset) <- tolower(colnames(dataset))
  
  # extract filename without extension to use as dataset name
  name <- tools::file_path_sans_ext(basename(file))
  
  # assign dataset to a variable with the name extracted from filename
  assign(name, dataset, envir = .GlobalEnv)
  
  # return dataset
  dataset
  
})

##### Since all Channels will be using the same steps Create a function #######
## General function 
models <- function(data,name){
  
  #first model with no controls
  # change dependent variables to payment fees to check it 
  # original models were with profit margin online.cm
  model1 <- lm(payment.fees ~ period  , data = data)
 
  # second model with controls
  # change dependent variables to payment fees to check it 
  # original models were with profit margin online.cm
  model2 <- lm(payment.fees ~ period + creditcardtypename + airporttax + hotel.margin +	car.margin + overrides + earnings.on.segments + insurance.margin + tg.margin + seo.costs + consolidator.costs  + tp + tax
               , data = data)
  
  ### Right below this code chuncK there is a quicker and neater way using the tidy function from the broom package ###
  # extract coefficients and confidence intervals for period predictor
  # coef_data <- summary(model2)$coefficients[c("period2", "period3", "period4"), c("Estimate", "Std. Error")]
  # conf_int <- confint(model2)[c("period2", "period3", "period4"), ]
  # 
  # # combine coefficients and confidence intervals into a single data frame
  # coef_df <- data.frame(coef = coef_data[,1],
  #                       conf_low = conf_int[,1],
  #                       conf_high = conf_int[,2],
  #                       period = c("period2", "period3", "period4"))
  # *********************************************************************** ##  
  
  # Extract coefficients and confidence intervals for period predictor
  coef_df <- tidy(model2, conf.int = TRUE) %>%
    filter(term %in% c("period2", "period3", "period4")) %>%
    select(term, estimate, conf.low, conf.high) %>%
    rename(coef = estimate, conf_low = conf.low, conf_high = conf.high) %>%
    mutate(period = term)

  # create coefficient plot
  plot_coef <- ggplot(coef_df, aes(x = period, y = coef)) +
    geom_point(fill = "steelblue", size = 3) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
    labs(x = "", y = "", title = paste0(name,"Channel - Regression coefficients net of covariates. Period 1 is the base category.")) +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_discrete(labels = c("Period 2", "Period 3", "Period 4")) +
    ylim(c(-30,10)) +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
    )
  
  # save the plot 
  ggsave(paste0(name,"_coef.png"), plot = plot_coef, path = path, width = 10, height = 7, units = "in")
  
  ## Models without outliers
  # Calculate z-scores
  z_scores <- abs(scale(data$online.cm))
  
  # Identify outliers
  outliers <- z_scores > 2
  
  # Subset the data to remove outliers
  data <- data[!outliers,]
  
  #first model with no controls
  model3 <- lm(payment.fees ~ period  , data = data)
  
  # second model with controls
  model4 <- lm(payment.fees ~ period + creditcardtypename + airporttax + hotel.margin +	car.margin + overrides + earnings.on.segments + insurance.margin + tg.margin + seo.costs + consolidator.costs  + tp + tax
               , data = data)
  return(list(plot_coef, summary(model1), summary(model2), summary(model3), summary(model4)))
   
}


## Now run the function for all datasets
# Direct
datasets <- list(direct, meta, skyscanner)
names <- c("Direct", "Meta", "Skyscanner")

for (i in seq_along(datasets)) {
  # Call models() with current dataset and print name
  output_list <- models(datasets[[i]], paste0(names[i], " "))
  cat("Results for",names[i], "dataset:\n")
  
  # Extract first and second element of output_list and print
  # Normal model with no controls
  first_result <- output_list[[2]]
  cat("First model summary:\n")
  print(first_result)
  cat("\n")
  
  # Normal Model with controls
  second_result <- output_list[[3]]
  cat("Second model summary:\n")
  print(second_result)
  cat("\n")
  
  # Model with no outilers and no controls
  third_result <- output_list[[4]]
  cat("Third model summary - first model with no outliers:\n")
  print(third_result)
  cat("\n")
  
  # Model with no outliers and controls
  fourth_result <- output_list[[5]]
  cat("Fourth model summary - second model with no outliers:\n")
  print(fourth_result)
  cat("\n")
}

#####***** CASE 3 - Impacts of pricing strategies on different samples *****#####

# read file
path2 <- "C:/Users/be_al/OneDrive - UvA/Desktop/Travix/Business_Case_2"
df_sample_1 <- read_csv2(file.path(path2, "sample_1.csv"))

# Column names to lower case
names(df_sample_1) <- tolower(colnames(df_sample_1))

# factor rules
to_factor <- c("rule_1001","rule_1002","rule_1003")
df_sample_1[to_factor] <- lapply(df_sample_1[to_factor],factor)

## Create one formula where both models and plots are calculated/created
rules_analysis <- function(data, controls = FALSE) {
  # Define the formula based on whether controls are included or not
  formula <- if (controls) {
    formula <- as.formula("online.cm ~ rule_1001 + rule_1002 + rule_1003 + earnings.on.segments + insurance.margin + tg.margin + seo.costs + consolidator.costs")
  } else {
    formula <- as.formula("online.cm ~ rule_1001 + rule_1002 + rule_1003")
  }
  
  # Run the model
  model <- lm(formula, data = data)
  
  # Extract coefficients and confidence intervals
  coef_df <- tidy(model, conf.int = TRUE) %>%
    filter(term %in% c("rule_10011", "rule_10021", "rule_10031")) %>%
    rename(period = term, coef = estimate, conf_low = conf.low, conf_high = conf.high)
  
  # Define title text based on controls
  title_text <- if (controls) {
    "Regressions coefficients.\nImpact of Rules on average profit per booking (net of controls)."
  } else {
    "Regressions coefficients.\nImpact of Rules on average profit per booking without controls."
  }
  
  # Create coefficient plot
  plot_coef <- ggplot(coef_df, aes(x = period, y = coef)) +
    geom_point(fill = "steelblue", size = 3) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
    labs(x = "", y = "", title = title_text) +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_discrete(labels = c("Rule 1001", "Rule 1002", "Rule 1003")) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    theme(plot.title = element_text(hjust = 0.5, size = if (controls) 14 else 18),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 18))
  
  # Define title based on controls
  filename <- if (controls) "TEST_controls.png" else "TEST_without_controls.png"
  
  # save file
  ggsave(filename, plot = plot_coef, path = path2, width = 10, height = 7, units = "in")
  
  return(summary = summary(model))
}

# run without controls
rules_analysis(df_sample_1)

# run with controls
riles_analysis(df_sample_1, T)

## I started with both formulas divided below
# After I decided to combine both in one single function
# as above, since the only major differences is that one model has
# controls and the other has not. Apart from that, there are some differences
# in the titles and how the images are saved of course

## The previous code with the two functions:
# rules_no_controls <- function(data){
#   
#   # run the model
#   model1 <- lm(online.cm ~ rule_1001 + rule_1002 + rule_1003, data = df_sample_1)
# 
#   # create visualization
#   
#   # extract coefficients and confidence intervals for period predictor
#   coef_df <- tidy(model1, conf.int = T) %>%
#     select(term, estimate, conf.low, conf.high) %>%
#     filter(term %in% c("rule_10011", "rule_10021", "rule_10031")) %>% 
#     rename(period = term, 
#            coef = estimate, 
#            conf_low = conf.low, 
#            conf_high = conf.high)
# 
#   # create coefficient plot
#   plot_coef <- ggplot(coef_df, aes(x = period, y = coef)) +
#     geom_point(fill = "steelblue", size = 3) +
#     geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
#     labs(x = "", y = "", title = "Regressions coefficients.\nImpact of Rules on average profit per booking without controls." ) +
#     theme_minimal() +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#     scale_x_discrete(labels = c("Rule 1001", "Rule 1002", "Rule 1003")) +
#     scale_y_continuous(limits = c(-30, 40), breaks = seq(-20, 30, by = 10)) +
#     theme(plot.title = element_text(hjust = 0.5, size = 18),
#           axis.text.x = element_text(size = 18),
#           axis.text.y = element_text(size = 18))
#  
#   ggsave("without_controls_NO_tp_tax.png", plot = plot_coef, path = path2, width = 10, height = 7, units = "in")
#   
#   return(summary(model1))
# }
# rules_with_controls <- function(data){
#   # run the model
#   # second model with controls
#   model1 <- lm(online.cm ~ rule_1001 + rule_1002 + rule_1003 + earnings.on.segments + insurance.margin + tg.margin + seo.costs + consolidator.costs
#                , data = data)
#   # create visualization
#   coef_df <- tidy(model1, conf.int = T) %>%
#     select(term, estimate, conf.low, conf.high) %>%
#     filter(term %in% c("rule_10011", "rule_10021", "rule_10031")) %>% 
#     rename(period = term, 
#            coef = estimate, 
#            conf_low = conf.low, 
#            conf_high = conf.high)
#   # extract coefficients and confidence intervals for period predictor
#   
# 
#   # create coefficient plot
#   plot_coef <- ggplot(coef_df, aes(x = period, y = coef)) +
#     geom_point(fill = "steelblue", size = 3) +
#     geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
#     labs(x = "", y = "", title ="Regressions coefficients.\nImpact of Rules on average profit per booking (net of controls).") +
#     theme_minimal() +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#     scale_x_discrete(labels = c("Rule 1001", "Rule 1002", "Rule 1003")) +
#     scale_y_continuous(limits = c(-30, 40), breaks = seq(-20, 30, by = 10)) +
#     theme(plot.title = element_text(hjust = 0.5, size = 14),
#           axis.text.x = element_text(size = 16),
#           axis.text.y = element_text(size = 16))
#   plot_coef
#   ggsave("with_controls_NO_tp_tax.png", plot = plot_coef, path = path2, width = 10, height = 7, units = "in")
#   
#   return(summary(model1))
# }
# 
# # apply function to 1st sample
# rules_no_controls(df_sample_1)
# 
# # apply function to 1st sample
# rules_with_controls(df_sample_1)
# 
# rules_analysis(df_sample_1, T) 
# 

