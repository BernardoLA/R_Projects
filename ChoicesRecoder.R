#################
# 18/04/2023 - Bernardo Leivas -
# This code is based on Choice-Based Choice data. Ultimately we want to 
# convey in one single file the products that respondents said they would
# choose over other available options. However, we have this information
# in two separate files and some recoding is necessary to make it work
# in one single file.
#################

## Build a Single CSV with Dual None  ##

## Load all necessary packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

## We need to import two files:
# 1) responses: the answer of respondents in the survey i.e. 
# their selected choice among the products shown in each screen
# 2) design: the statistical design that allows us to know the configuration
# of products shown in each screen for each respondent.

setwd("C:/Users/BernardoLeivasdeAlme/OneDrive - SKIM/Trainings/My R/Counts")
responses <- read_csv(file.path(getwd(), "1. PoultryUK_responses_v4.csv"))
design <- read_csv(file.path(getwd(), "7. 50 desjes picked HW.csv"))



## 1) 1st step create a single csv file with the actual skus and concepts (without the none)


# get some variables
nr_of_resps<- nrow(responses) ## nr of resps
nr_of_tasks <- max(design$Task) ## nr of tasks
nr_of_concepts <- max(design$Concept) ## nr of concepts
nr_of_atts <- length(design)-length(design[c(1:3)]) ## nr of attributes


data_merged_long <- responses %>%
dplyr::select(-matches("_none"),
         Resp = sys_RespNum,
         Version = sys_CBCVersion_CBCPoultryUK) %>% 
  pivot_longer(!c(1,2), names_to ="Task", values_to = "Choice") %>% 
  mutate(Task = as.numeric(str_extract(Task,"\\d+"))) %>% 
  left_join(design, by = c("Version","Task")) %>% 
  mutate(Responses = ifelse(Choice==Concept,1,0)) %>% 
  dplyr :: select(-matches("Choice"))
  
data_merged_long

## 2) renumbering the tasks to the correct numbering

old_tasks <- c(nr_of_tasks:1)

min_new <- min(old_tasks)-1
max_new <- max(old_tasks)-1

new_tasks <- old_tasks + c(max_new:min_new)

new_tasks
old_tasks
max_new
for (n in 1:nr_of_tasks) {
  data_merged_long["Task"] <- replace(data_merged_long["Task"],data_merged_long["Task"]==old_tasks[n],new_tasks[n])
}

data_merged_long
# 3) Creating new dataset with all respondents needed and with the correct task numbering that will be used to merge the  .number of rows is the number row times tasks 36

nr_resp_rows <- (nr_of_concepts+2)*(nr_of_tasks) ## is the two extra tasks we have to include for the none. *** this would have to be adjusted for more none options
max_task_new <- 2*max(old_tasks) ## we get the maximum number of a task in this new dataset by simply multiplying the nr of tasks by 2, because we have only one none, with more nones have to check. 
nr_resp_tasks <- nr_of_tasks*nr_of_resps

resp_1000 <- rep(responses$sys_RespNum, times = rep(nr_resp_rows,nr_of_resps))
task_1 <- rep(1:max_task_new, times = rep(c(nr_of_concepts,2),nr_of_tasks)) ## same here as before
task_1
task_1000 <-  rep(task_1, nr_of_resps)

# 3.1) we can then finally create the dataset

df <- tibble(Resp = resp_1000,Task = task_1000, Concept = rep(c(1:nr_of_concepts,1,2),nr_resp_tasks))
df
# 3.2) And we can finally merge this new data set with our data and we'll repeat roughly the same process for the 

df_merged <- df %>% 
  left_join(data_merged_long, by = c("Resp","Task","Concept"))


data_merged_long


df_merged


## 4) Getting the none data ready, first getting the single csv.


new_tasks_none <- new_tasks + 1 


data_none <- responses %>% 
  select(Resp = sys_RespNum,
         Version = sys_CBCVersion_CBCPoultryUK,
         matches("_none"),
         ) %>% 
  pivot_longer(cols = -c("Resp", "Version"),
               names_to = "Task",
               values_to = "Choice") %>% 
  mutate(Task = as.numeric(str_extract(Task,"\\d+"))) %>% 
  left_join(design, by = c("Version","Task")) %>% 
  add_column(Responses = data_merged_long$Responses)

data_none_con <- data_none %>% 
  mutate(Concept = ifelse(Responses == 1, 1, row_number()+100))
data_none_con

for (n in 1:nr_of_tasks) {
  data_none_con["Task"] <- replace(data_none_con["Task"], data_none_con["Task"]==old_tasks[n], new_tasks_none[n])
}
 
## the steps are to create a data set to emerge with the merged none dataset than merge the none data set with the other one
## create new data for the none

data_none_con

Resp <-data_none_con$Resp[c(rep(T,nr_of_tasks*2),rep(F,nr_of_tasks*2))]
Version <- data_none_con$Version[c(rep(T,nr_of_tasks*2),rep(F,nr_of_tasks*2))]
Task <- data_none_con$Task[c(T,T,F,F)]
Concept <- rep(rep(c(1,2),nr_of_resps),nr_of_tasks)

df_none <- tibble(Resp, Version, Task, Concept)
df_none
last_column <- grep("Responses",names(data_none_con))
first_att_pos <- grep("att",names(data_none_con))[1]

df_none

df_none_merged <- df_none %>% 
  left_join(data_none_con, by = c("Resp","Version","Task","Concept")) %>% 
  mutate_at(first_att_pos:last_column, ~replace_na(.,0)) 

  df_none_merged
  data_none_con
## here is very important! we vectorize the points where have to be adjusted, we avoid using loops and leave the code more efficient
# get those that actually chose none after choosing the concept and adjust their response as None based on that.

vector_adjustment = which(df_none_merged$Choice==2)
vector_adjustment
df_none_merged$Responses[vector_adjustment]=0
df_none_merged$Responses

vector_replacement <- which(df_none_merged$Choice==2)+1
vector_replacement
df_none_merged$Responses[vector_replacement]=1

df_none_merged$Responses


## now try to merge None with the other dataset
final_data_merged <- df_merged %>% 
  full_join(df_none_merged, by = c("Resp","Task","Concept")) %>%
  select(-Choice)

final_data_merged$None = rep(rep(c(0,0,0,0,0,1),12),1000)
Single_CSV <- final_data_merged

Single_CSV
for (n in 1:nr_of_tasks) {
  rows_index <- which(Single_CSV$Task==new_tasks_none[n])
  Single_CSV[rows_index,c(4:16)] <- Single_CSV[rows_index,c(17:29)]
}
new_tasks_none
Single_CSV <- Single_CSV %>% 
  select(-ends_with(".y"),-Version.x) %>% 
  rename_with(~str_remove(.,".x"), everything())

  
## Prepare to export the Single CSV file

Single_CSV <- Single_CSV[,c(1:14,16,15)]
write_csv(Single_CSV,"MyPoultry_Single_CSV.csv")



