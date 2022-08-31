setwd("D:/Civic Data Lab/Assam Budget")
library(readxl)
library(tidyverse)
raw_budget <- read.csv("receiptbudget2018-19.csv",encoding="UT-8")

st_budget<-read_excel("Open Budget Standard _ Version 1.2 _ English.xlsx",range="A2:AQ2")

#Taking out head codes  
head_names<-data.frame(name=names(st_budget)) %>% 
  filter(grepl("code_en",name)) %>%
  slice(1:7) %>% add_row(name=c("scheduled_general_total_en")) %>% pull(name)

heads<-raw_budget %>% separate(HEAD.OF.ACCOUNT,sep="-",into=head_names) %>% 
  select(head_names)

#Taking out head descriptions (English)
head_description_names_en<-data.frame(name=names(st_budget)) %>% filter(grepl("desc_en",name)) %>% 
  slice(2:8) %>% pull(name)

head_descriptions_en<-raw_budget %>% separate(HEAD.DESCRIPTION,into=head_description_names_en,sep="[$]") %>% 
  select(all_of(head_description_names_en))

#Taking out budget numbers
budget_names<-data.frame(name=names(st_budget)) %>% filter(grepl("_year",name)) %>% 
  slice(2:5) %>% pull(name) 
old_names=names(raw_budget %>% select(`ACTUALS.2016.17`:`BUDGET.2018.19`))

budget_numbers<-raw_budget %>% 
  rename_with(~ budget_names[which(old_names == .x)], .cols = old_names) %>% 
  select(all_of(budget_names))

#State
state<-raw_budget %>% mutate(state_ut_name_eng="Assam") %>% select(state_ut_name_eng)

#Merging all of the above 
mdata<-cbind(heads,head_descriptions_en,budget_numbers,state) %>% 
  mutate(budget_financial_year="2018-19") %>% 
  mutate(major_head_code_en=as.numeric(major_head_code_en))

#Mapping
mapping<-read.csv("Parsed Major and Minor Heads _ Union and States _ 2021.csv") %>% 
  drop_na(major_head_code_en_start) %>% 
  mutate(position=1:n())

cols<-setdiff(names(mapping),c("major_head_code_en_start","major_head_code_en_end"))

long <- melt(mapping, id.vars = cols) %>% arrange(position)
long[long==""]="Not Defined"
mapped_cols<-long %>% 
  group_by(gov_fund_en) %>% 
  complete(value = first(value):max(value)) %>% 
  ungroup() %>% 
  fill(revenue_or_capital_en:type_of_fund_en,.direction = "down")  %>% 
  mutate_all(na_if,"Not Defined") %>% select(-position,-variable) %>% 
  rename(major_head_code_en=value)

mdata<-mdata %>% inner_join(mapped_cols,by="major_head_code_en")

#Finding common names between standard file and the merged file and reordering based on the standard
mdata<-mdata %>% mutate(major_head_code_en=heads$major_head_code_en)
cols_not_there<-setdiff(names(st_budget),names(mdata))
for (i in cols_not_there){
  mdata[,i]<-NA
}
common_names<-data.frame(colname=names(st_budget)) %>% filter(colname %in% names(mdata))
mdata<-mdata[common_names$colname] 

writexl::write_xlsx(mdata,"D:/Civic Data Lab/Assam Budget/Standardized/assam_receipts_18-19.xlsx")










