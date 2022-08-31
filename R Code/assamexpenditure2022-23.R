setwd("D:/Civic Data Lab/Assam Budget")
library(readxl)
library(tidyverse)
library
raw_budget <- read_excel("expenditurebudget2022-23.xlsx")
st_budget<-read_excel("Open Budget Standard _ Version 1.2 _ Bilingual.xlsx",range="A2:BZ2") 

#Taking out Grant Columns  
three_pieces_raw<-raw_budget %>% 
  separate(GRANT_NUMBER,sep="-",into=c("demand_grant_no_en","demand_grant_desc_en","unknown")) %>% 
  select("#","demand_grant_no_en","demand_grant_desc_en","unknown")

two_pieces<-three_pieces_raw %>% filter(is.na(unknown)) %>% select(-unknown)

three_pieces<-three_pieces_raw %>% drop_na() %>% 
  mutate("demand_grant_desc_en"=paste(demand_grant_desc_en,unknown,sep=" - ")) %>% 
  select(-unknown)
demand_budget<-bind_rows(two_pieces,three_pieces) %>% arrange("#")

#Taking out head codes  
head_names<-data.frame(name=names(st_budget)) %>% 
  filter(grepl("code_en",name)) %>%
  slice(1:7) %>% 
  add_row(name=c("plan_scheme_code_en","scheduled_general_total_en","voted_or_charged_en")) %>% 
  pull(name)

eleven_pieces_raw<-raw_budget %>% separate(HEAD,sep="-",into=c(head_names,"unknown")) %>% 
  select(c(head_names,"unknown")) %>% mutate(position=1:n())

#Taking out columns which have 10 pieces
ten_pieces<-eleven_pieces_raw %>% filter(is.na(unknown)) %>% select(-unknown)

#Working on columns which have 11 pieces
eleven_pieces<-eleven_pieces_raw %>% drop_na() %>% 
  mutate(plan_scheme_code_en=paste(plan_scheme_code_en,scheduled_general_total_en,sep="-")) %>% 
  select(-scheduled_general_total_en) %>% 
  rename("scheduled_general_total_en"=voted_or_charged_en,
         "voted_or_charged_en"="unknown") 

heads<-bind_rows(ten_pieces,eleven_pieces) %>% arrange(position) %>% select(-position)

#Taking out head descriptions (English)
head_description_names_en<-data.frame(name=names(st_budget)) %>% filter(grepl("desc_en",name)) %>% 
  slice(2:8) %>% pull(name)
head_descriptions_en<-raw_budget %>% separate(HEAD_DESCRIPTION,into=head_description_names_en,sep="[$]") %>% 
  select(head_description_names_en)

#Taking out head descriptions (Assamese)
head_description_names_olang<-data.frame(name=names(st_budget)) %>% filter(grepl("desc_olang",name)) %>% 
  slice(2:8) %>% pull(name)
head_descriptions_olang<-raw_budget %>% separate(HEAD_DESCRIPTION_ASSAMESE,into=head_description_names_olang,sep="[$]") %>% 
  select(head_description_names_olang)

#Taking out budget numbers
budget_names<-data.frame(name=names(st_budget)) %>% filter(grepl("_year",name)) %>% 
  slice(2:5) %>% pull(name) 
old_names=names(raw_budget %>% select(ACTUALS_2020_21:BUDGET_2022_23))
budget_numbers<-raw_budget %>% 
  rename_with(~ budget_names[which(old_names == .x)], .cols = old_names) %>% 
  select(budget_names)

#State, Department and Nodal Agency
department<-raw_budget %>% select(nodal_agency_en=DEPARTMENT)
state<-raw_budget %>% mutate(state_ut_name_eng="Assam") %>% select(state_ut_name_eng)

#Merging all of the above 
mdata<-cbind(demand_budget,heads,head_descriptions_en,head_descriptions_olang,budget_numbers,department,state) %>% 
  mutate(budget_financial_year="2022-23") %>% 
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

mdata<-mdata %>% left_join(mapped_cols,by="major_head_code_en")
mdata<-distinct(mdata)

#Finding common names between standard file and the merged file and reordering based on the standard
mdata<-mdata %>% mutate(major_head_code_en=heads$major_head_code_en)
cols_not_there<-setdiff(names(st_budget),names(mdata))
for (i in cols_not_there){
  mdata[,i]<-NA
}
common_names<-data.frame(colname=names(st_budget)) %>% filter(colname %in% names(mdata))
mdata<-mdata[common_names$colname] 



writexl::write_xlsx(mdata,"D:/Civic Data Lab/Assam Budget/Standardized/assam_expenditure_22-23.xlsx")










