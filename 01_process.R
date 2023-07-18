library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(factoextra)
#functions------------------
read_data <- function(file_name){
  read_excel(here("raw_data", "onet", file_name))%>%
    clean_names()%>%
    select(o_net_soc_code, element_name, scale_name, data_value)%>%
    pivot_wider(names_from = scale_name, values_from = data_value)%>%
    mutate(score=sqrt(Importance)*sqrt(Level), #geometric mean of importance and level
           category=(str_split(file_name,"\\.")[[1]][1]))%>%
    unite(element_name, category, element_name, sep=": ")%>%
    select(-Importance, -Level)
}
read_ete <- function(file_name){
  read_excel(here("raw_data", "onet", file_name))%>%
    clean_names()%>%
    select(o_net_soc_code, element_name, category, data_value)%>%
    filter(!element_name %in% c("Job-Related Professional Certification", "Apprenticeship"))%>%
    mutate(catval=category*data_value/100)%>% #apply weights to categories
    group_by(o_net_soc_code, element_name)%>%
    summarize(score=sum(catval, na.rm = TRUE))%>% #weighted mean
    mutate(element_name=paste("Educ. Train. Exper.",element_name, sep=": "))
}

# the program------------------------
mapping <- read_excel(here("mapping", "onet2019_soc2018_noc2016_noc2021_crosswalk.xlsx"))%>%
  mutate(noc2021=str_pad(noc2021, "left", pad="0", width=5))%>%
  unite(noc, noc2021, noc2021_title, sep=": ")%>%
  select(noc, o_net_soc_code = onetsoc2019)%>%
  distinct()
tbbl1 <- tibble(file=c("Skills.xlsx", "Abilities.xlsx", "Knowledge.xlsx", "Work Activities.xlsx"))%>%
  mutate(data=map(file, read_data))
tbbl2 <- tibble(file="Education, Training, and Experience.xlsx")%>%
  mutate(data=map(file, read_ete))
tbbl <- bind_rows(tbbl1, tbbl2)%>%
  select(-file)%>%
  unnest(data)%>%
  pivot_wider(id_cols = o_net_soc_code, names_from = element_name, values_from = score)%>%
  inner_join(mapping)%>%
  ungroup()%>%
  select(-o_net_soc_code)%>%
  select(noc, everything())%>%
  group_by(noc)%>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))%>% #mapping from SOC to NOC is not one to one: mean give one value per NOC
#  mutate("Educ. Train. Exper.: TEER"= as.numeric(str_sub(noc, 2, 2)))%>% #NOPE: teer is an un-ordered factor, not numeric
  mutate(across(where(is.numeric), ~ if_else(is.na(.), mean(., na.rm=TRUE), .)))%>% #for 11 occupations and 4 variables replace missing values with the mean
  column_to_rownames(var="noc")%>%
  scale()%>%
  as.data.frame()%>%
  rownames_to_column(var="noc")

tbbl%>%
  write_csv(file=here("processed_data", "scaled_characteristics_noc.csv"))
#make some fake job openings and wage data-----------
tbbl%>%
  select(noc)%>%
  mutate(wage=round(runif(506, min=20, max=75),2),
         job_openings=round(runif(506, min=1000, max=10000)))%>%
  write_csv(file=here("processed_data", "jow.csv"))





