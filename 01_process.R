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
    mutate(score=sqrt(Importance*Level), #geometric mean of importance and level
    #mutate(score=Level,
           category=(str_split(file_name,"\\.")[[1]][1]))%>%
    unite(element_name, category, element_name, sep=": ")%>%
    select(-Importance, -Level)
}

# the program------------------------
mapping <- read_excel(here("mapping", "onet2019_soc2018_noc2016_noc2021_crosswalk_consolodated.xlsx"))%>%
  mutate(noc2021=str_pad(noc2021, "left", pad="0", width=5))%>%
  unite(noc, noc2021, noc2021_title, sep=": ")%>%
  select(noc, o_net_soc_code = onetsoc2019)%>%
  distinct()

#the onet data-----------------------------------
tbbl <- tibble(file=c("Skills.xlsx", "Abilities.xlsx", "Knowledge.xlsx", "Work Activities.xlsx"))%>%
#tbbl <- tibble(file=c("Skills.xlsx"))%>%
  mutate(data=map(file, read_data))%>%
  select(-file)%>%
  unnest(data)%>%
  pivot_wider(id_cols = o_net_soc_code, names_from = element_name, values_from = score)%>%
  inner_join(mapping)%>%
  ungroup()%>%
  select(-o_net_soc_code)%>%
  select(noc, everything())%>%
  group_by(noc)%>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))%>% #mapping from SOC to NOC is not one to one: mean give one value per NOC
  mutate(across(where(is.numeric), ~ if_else(is.na(.), mean(., na.rm=TRUE), .))) #for 11 occupations and 4 variables replace missing values with the mean

tbbl%>%
  write_csv(file=here("processed_data", "unscaled_characteristics_noc.csv"))

#wage data-----------

wages <- read_excel("raw_data/2024 ESDC Job Bank Wage Data BC Only.xlsx",
           sheet = "BC Only 2024")|>
  clean_names()|>
  mutate(noc=str_sub(noc, 5))|>
  select(noc, noc_title, low_wage, median_wage, high_wage)|>
  mutate(across(contains("wage"),
                ~ if_else(.x < 5000, .x, .x / 2080)))|>
  unite(noc, noc, noc_title, sep=": ")

write_csv(wages, file=here("processed_data", "wages.csv"))

tbbl%>%
  column_to_rownames(var="noc")%>%
  scale()%>%
  as.data.frame()%>%
  rownames_to_column(var="noc")%>%
  left_join(wages)%>%
  select(-low_wage,-high_wage)%>%
  write_csv(file=here("processed_data", "scaled_characteristics_noc.csv"))

# 10 year job openings data------------------

read_csv(here("raw_data", "job_openings.csv"), skip = 3)|>
  pivot_longer(cols=starts_with("2"), names_to = "year", values_to = "job_openings")|>
  clean_names()|>
  filter(industry=="All industries",
         variable=="Job Openings",
         geographic_area=="British Columbia",
         noc!="#T")|>
  mutate(noc=str_sub(noc, 2))|>
  select(noc, description, year, job_openings)|>
  unite(noc, noc, description, sep=": ")|>
  write_csv(file=here("processed_data", "job_openings.csv"))

#get top 5 cip by noc-------------------

cip_noc <- vroom::vroom(here("raw_data","stats_can", "cip_noc.csv"), skip= 13, n_max = 436)
colnames(cip_noc)[1] <- "Field of Study"
cip_noc <- cip_noc[-1,]

cip_long <- cip_noc%>%
  select(!contains("..."))%>%
  pivot_longer(cols=-"Field of Study", names_to="noc", values_to = "count")
#how many nocs do we have field of study info for?
length(unique(cip_long$noc))

cip_noc_top5 <- cip_long%>%
  mutate(count=as.numeric(str_replace_all(count,",","")),
         `Field of Study`=str_sub(`Field of Study`, 7,-1),
         noc=str_sub(noc,1,5)
         )%>%
  group_by(noc)%>%
  mutate(prop=scales::percent(count/sum(count), accuracy=.1))%>%
  slice_max(count, n=5, with_ties = FALSE)%>%
  arrange(noc, desc(count))%>%
  unite(`Field of Study`, `Field of Study`, prop, sep=": ")%>%
  select(-count)

correct_names <- mapping%>%
  select(noc)|>
  distinct()|>
  mutate(dup = noc)%>%
  separate(dup, into=c("code", "description"), sep=":")%>%
  select(-description)%>%
  rename(noc_full=noc,
         noc=code)

#renaming to match the mapping file names
inner_join(cip_noc_top5, correct_names, by = join_by(noc))%>%
  ungroup()%>%
  select(-noc)%>%
  rename(noc=noc_full)%>%
  write_csv(here("processed_data","cip_noc_top5.csv"))





