#libraries-----------------------------
library(tidyverse)
library(here)
library(vroom)
#functions----------------------------
setup_nn <- function(noc, tbbl){
  q <- tbbl|>
    filter(row.names(tbbl)==noc)
  get_nn(q, tbbl)
}
get_nn <- function(q, tbbl){
  nn <- dbscan::kNN(tbbl, k = 11, sort=TRUE,  query = q)
  tibble(nearest_neighbours = rownames(tbbl)[as.vector(nn[["id"]])],
         distance = as.vector(nn[["dist"]]))
}
filter_by_teer <- function(oteer, tbbl){
  tbbl|>
    filter(teer>=oteer)
}
prep <- function(tbbl){
  tbbl|>
    select(-teer)|>
    column_to_rownames(var="noc")
}
first_five <- function(pca){
  pca[["x"]][,1:5]%>% #keep only the first 5 principal components.
    as.data.frame()
}
#read in the data--------------------------------
scaled_char <- vroom(here("processed_data", "scaled_characteristics_noc.csv"))%>%
  mutate(teer=str_sub(noc, 2,2), .after="noc")|>
   select(-median_wage)

#unrestricted-------------------------------
unrestricted_char <- scaled_char|>
  select(-teer)|>
  column_to_rownames(var="noc")

pca <- prcomp(unrestricted_char)

first_pca <-pca[["x"]][,1:5]%>% #keep only the first 5 principal components.
    as.data.frame()

unrestricted <- data.frame(first_pca)%>%
  mutate(data=list(first_pca))%>%
  rownames_to_column(var="noc")%>%
  nest(query = starts_with("PC"))%>%
  mutate(ten_nearest=map2(query, data, get_nn))%>%
  select(-data,-query)%>%
  unnest(ten_nearest)%>%
  filter(noc!=nearest_neighbours)%>%
  arrange(noc)%>%
  mutate(noc_teer=str_sub(noc,2,2), .after="noc")%>%
  mutate(nn_teer=str_sub(nearest_neighbours,2,2), .after="nearest_neighbours")

write_csv(unrestricted, here("processed_data", "unrestricted_ten_nearest_noc.csv"))

#restricted search---------------------------

restricted <- tibble(noc=scaled_char$noc,
                    teer=scaled_char$teer,
                    data=list(scaled_char))|>
  mutate(data=map2(teer, data, filter_by_teer),
         data=map(data, prep),
         pca=map(data, prcomp),
         first_five=map(pca, first_five),
         nearest_neighbours=map2(noc, first_five, setup_nn)
  )|>
  select(noc, noc_teer=teer, nearest_neighbours)|>
  unnest(nearest_neighbours)|>
  mutate(nn_teer=str_sub(nearest_neighbours,2,2), .after="nearest_neighbours")|>
  filter(noc!=nearest_neighbours)

write_csv(restricted, here("processed_data", "restricted_ten_nearest_noc.csv"))





