library(tidygraph)
library(ggraph)
library(tidyverse)
library(magrittr)
library(tidytext)
library(widyr)

otu <- read_tsv("otu_taxa_table.xls") %>% 
  separate(taxonomy,
           into=c("domain","phylum","class","order","family","genus","species"),sep=";") %>% 
  mutate_at(vars(c(`domain`:`species`)),~str_split(.,"__",simplify=TRUE)[,2]) %>% 
  column_to_rownames("OTU")

table <- otu %>% select_if(~is.numeric(.)) %>% rownames_to_column("ID")
tax <- otu %>% select_if(~!is.numeric(.)) %>% rownames_to_column("ID")

titles_node <- table %>% left_join(.,tax %>% select(1,phylum),by="ID") %>% 
  select(-ID) %>% 
  group_by(phylum) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm=TRUE))) %>% select(1) %>% 
  distinct(phylum) %>%
  filter(phylum!="") %>% 
  rowid_to_column("id")

title_description_tf_idf <- table %>% left_join(.,tax %>% select(1,phylum),by="ID") %>% 
  select(-ID) %>% 
  group_by(phylum) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm=TRUE))) %>% 
  pivot_longer(-phylum) %>%
  filter(phylum!="",value!=0) %>% 
  set_colnames(c("title","word","n")) %>% 
  bind_tf_idf(word, title , n)

title_similarity <- title_description_tf_idf %>%
  pairwise_similarity(title, word, tf_idf, sort = TRUE)

titles_edges <- title_similarity %>%
  left_join(titles_node, by = c("item1" = "phylum")) %>%
  rename(from = id)

titles_edges %<>%
  left_join(titles_node, by = c("item2" = "phylum")) %>%
  rename(to = id)

titles_edges %<>% select(from, to, similarity)

titles_graph <- tbl_graph(nodes = titles_node,
                          edges = titles_edges,directed = TRUE)

set.seed(123)

ggraph(titles_graph,layout = "fr") +
  geom_edge_fan0(aes(color = similarity))+
  geom_node_point(shape=21,fill = "green",color="white",size=3) +
  geom_node_text(aes(label = phylum), vjust = -1,hjust=0.5,size=3.5,color="black") +
  scale_edge_color_gradientn(colours=colorRampPalette(c("#5eaaf5","#f4d963","red"))(10),
                             na.value="grey80")+
  theme_graph()+
  theme(legend.title = element_blank())
