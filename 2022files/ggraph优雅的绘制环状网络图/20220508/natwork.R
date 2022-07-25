library(tidygraph)
library(ggraph)
library(tidyverse)
library(magrittr)
library(devtools)
library(tidytext)
library(tidygraph)
library(RColorBrewer)
# install_github("gaospecial/ccgraph")

library(ccgraph)

otu <- read_tsv("otu_taxa_table.xls") %>% 
  select(OTU:C5,taxonomy) %>% 
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

title_description_tf_idf
country_index <- c("title","word")
nodes_country <- gather_graph_node(title_description_tf_idf ,index = country_index, value = "n",root="phylum")
edges_country <- gather_graph_edge(title_description_tf_idf ,index = country_index,root="phylum")

graph_country <- tbl_graph(nodes_country,edges_country)

ggraph(graph_country,layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(aes(color=node1.node.branch),alpha=1/3) + 
  geom_node_point(aes(size=node.size,color=node.branch),alpha=1/3) + 
  coord_fixed()+
  theme_void()+
  theme(legend.position = "none")+
  scale_size(range = c(0.5,80)) +
  geom_node_text(aes(x = 1.0175 * x,y = 1.0175 * y,
      label = node.short_name,
      angle = -((-node_angle(x, y) + 90) %% 180) + 90,
      filter = leaf,color = node.branch), size =3, hjust = 'outward') +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30))+
  geom_node_text(aes(label=node.short_name,filter = !leaf,color = node.branch),
                 fontface="bold",size=4,family="Times")


