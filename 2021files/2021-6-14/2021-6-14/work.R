library(tidyverse)
library(ggvenn)


group <- read.delim("group.xls")

df <- read.delim("otu.xls",check.names=F) %>% 
  as_tibble() %>% pivot_longer(-ASV) %>%
  filter(value !=0) %>% rename(sample=name)


A <- df %>% filter(sample=="A_1")
B <- df %>% filter(sample=="B_1")
C <- df %>% filter(sample=="C_1") 
D <- df %>% filter(sample=="D_1") 


list(A=A$ASV,B=B$ASV,C=C$ASV,D=D$ASV) %>% 
ggvenn(show_percentage = T,show_elements = F,label_sep = ",",
       digits = 1,stroke_color = "white",stroke_alpha=1,
     #  stroke_linetype="dashed",
       fill_color = c("#E41A1C", "#1E90FF", "#FF8C00",
                      "#4DAF4A"),
       set_name_color = c("#E41A1C", "#1E90FF","#FF8C00","#4DAF4A"))


A <- df %>% left_join(group,by="sample") %>% filter(group=="A")
B <- df %>% left_join(group,by="sample") %>% filter(group=="B")
C <- df %>% left_join(group,by="sample") %>% filter(group=="C")
D <- df %>% left_join(group,by="sample") %>% filter(group=="D")


list(A=A$ASV,B=B$ASV,C=C$ASV,D=D$ASV) %>% 
  ggvenn(show_percentage = T,show_elements = F,label_sep = ",",
         digits = 1,stroke_color = "white",stroke_alpha=1,
         stroke_size=0.6,
         stroke_linetype="dashed",
         fill_color = c("#E41A1C", "#1E90FF", "#FF8C00",
                        "#4DAF4A", "#984EA3"),
         set_name_color = c("#E41A1C", "#1E90FF","#FF8C00","#4DAF4A"))

