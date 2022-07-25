library(tidyverse)
library(magrittr)
library(ggh4x)
library(ggsci)

da <- function(da) {
  (da/(da %>% rowSums())) %>% return()
}

otu <- read.delim('otu.xls',row.names=1) %>%
  select_if(is.numeric) %>% t() %>% da() %>% 
  t() %>% as.data.frame() %>% rownames_to_column(var="OTU") %>% 
  left_join(.,read_tsv("otu.xls") %>% select_if(~!is.numeric(.)),
            by="OTU") %>%
  separate(taxonomy,
           into=c("domain","phylum","class","order","family","genus","species"),sep=";") %>%
mutate_at(vars(c(`domain`:`species`)),~str_split(.,"__",simplify=TRUE)[,2]) %>% 
  column_to_rownames("OTU")


table <- otu %>% select_if(~is.numeric(.)) %>% rownames_to_column("ID")
tax <- otu %>% select_if(~!is.numeric(.)) %>% rownames_to_column("ID")


# 定义颜色
pal <- c("#E41A1C","#1E90FF","#FF8C00","#4DAF4A","#984EA3","#40E0D0","#FFC0CB",
         "#00BFFF","#FFDEAD","#90EE90","#EE82EE","#00FFFF","#F0A3FF", "#0075DC", 
         "#993F00","#4C005C","#2BCE48","#FFCC99","#808080","#94FFB5","#8F7C00",
         "#9DCC00","#C20088","#003380","#FFA405","#FFA8BB","#426600","#FF0010",
         "#5EF1F2","#00998F","#740AFF","#990000","#FFFF00")

# 定义主题 
theme_niwot <- function(){
  theme_bw()+
    theme(strip.background = element_rect(fill="white",color="black"),
          panel.spacing = unit(0,"lines"),
          strip.text.x = element_text(size=8,color="black"),
          axis.text.y=element_text(size=8,color="black"),
          axis.title.y = element_text(size=10,color="black"),
          legend.key=element_blank(),
          legend.text = element_text(color="black",size=8),
          legend.spacing.x=unit(0.1,'cm'),
          legend.spacing.y=unit(0.1,'cm'),
          legend.key.width=unit(0.4,'cm'),
          legend.key.height=unit(0.4,'cm'),
          legend.background=element_blank(),
          panel.grid.major=element_blank(),panel.grid.minor=element_blank())
}


table %>% left_join(.,tax %>% select(1,genus),by="ID") %>% 
  select(-ID) %>% 
  group_by(genus) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm=TRUE))) %>% 
  filter(genus !="") %>% 
  column_to_rownames(var="genus") %>% 
  write.table(.,file="genus.xls",sep="\t",quote = F)


computed_persent <- function(path) {
  data <- path %>% read.delim(check.names = FALSE, row.names = 1)
  data2 <- data %>%
    mutate(sum = rowSums(.), persent = sum / sum(sum) * 100,sum = NULL,) %>%
    rbind(filter(., persent < 1) %>% colSums()) %>%
    mutate(OTU_ID = c(data %>% rownames(), "others"))
  filter(data2[1:(nrow(data2) - 1),], persent > 1) %>%
    rbind(data2[nrow(data2),]) %>%
    select(ncol(.), 1:(ncol(.) - 2)) %>%
    set_rownames(seq_len(nrow(.))) %>%
    return()
}


df <- computed_persent("genus.xls") %>% 
  pivot_longer(-OTU_ID) %>%
  set_colnames(c("genus","SamplesID","value")) %>% 
  right_join(.,read_tsv("group.xls",col_name=F) %>% 
               set_colnames(c("SamplesID","group")),by="SamplesID") %>% 
  filter(value !=0)

df$group <- factor(df$group,levels = df$group %>% as.data.frame() %>% 
                     distinct() %>% pull())

df %>% ggplot(.,aes(SamplesID,value,fill=genus))+
  geom_bar(stat="identity",position = "fill")+
  facet_nested(.~group,drop=T,scale="free",space="free",switch="y")+
  labs(x=NULL,y=NULL)+
  scale_y_continuous(expand=c(0,0),labels=scales::percent)+
  scale_x_discrete(expand=c(0.1,0.1)) +
  labs(fill="genus")+
  scale_fill_manual(values = pal)+
  theme_niwot()+
  theme(axis.text.x=element_text(angle = 90,color="black",vjust=0.5))
