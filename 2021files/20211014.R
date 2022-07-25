BiocManager::install("rphylopic")
library(tidyverse)
library(rphylopic)

beaver_plot <- beaver1 %>% rownames_to_column("seq") %>% 
  mutate(seq=as.numeric(seq)) %>% 
  ggplot(.,aes(seq,temp,fill="A"))+
  geom_point(size=4,pch=21,color="white")+
  geom_line(group=1)+
  scale_fill_manual(values=c('#56B4E9',"#E69F00"))+
  scale_x_continuous(limits=c(0,114),breaks=seq(0,114,20))+
  labs(x=NULL,y=NULL)+
  theme_classic()+
  theme(axis.text=element_text(color="black",size=10),
        legend.position="non")

#### 获取物种信息

beaver <- name_search(text = "Ginkgo",options = "namebankID")[[1]]

#### 提取想要的图标的id
beaver_id_all <- name_images(uuid = beaver$uid[1]) 
beaver_id <- name_images(uuid = beaver$uid[1])$same[[2]]$uid 

#### 取图标本身
beaver_pic <- image_data(beaver_id, size = 256)[[1]] 
#### 将图标添加到绘图中
beaver_plot + add_phylopic(beaver_pic,alpha=1,color="#56B4E9",
                           x=10,y=37.4,ysize = 10)

