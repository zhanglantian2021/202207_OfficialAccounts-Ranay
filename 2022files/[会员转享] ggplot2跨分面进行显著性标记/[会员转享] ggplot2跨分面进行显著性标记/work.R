library(tidyverse)
library(ggpubr)
library(vegan)
library(openxlsx)
library(scales)
library(rstatix)
library(ggsci)

# 加载数据
abund_table<-read.xlsx("OysterHatcheryMicrobiome_phyladata.xlsx",sheet="taxa", rowNames = TRUE)
meta_table<-read.xlsx("OysterHatcheryMicrobiome_phyladata.xlsx",sheet="meta")

### 计算simpson指数
meta_table$Simpsons<-diversity(abund_table, index="simpson")

meta_table$Type <- factor(meta_table$Type,levels =c("Oyster","Swab","Water"))

###  数据整合
df <- meta_table %>% select(1,Type,Day,Trial,Group,Simpsons) %>% 
  unite(.,col="group",Type:Trial,sep="_",remove = T,na.rm = F) %>% 
  select(-1)

# 定义因子
df$group <- factor(df$group,levels=c("Oyster_12_1","Oyster_5_1","Oyster_6_2","Oyster_9_2",
                                    "Swab_12_1","Swab_5_1","Swab_6_2","Swab_9_2",
                                    "Water_1_1","Water_12_1","Water_1_2","Water_9_2","Water_5_3","Water_8_3","Water_12_3"))

# 由于要跨分面添加p值，在此我们使用geom_rect来实现分面的效果，下面数据为定义的坐标点，由于需要做三层注释所以定义三个数据框
df2 <- data.frame(
  xmin = c(0.3,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5),
  xmax = c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,Inf))

df3 <- data.frame(xmin=c(0.3,2.5,4.5,6.5,8.5,10.5,12.5),
                  xmax=c(2.5,4.5,6.5,8.5,10.5,12.5,Inf))

df4 <- data.frame(xmin=c(0.3,4.5,8.5),xmax=c(4.5,8.5,Inf))

# 定义分面文本填充信息，此处应该结合后期的图来进行观看，为了文档的流畅性我将其提前放置‘当然也是三层文本，后两个我们采用一种新的方式
df5 <- data.frame(
  x = factor(c("Oyster_12_1","Oyster_5_1","Oyster_6_2","Oyster_9_2",
               "Swab_12_1","Swab_5_1","Swab_6_2","Swab_9_2",
               "Water_1_1","Water_12_1","Water_1_2","Water_9_2","Water_5_3","Water_8_3","Water_12_3")),
  y = -0.025,
  text = c("12","5","6","9","12","5","6","9","1","12","1","9","5","8","12"))

# 统计分析

df %>% t_test(Simpsons ~ group) %>% adjust_pvalue() %>% 
  add_significance("p.adj") %>% add_xy_position(x="group") %>%
  filter(p.adj.signif !="ns",group1 %in% c("Swab_9_2","Oyster_5_1","Swab_5_1")) %>% as.data.frame()

# 定义p值信息
p_value1 <- tibble(
  x = c("Swab_9_2","Swab_9_2","Water_5_3","Water_5_3"),
  y= c(0.58,0.7,0.7,0.68))

p_value2 <- tibble(
  x = c("Oyster_5_1","Oyster_5_1","Swab_12_1","Swab_12_1"),
  y= c(0.2,0.63,0.63,0.62))

p_value3 <- tibble(
  x = c("Swab_5_1","Swab_5_1","Water_1_2","Water_1_2"),
  y= c(0.12,0.08,0.08,0.5))

# 数据可视化
ggplot()+
  stat_boxplot(data=df,geom="errorbar",width=0.2,aes(fill=Group,group,Simpsons),position=position_dodge(1))+
  geom_boxplot(data=df,aes(fill=Group,group,Simpsons),position=position_dodge(1))+
  scale_y_continuous(limits = c(-0.15,0.75),expand = c(0,0))+
  # 绘制三层注释
  geom_rect(data=df4,aes(xmax=xmax,xmin=xmin),ymin=-0.15,ymax=-0.1,fill=c("#E64B35FF","#4DBBD5FF","#00A087FF"),color="grey90")+
  geom_rect(data=df3,aes(xmax=xmax,xmin=xmin),ymin=-0.1,ymax=-0.05,fill=c("#3C5488FF","#F39B7FFF","#3C5488FF","#F39B7FFF",
                                                                          "#3C5488FF","#F39B7FFF","#8491B4FF"),color="grey90")+
  geom_rect(data=df2,aes(xmax=xmax,xmin=xmin),ymin=-0.05,ymax=0,fill=c("#91D1C2FF","#FF0000","#4DBBD5FF","#00A087FF",
                                                                       "#91D1C2FF","#FF0000","#4DBBD5FF","#00A087FF",
                                                                       "#3C5488FF","#91D1C2FF","#3C5488FF","#00A087FF",
                                                                       "#FF0000","#868686FF","#91D1C2FF"),color="grey90")+
  # 绘制p值间的线条
  geom_line(data = p_value1, aes(x = x, y = y,group=1))+
  geom_line(data = p_value2, aes(x = x, y = y,group=1))+
  geom_line(data = p_value3, aes(x = x, y = y,group=1))+
  # 添加p值文本
  annotate("text", x =10.5, y = 0.71, label = "*",size = 6, color = "#22292F")+
  annotate("text", x =3.35, y = 0.632, label = "****",size = 6, color = "#22292F")+
  annotate("text", x =8.35, y = 0.065, label = "*",size = 6, color = "#22292F")+
  # 添加文本填充
  geom_text(data=df5,aes(x=x,y=y,label=text),color="#22292F",size=5)+
  annotate("text", x =2.5, y = -0.125, label = "Oyster",size = 5, color = "#22292F")+
  annotate("text", x =6.5, y = -0.125, label = "Swad",size = 5, color = "#22292F")+
  annotate("text", x =12.5, y = -0.125, label = "Water",size = 5, color = "#22292F")+
  
  annotate("text", x =1.5, y = -0.08, label = "1",size = 5, color = "#22292F")+
  annotate("text", x =3.5, y = -0.08, label = "2",size = 5, color = "#22292F")+
  annotate("text", x =5.5, y = -0.08, label = "1",size = 5, color = "#22292F")+
  annotate("text", x =7.5, y = -0.08, label = "2",size = 5, color = "#22292F")+
  annotate("text", x =9.5, y = -0.08, label = "1",size = 5, color = "#22292F")+
  annotate("text", x =11.5, y = -0.08, label = "2",size = 5, color = "#22292F")+
  annotate("text", x =14, y = -0.08,label = "3",size = 5, color = "#22292F")+
  scale_fill_brewer()+
  labs(x=NULL,y=NULL)+
  theme_test() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(color ="#DAE1E7"),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(rep(0.2,4),"cm"),
        axis.text = element_text(size = 10, color = "black"),
        axis.text.y = element_text(margin = margin(r = 5)),
        legend.title = element_blank(), 
        legend.key=element_blank(),  
        legend.text = element_text(color="black",size=10),
        legend.spacing.x=unit(0.1,'cm'), 
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'), 
        legend.background=element_blank(), 
        legend.position = c(0,1),legend.justification = c(0,1))



