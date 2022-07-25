library(tidyverse)

# 自定义颜色
colors <-c("#FED439FF","#709AE1FF",
           "#D5E4A2FF","#197EC0FF","#F05C3BFF","#46732EFF",
           "#71D0F5FF","#370335FF","#075149FF","#C80813FF","#91331FFF",
           "#1A9993FF","#FD8CC1FF")

# 构建数据
data1 <- mtcars %>% head(6) %>% 
  mutate_if(is.numeric, function(x) x+10) %>% 
  log10() %>% as.data.frame() %>% 
  rownames_to_column("type") %>%
  pivot_longer(-type) %>% arrange(name) %>% 
  mutate(type=factor(type))

# 计算角度
empty_bar <- 0
data1$id <- seq(1,nrow(data1))
label_data <- data1
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# 构建分组线段
df <- data1 %>% 
  group_by(name) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start,end))) %>% 
  mutate(name=as.factor(name))

# 数据可视化
ggplot(data1,aes(id,value,fill=type))+
  geom_bar(stat="identity",alpha=0.8)+
  scale_fill_manual(values = colors)+
  labs(x=NULL,y=NULL)+
  ylim(-7,8)+
  coord_polar(start = 0)+
  theme_void()+
  theme(
  legend.text = element_text(color="black"),
  legend.title=element_blank(),
  legend.spacing.x=unit(0.2,'cm'),
  legend.key=element_blank(),
  legend.key.width=unit(0.3,'cm'),
  legend.key.height=unit(0.3,'cm'),
  legend.position=c(0.5,0.5))+
  # 添加分组线段
 # geom_segment(data=df,aes(x = start,y =-0.3,xend = end,yend =-0.3),
 #              colour = "black",alpha=0.8,size=0.5,inherit.aes = FALSE)+
  # 添加标签
  geom_text(data=label_data,aes(x=id, y=value+0.5,label=type, hjust=hjust), 
            color="black", fontface="plain",size=2.5, 
            angle= label_data$angle, inherit.aes = FALSE )+
  # 添加文本
  annotate("text",x=4,y=-0.8,label="A",angle=-35,size=3.5,color="black")+
  annotate("text",x=10,y=-0.8,label="B",angle=-50,size=3.5,color="black")+
  # 添加外圈
  geom_segment(aes(x=0, y=8,xend=66.5,yend =8),size=1.5,color="#3B9AB2",
    arrow = arrow(length = unit(0, "npc"),type="closed"))+
  # 添加竖线
  annotate("segment",x=0,xend =0,y=-3,yend =8,colour="#FF0000")+
  annotate("segment",x=6.5,xend =6.5,y=-3,yend =8,colour="#00A08A")+
  annotate("segment",x=12.5,xend =12.5,y=-3,yend=8,colour="#FF0000")+
  annotate("segment",x=18.5,xend =18.5,y=-3,yend =8,colour="#00A08A")+
  annotate("segment",x=24.5,xend =24.5,y=-3,yend =8,colour="#FF0000")+
  annotate("segment",x=30.5,xend =30.5,y=-3,yend =8,colour="#00A08A")+
  annotate("segment",x=36.5,xend =36.5,y=-3,yend =8,colour="#FF0000")+
  annotate("segment",x=42.5, xend =42.5,y=-3,yend =8,colour="#00A08A")+
  annotate("segment",x=48.5,xend =48.5,y=-3,yend =8,colour="#FF0000")+
  annotate("segment",x=54.5,xend =54.5,y=-3,yend =8,colour="#00A08A")+
  annotate("segment",x=60.5,xend =60.5,y=-3,yend =8,colour="#FF0000")+
  # 添加内圈
  geom_segment(aes(x=0, y=-3,xend=66.5,yend =-3),size=0.5,color="#3B9AB2",
               arrow = arrow(length = unit(0, "npc"),type="closed"))+
  geom_segment(aes(x=0, y=-0.1,xend=66.5,yend =-0.1),size=0.5,color="black",
               arrow = arrow(length = unit(0, "npc"),type="closed"))



