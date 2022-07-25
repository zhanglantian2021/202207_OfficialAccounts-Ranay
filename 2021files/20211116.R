library(tidyverse)
library(ggpubr)
library(gapminder)
library(ggthemes)
library(countrycode)
library(ggsci)


colors <-c("#E41A1C","#1E90FF","#FF8C00","#4DAF4A","#984EA3",
           "#40E0D0","#FFC0CB","#00BFFF","#FFDEAD","#90EE90",
           "#EE82EE","#00FFFF","#F0A3FF", "#0075DC", 
           "#993F00","#4C005C","#2BCE48","#FFCC99",
           "#808080","#94FFB5","#8F7C00","#9DCC00",
           "#426600","#FF0010","#5EF1F2","#00998F",
           "#740AFF","#990000","#FFFF00")

gapminder %>% filter(year == "2002") %>%
  ggplot(aes(gdpPercap,lifeExp,size=pop))+
  geom_point(aes(fill=continent),pch=21,color="black")+
  scale_size_continuous(range=c(1,20))+
  scale_fill_manual(values = colors)+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 3),se = FALSE)+
  annotate("text", x =25000,y =60, hjust = 0.5,size =50, color = "#999999",label = "2002", alpha = .3)+
  guides(size="none")+
  guides(fill = guide_legend(override.aes = list(size=4)))+
  theme_minimal()+
  labs(x=NULL,y="lifeExp")+
  theme(
    plot.margin=unit(c(0.3,0.3,0.3,0.3),units=,"cm"),
    axis.line = element_line(color = "#999999",size = 0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.2,color = "#e5e5e5"),
    axis.title.y = element_text(margin = margin(r =8),size = 11,color="black"),
    axis.title.x = element_text(margin = margin(t =8), size = 11,color="black"),
    axis.text = element_text(color="black"),
    legend.title = element_blank(), 
    legend.key=element_blank(), 
    legend.text = element_text(color="black",size=10), 
    legend.spacing.x=unit(0.1,'cm'), 
    legend.key.width=unit(0.5,'cm'), 
    legend.key.height=unit(0.5,'cm'), 
    legend.box.background=element_rect(colour = "black"), 
    legend.box.margin = margin(1,1,1,1))+
  coord_cartesian()+
  stat_cor(color="black",size=4,label.x.npc = "left",label.y.npc = "top")
  

df <- gapminder %>% filter(year == "2002") %>%
  group_by(continent) %>% 
  mutate(q25 = quantile(gdpPercap, probs = .25),
         q75 = quantile(gdpPercap, probs = .75),
         median = median(gdpPercap),
         n = n()) %>% ungroup() %>% 
  mutate(Species_num = as.numeric(fct_rev(continent)),
         location=as.factor(continent))
df %>% 
  ggplot(.,aes(gdpPercap,Species_num -.2)) +
  geom_boxplot(aes(color =continent),width = 0,size = .9)+
  geom_rect(aes(xmin = q25,xmax = median,ymin = Species_num - .35,ymax = Species_num - .05),
            fill = "grey89")+
  geom_rect(aes(xmin = median,xmax = q75,ymin = Species_num - .35,ymax = Species_num - .05),
            fill = "grey79")+
  geom_segment(aes(x = q25, xend = q25,y = Species_num - .05,yend = Species_num - .35,
                   color = continent),size = .25)+
  geom_segment(aes(x = q75, xend = q75,y = Species_num - .05,
                   yend = Species_num - .35,color = continent),size = .25)+
  geom_point(aes(color = continent),shape = "|",size = 5,alpha = .33)+
  ggdist::stat_halfeye(aes(y=Species_num,color=location,fill=continent),
                       shape=18,point_size =3,interval_size =1.8,adjust =.5,.width=c(0,1))+
  geom_text(data = df %>% group_by(continent,Species_num) %>% 
              summarize(m=unique(median)),
            aes(x=m,y=Species_num + .12,label=format(round(m,2),nsmall=2)),
            inherit.aes = F,color = "black",size=3.5)+
  geom_text(data=df %>% group_by(continent,Species_num) %>% 
              summarize(n=unique(n),max=max(gdpPercap,na.rm=T)),
            aes(x=max+.01,y=Species_num+.02,label=glue::glue("n={n}"),
                color=continent),inherit.aes=F,
            size = 3.5,hjust = 0)+
  coord_cartesian(clip="off")+xlab(NULL)+
  ylab("continent")+
  annotate("text",x =30000,y =5,hjust=0.5,size =30,color="#999999",label="2002",alpha = .3)+
  scale_fill_npg()+scale_color_npg()+
  scale_y_continuous(limits =c(.1, NA),breaks = 1:5,
                     labels=rev(c("Africa","Americas","Asia","Europe",
                                  "Oceania")),expand=c(0,0))+
  theme_minimal()+
  theme(panel.grid.major.y=element_blank(),
        axis.text.y = element_text(color=rev(c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF")),
                                   size=10),
        axis.title.y=element_text(color="black",size=11,margin = margin(r=10)),
        legend.position="non",
        axis.text.x=element_text(color="black"))
