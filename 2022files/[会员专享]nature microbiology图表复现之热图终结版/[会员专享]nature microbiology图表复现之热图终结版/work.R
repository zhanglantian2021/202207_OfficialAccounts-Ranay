library(tidyverse)
library(magrittr)
library(ggh4x)
library(grid)

df <- read_tsv("R.xls") %>% pivot_longer(-ID) %>% 
  left_join(.,read_tsv("p.xls") %>% pivot_longer(-ID),by=c("ID","name")) %>% 
  set_colnames(c("ID","name","Rvalue","Pvalue")) %>% 
  mutate(p_signif=symnum(Pvalue,corr = FALSE, na = FALSE,  
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                         symbols = c("***", "**", "*", ".", " "))) 


group <- df %>% select(1) %>% distinct() %>% as.data.frame() %>% 
  rownames_to_column("raw") %>% 
  mutate(raw=as.numeric(raw),group=case_when(raw >3 & raw <= 17 ~ "Baclerial phyla",
                                             raw <=3 ~ " ",
                                             raw >17 & raw<=20 ~"Fungal\nphyla",
                                             raw >20 & raw<=23 ~"Fungal\nguilds",
                                             raw > 23 & raw <= 28 ~"Protistan\nlineages",
                                             raw > 28 ~ "Protistan\ntrophic\ngroups")) %>% 
  select(-raw)

df2 <- df %>% left_join(.,group,by="ID") %>% 
  mutate(across("ID",str_replace,"total","richness"),
         across("name",str_replace,"Total.biomass","Microbial"),
         across("name",str_replace,".biomass",""),
         across("name",str_replace,"F.B.ratio","F:B ratio"))

df2$ID <- factor(df2$ID,levels = df2$ID %>% as.data.frame() %>% distinct() %>% pull() %>% rev())
df2$group <- factor(df2$group,levels=df2$group %>% as.data.frame() %>% distinct() %>% pull())
df2$name <- factor(df2$name,levels=df2$name %>% as.data.frame() %>% distinct() %>% pull())


annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}


ridiculous_strips <- strip_themed(
  text_y = elem_list_text(colour = c("#FCFAD9","#EDB749","#3CB2EC","#12618D","#9C8D58","#4A452A"),
                          face = c("bold","bold","bold","plain","bold","bold"),size=c(12,12,12,12,12,12)),
  background_y = elem_list_rect(
    fill =  c("#FCFAD9","#FFF2E7","#E8F2FC","#BDE7FF","#EEECE1","#DDD9C3")))

scales <- list(
  scale_y_discrete(guide = guide_axis_colour(colour = rev(c("#EDB749","#3CB2EC","#9C8D58"))),expand=c(0,0)),
  scale_y_discrete(guide = guide_axis_colour(colour ="#EDB749"),expand=c(0,0)),
  scale_y_discrete(guide = guide_axis_colour(colour ="#3CB2EC"),expand=c(0,0)),
  scale_y_discrete(guide = guide_axis_colour(colour ="#12618D"),expand=c(0,0)),
  scale_y_discrete(guide = guide_axis_colour(colour ="#9C8D58"),expand=c(0,0)),
  scale_y_discrete(guide = guide_axis_colour(colour ="#4A452A"),expand=c(0,0))
  
)


df2 %>% ggplot(.,aes(name,ID,col=Rvalue,fill=Rvalue))+
  geom_tile(color="grey80",fill="white",size=0.1)+
  geom_point(aes(size =abs(Rvalue)),shape=22)+
  geom_text(aes(label=p_signif),size=4,color="white",hjust=0.5,vjust=0.7)+
  facet_grid2(vars(group),scale="free_y",switch = "y",strip = ridiculous_strips)+
  force_panelsizes(cols=c(2.8),rows = c(1,4,1,1,1.2,1),respect = TRUE)+
  labs(x = NULL,y = NULL,color=NULL,fill=NULL)+
  scale_color_gradientn(colours = (RColorBrewer::brewer.pal(11,"RdBu")))+
  scale_fill_gradientn(colours = (RColorBrewer::brewer.pal(11,"RdBu")))+
  scale_y_discrete(expand=c(0,0),position = 'left')+
  scale_x_discrete(expand=c(0,0),position = 'top',
                   labels=c("HR"=expression(R[h]),
                            "SR"=expression(R[s])))+
  theme(axis.text.x=element_text(angle =45,hjust =0,vjust =1,color=c(rep("#3CB2EC",4),rep("#EDB749",5)),
                                                                     face = "bold",size = 10),
        axis.text.y=element_text(face = "bold",size = 10),
        axis.ticks= element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.placement = 'outside',
        panel.background = element_blank(),
        strip.text.y = element_text(size=10,color="black",face = "bold"),
        panel.spacing.y = unit(0,"cm"))+
    facetted_pos_scales(y = scales)+
    scale_size(guide=NULL)+
    guides(color=guide_colorbar(direction="vertical",reverse=F,barwidth=unit(.5,"cm"),
                                barheight=unit(20.45,"cm")))+
  coord_cartesian(clip = 'off')+
  annotation_custom2(grob = rectGrob(gp = gpar(fill="#DDD9C3",col="#DDD9C3")),
                     data = df2 %>% filter(group=="Protistan\ntrophic\ngroups"),
                     xmin =-7.5,xmax =0.38,ymin =0.54,ymax=3.5)+
  annotation_custom2(grob = rectGrob(gp = gpar(fill="#EEECE1",col="#EEECE1")),
                     data = df2 %>% filter(group=="Protistan\nlineages"),
                     xmin =-7.5,xmax =0.38,ymin=0.54,ymax=5.5)+
  annotation_custom2(grob = rectGrob(gp = gpar(fill="#E8F2FC",col="#E8F2FC")),
                     data = df2 %>% filter(group=="Fungal\nphyla"),
                     xmin =-7.5,xmax =0.38,ymin=0.53,ymax=3.5)+
  annotation_custom2(grob = rectGrob(gp = gpar(fill="#BDE7FF",col="#BDE7FF")),
                     data = df2 %>% filter(group=="Fungal\nguilds"),
                     xmin =-7.5,xmax =0.38,ymin=0.53,ymax=3.5)+
  annotation_custom2(grob = rectGrob(gp = gpar(fill="#FFF2E7",col="#FFF2E7")),
                     data = df2 %>% filter(group=="Baclerial phyla"),
                     xmin =-7.5,xmax =0.38,ymin=0.53,ymax=14.5)+
  annotation_custom2(grob = rectGrob(gp = gpar(fill="#FCFAD9",col="#FCFAD9")),
                     data = df2 %>% filter(group==" "),
                     xmin =-7.5,xmax =0.38,ymin=0.53,ymax=3.5)
