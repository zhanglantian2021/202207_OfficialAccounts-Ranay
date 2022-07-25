package.list=c("tidyverse","ggdist")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- iris %>% select(1,2,5) %>% 
  mutate(ratio = Sepal.Length/ Sepal.Width) %>% as_tibble() %>% 
  filter(!is.na(ratio)) %>% group_by(Species) %>% 
  mutate(median = median(ratio),
         q25 = quantile(ratio, probs = .25),
         q75 = quantile(ratio, probs = .75),
         n = n()) %>% ungroup() %>% 
  mutate(Species_num = as.numeric(fct_rev(Species))) %>% select(-1,-2)

P <- ggplot(df,aes(ratio,Species_num -.2)) +
  geom_boxplot(aes(color = Species),width = 0,size = .9)+
  geom_rect(aes(xmin = q25,xmax = median,ymin = Species_num - .35,ymax = Species_num - .05),
            fill = "grey89")+
  geom_rect(aes(xmin = median,xmax = q75,ymin = Species_num - .35,ymax = Species_num - .05),
            fill = "grey79")+
  geom_segment(aes(x = q25, xend = q25,y = Species_num - .05,yend = Species_num - .35,
                   color = Species),size = .25)+
  geom_segment(aes(x = q75, xend = q75,y = Species_num - .05,
                   yend = Species_num - .35,color = Species),size = .25)+
  geom_point(aes(color = Species),shape = "|",size = 5,alpha = .33)

P
pp <- P + ggdist::stat_halfeye(aes(y = Species_num,color = Species,fill = Species),
                     shape =18,point_size =3,interval_size =1.8,
                     adjust =.5,.width = c(0,1))+
  geom_text(data = df %>% group_by(Species, Species_num) %>% 
              summarize(m = unique(median)),
            aes(x = m, y = Species_num + .12,
                label = format(round(m, 2), nsmall = 2)),
            inherit.aes = F,color = "white",size = 3.5) +
  geom_text(data = df %>% 
              group_by(Species,Species_num) %>% 
              summarize(n = unique(n),max = max(ratio, na.rm = T)),
            aes(x = max + .01, y = Species_num + .02,
                label = glue::glue("n = {n}"),
                color = Species),inherit.aes = F,
            size = 3.5,hjust = 0) +
  coord_cartesian(clip = "off")+
  xlab(NULL)+ylab(NULL)
  
pp +scale_y_continuous(limits =c(.1, NA),breaks = 1:3,
                     labels = c("setosa","versicolor", "virginica"),
                     expand = c(0, 0))+
  scale_color_manual(values = c("#FF8C00", "#A034F0", "#159090"),guide = F) +
  scale_fill_manual(values = c("#FF8C00", "#A034F0", "#159090"),guide = F)  +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = rev(c("#FF8C00", "#A034F0", "#159090")),
                                   size = 12))
