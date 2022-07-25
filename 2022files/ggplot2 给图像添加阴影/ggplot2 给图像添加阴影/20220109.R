package.list=c("tidyverse","ggsci","GGally","GEOquery")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

geom_stripped_rows(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  show.legend = NA,
  inherit.aes = TRUE,
  xfrom = -Inf,
  xto = Inf,
  width = 1,
  nudge_y = 0
)

geom_stripped_cols(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  show.legend = NA,
  inherit.aes = TRUE,
  yfrom = -Inf,
  yto = Inf,
  width = 1,
  nudge_x = 0
)

load("GSE33126.rdata")
gset <- gset[[1]]

sampleinfo <- pData(gset) %>% # 提取样本信息表
  select(source_name_ch1,characteristics_ch1.1) %>% 
  rename(group = source_name_ch1,patient=characteristics_ch1.1) %>% 
  mutate_at(vars(patient),~str_split(.," ",simplify = T)[,2]) %>%
  rownames_to_column(var="name")

gene_exp <- exprs(gset) %>% as.data.frame() %>% rownames_to_column(var="id")

p <- gene_exp %>% sample_n(30) %>% column_to_rownames("id") %>% 
  mutate_if(is.numeric, function(x) x+1) %>%
  log10() %>% rownames_to_column(var="id") %>% 
  pivot_longer(-id) %>% left_join(.,sampleinfo,by="name") %>% 
  mutate(patient=as.factor(patient)) %>% 
  ggplot(aes(name,value,fill=patient))+
  stat_summary(fun.data="mean_cl_normal",geom="errorbar",width=0.2) +
  stat_summary(fun = "mean",geom = "point",size=5,pch=21)+
  facet_wrap(.~group,scales = "free",labeller = label_wrap_gen(),nrow = 1)+
  scale_fill_npg()+
  theme_test()+
  theme(panel.spacing.x = unit(0.2,"cm"),
        panel.spacing.y = unit(0.1, "cm"),
        axis.title = element_blank(),
        strip.text.x = element_text(size=9,color="black"),
        axis.text = element_text(color="black"),
        axis.text.x=element_text(angle = 45,vjust=1,hjust=1),
        axis.ticks.x=element_blank(),
        legend.text = element_text(color="black",size=9),
        legend.title=element_blank(),
        legend.spacing.x=unit(0.1,'cm'),
        legend.key=element_blank(),
        legend.key.width=unit(0.4,'cm'),
        legend.key.height=unit(0.4,'cm'),
        legend.position = "top",
        plot.margin=unit(c(0.3,0.3,0.3,0.3),units=,"cm"))+
  guides(fill = guide_legend(direction = "horizontal"))+
  guides(fill=guide_legend(nrow=1, byrow=TRUE))

p + geom_stripped_cols()

p + geom_stripped_cols(width = 5)

p + geom_stripped_cols(width = 1)

p + geom_stripped_cols(width = 1, nudge_x = 1)

p + geom_stripped_cols(odd = "blue", even = "yellow", alpha = .1)+
  scale_x_discrete(expand = expansion(0, 0.5))
