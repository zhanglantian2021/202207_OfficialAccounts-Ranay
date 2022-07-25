package.list=c("tidyverse","reshape","psych","RColorBrewer","magrittr","ggh4x")

for (package in package.list) {
  if (!require(package,character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
table1 <- read.delim("env.xls",header =T,sep="\t",row.names = 1,check.names = F)

table2 <- read.delim("genus.xls",header =T,sep="\t",row.names = 1,check.names = F) %>% 
  t() %>% as.data.frame()

pp <- corr.test(table1,table2,method="pearson",adjust = "fdr")

cor <- pp$r
pvalue <- pp$p

df <- melt(cor) %>% mutate(pvalue=melt(pvalue)[,3],
                     p_signif=symnum(pvalue, corr = FALSE, na = FALSE,  
                                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                     symbols = c("***", "**", "*", "", " "))) %>% 
  set_colnames(c("env","genus","r","p","p_signif"))

ridiculous_strips <- strip_themed(
  background_y = elem_list_rect(
    fill =  c("#DE9ED6FF","#709AE1FF","#D2AF81FF"))
)

df %>% left_join(.,read_tsv('annotation.xls'),by=c("genus")) %>% 
  ggplot(.,aes(env,genus,col=r,fill=r))+
  geom_tile(color="grey80",fill="white",size=0.3)+
  geom_point(aes(size =abs(r)),shape=21)+
  geom_text(aes(label=p_signif),size=4,color="white",hjust=0.5,vjust=0.7)+
  facet_grid2(group~.,scale="free_y",switch = "y",strip = ridiculous_strips)+
  labs(x = NULL,y = NULL,color=NULL,fill=NULL)+
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
  scale_x_discrete(expand=c(0,0),
                   labels=c("NH4+"=expression(NH[4]^+""),
                            "NO2--"=expression(NO[2]^-""),
                            "CuSO4"=expression(CuSO[4])))+
  scale_y_discrete(expand=c(0,0),position = 'right') +
  theme(axis.text.x=element_text(angle =45,hjust =1,vjust =1,
                                 color="black",face = "bold",size = 10),
        axis.text.y=element_text(color="black",face = "bold",size =10),
        axis.ticks= element_blank(),
        panel.spacing.y = unit(0,"cm"))+
  scale_size(range=c(1,10),guide=NULL)+
  guides(color=guide_colorbar(direction="vertical",reverse=F,barwidth=unit(.5,"cm"),
                               barheight=unit(15,"cm")))



