library(vegan)

library(ggcor)

library(ggplot2)#一定要加载

varechem<-read.csv("varechem.csv")
varespec<-read.csv("varespec.csv")


mantel <- mantel_test(varespec, varechem,
                      spec.select = list(Spec01 = 1:1,
                                         Spec02 = 2:2,
                                         Spec03 = 3:3 )) %>% 
 
 mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),        
                 labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
        pd = cut(p.value, breaks = c(-Inf, 0.01, 0.05, Inf),
                 labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

quickcor(varechem, type = "upper") + #主要性状矩阵的相关热图，右上方
  geom_square() +
  anno_link(aes(colour = pd, size = rd), data = mantel) + #r,p,值来源于那个表data
  scale_size_manual(values = c(0.5, 1, 2)) +   
  scale_colour_manual(values = c("#D95F02", "#1B9E77", "#A2A2A288")) +
  guides(size = guide_legend(title = "Mantel's r",  
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p",  
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))

