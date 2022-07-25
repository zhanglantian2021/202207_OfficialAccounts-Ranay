library(tidyverse)

freedom <- read_csv('freedom.txt')

seg_size <- 0.01

freedom_clean <- freedom %>%
  mutate(CL = 8 - CL,
         PR = 8 - PR) %>%
  filter(year %in% c(1995,2020)) %>%
  group_by(country) %>%
  mutate(gap_cl = CL - lag(CL),
         gap_pr = PR - lag(PR)) %>%
  ungroup() %>%
  filter(!is.na(gap_cl), !is.na(gap_pr)) 


seg_cl <- tibble(x = seq(0.8,7.2,seg_size),
                  xend = seq(0.8,7.2,seg_size),
                  y = rep(-0.1, 6.4/seg_size +1),
                  yend = rep(-0.4, 6.4/seg_size +1)) %>%
   mutate(color = x^2)
 
 seg_pr <- tibble(x = rep(0-0.1, 6.4/seg_size +1),
                  xend = rep(-0.4, 6.4/seg_size +1),
                  y = seq(0.8,7.2,seg_size),
                  yend = seq(0.8,7.2,seg_size))%>%
   mutate(color = y^2)
 
 signs <- tibble(x = c(-0.25, -0.25, 1.25, 6.75),
                 y = c(1.25, 6.75, -0.25, -0.25),
                 text = c("-", "+", "-", "+"))
 
 seg <- bind_rows(seg_cl, seg_pr)
 
 
freedom_clean %>%
   filter(!is.na(gap_cl)) %>%
   count(CL, PR) %>%
   arrange(n) %>%
   mutate(prod = CL * PR) %>%
   ggplot() +
   scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
   geom_point(aes(CL, PR, size = n, color = prod)) +
   geom_text(aes(CL, PR,label = n), size = 4,fontface = "bold")+
   geom_segment(data = seg, aes(x = x, xend = xend, y = y, yend = yend, color = color))+
   geom_text(data = signs, aes(x = x, y = y, label = text),size = 5, fontface = "bold")+
   annotate("text", x = 4, y = -0.25, label = "Civil Liberties", size = 4, fontface = "bold") +
   annotate("text", x = -0.25, y = 4, label = "Political Rights",size = 4, fontface = "bold", angle = 90) +
   scale_size(range= c(5, 25)) +
   guides(color = "none", size = "none") +
   theme_void()+
   theme(plot.background = element_rect(fill="Aliceblue",color="Aliceblue"),
         plot.margin = unit(c(0.2,0.5,0.2,0.2),"cm"))



