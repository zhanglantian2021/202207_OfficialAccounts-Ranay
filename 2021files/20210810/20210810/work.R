library(tidyverse)
library(ggtext)

food_consumption <- readr::read_csv('food_consumption.csv')

# 绘制极坐标条形图，首先做一个标签
label_data2 <- food_consumption %>% group_by(country) %>%
  summarise(sum=sum(co2_emmission)) %>%
  arrange(desc(sum))
label_data2$id <- c(1:nrow(label_data2))

# 计算标签的角度
number_of_bar <- nrow(label_data2)
angle <-  90 - 360 * ( label_data2$id-.5 )/number_of_bar

# 计算标签的对齐方式：右对齐还是左对齐
# 如果我在图的左侧，我的标签当前的角度 < -90
# 0 表示左对齐
# 1 表示右对齐

label_data2$hjust<-ifelse( angle < -90,1,0)

# 翻转角度
label_data2$angle<-ifelse(angle < -90,angle+180,angle)

order <- label_data2$country %>% c() %>%
  factor(.,levels=order)


pal <- c("#BC3C29FF","#0072B5FF","#E18727FF")

p <- food_consumption %>%
  group_by(country) %>%
  mutate(food_category2 = case_when(food_category %in% c("Beef","Fish",
                                                         "Lamb & Goat","Pork","Poultry") ~ "Meat",
                                    food_category %in% c("Eggs",
                                                         "Milk - inc. cheese") ~ "Eggs & Dairy",
                                    food_category %in% c("Nuts inc. Peanut Butter",
                                                         "Rice","Soybeans",
                                                         "Wheat and Wheat Products") ~ "Nuts & Grains")) %>%
  mutate(food_category2 = factor(food_category2,
                                 levels = c("Meat","Eggs & Dairy","Nuts & Grains"))) %>%
  ggplot( aes(fill=food_category2, y=co2_emmission,x=country)) + 
  geom_bar(position="stack",stat="identity") +
  scale_x_discrete(limits=order)+
  scale_fill_manual(values=pal,guide=F)+
  ylim(-1500,2250) +  # 添加 ylim 以保持中间的空圆圈
  theme_void() +
  coord_polar(direction = 1,
              clip = "off") +
  geom_text(data=label_data2,aes(x=id,y=sum+50,
                                  label=country,hjust=hjust), 
            color="black",fontface="plain",alpha=0.6,size=2, 
            angle=label_data2$angle,inherit.aes=FALSE )
p

ggsave(p,filename = "bar.png",height=9,width =9,dpi=300,units="in")


