library(tidyverse)
library(magrittr)

iris %>% as_tibble() %>%
  group_by(Species) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm=TRUE)))


otu <- read.delim("otu.xls",check.names = F)
group <- read.delim("group.xls")


otu %>% left_join(.,group,by="sample") %>% 
  select(sample,group,everything()) %>%
  pivot_longer(-c(sample,group)) %>% 
  group_by(group,name) %>%
  summarise(sum=sum(value),.groups = 'drop') %>% 
  pivot_wider(names_from=name, values_from=sum)


otu %>% left_join(.,group,by="sample") %>% 
  select(sample,group,everything()) %>%
  group_by(group) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm=TRUE))) %>% 
  column_to_rownames("group") %>% t() %>% as.data.frame() %>% 
  rownames_to_column("otu")



data <- data.frame(A1=1:10,A2=11:20,A3=12:21,
                   B2=80:89,B1=12:21,B3=15:24)

data

result <- list(); b <- 3
for (i in 1:(data %>% ncol() / b)) {
  result[[i]] <- data %>%
    select((b * i - b + 1):(b * i) %>% all_of()) %>%
    apply(1, mean)
}

result %>% as.data.frame() %>% 
  set_colnames(c("A","B"))


data %>% t() %>% as.data.frame() %>% 
  mutate(group=rep(LETTERS[1:2],times=c(3,3))) %>%
  group_by(group) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  column_to_rownames("group") %>% t() %>% as.data.frame()
  



