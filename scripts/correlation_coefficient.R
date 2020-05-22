library('tidyverse')
library('ggplot')

table_1 <- read_csv("data_clean/data_table_1.csv")

current_smokers <- table_1$current_smoker
former_smokers <- table_1$former_smoker

current_former_smokers <- coalesce(table_1$current_smoker, table_1$current_former_smoker, table_1$former_smoker)

missing_smokers <- coalesce(table_1$not_stated, table_1$missing)

cor.test(current_smokers, missing_smokers)
cor.test(former_smokers, missing_smokers)
cor.test(current_former_smokers, missing_smokers)


ggplot(data = table_1, aes(x = current_former_smokers,y = missing_smokers))+
  geom_point()