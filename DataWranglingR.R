rm(list=ls())
library(tidyverse)

?diamonds

#subset by row with filter()
diamonds_sm = filter(diamonds, cut == "Ideal")
diamonds_sm = filter(diamonds, price > 10000)
View(diamonds_sm)

diamonds_sm = filter(diamonds, cut == "Ideal", price >10000)  #and
diamonds_sm = filter(diamonds, cut == "Ideal" | price >10000) #or

#filter for missing values using is.na or !is.na

#subset by column with select()


#reorder rows with arrange()
diamonds_sm = select(diamonds, cut, color)

diamonds_sm = select(diamonds, 1:4)

diamonds_sm = select(diamonds, starts_with("c"))
diamonds_sm = select(diamonds, contains("c"))

diamonds_sm = select(diamonds, price, everything())

diamonds_sm = select(diamonds, -price)

#using the pipe
diamonds_sm = diamonds %>%
  select(-price)

#reorder rows with arrange()
diamonds_arr = diamonds %>% 
  arrange(color)

diamonds_arr = diamonds %>% 
  arrange(carat)

diamonds_arr = diamonds %>% 
  arrange(desc(carat))

diamonds_arr = diamonds %>% 
  arrange(color, carat)

diamonds_arr = diamonds %>% 
  arrange(carat, color)

glimpse(diamonds_arr)

#add or modify columns with mutate()
diamonds_new = diamonds %>% 
  mutate(mass_g = .20 * carat)
glimpse(diamonds_new)

diamonds_new = diamonds %>% 
  mutate(mass_g = .20 * carat,
         price_per_carat = price / carat)
glimpse(diamonds_new)

diamonds_new = diamonds %>% 
  mutate(mass_g = .20 * carat,
         price_per_carat = price / carat,
         cut = tolower(cut))
glimpse(diamonds_new)

diamonds_new = diamonds %>% 
  mutate(mass_g = .20 * carat,
         price_per_carat = price / carat,
         cut = tolower(cut),
         expensive_TF = price > 10000)
glimpse(diamonds_new)

#Other Smaller Verbs
?slice_max
?bind_rows
?left_join
?rename
?case_when

#grouped summaries with group_by() and summarize()
diamonds %>% 
  group_by(cut) %>% 
  summarize(mean(price))

diamonds %>% 
  group_by(cut) %>% 
  summarize(avg_price = mean(price))

diamonds %>% 
  group_by(cut) %>% 
  summarize(avg_price = mean(price),
            sd_price = sd(price))

diamonds %>% 
  group_by(cut,
           color) %>% 
  summarize(avg_price = mean(price),
            sd_price = sd(price),
            count= n()) #count

diamonds %>% 
  count(cut,
        color) # counts only

diamonds %>% 
  group_by(expensive = price > 10000) %>% 
  summarize(avg_price = mean(price),
            sd_price = sd(price),
            count= n())