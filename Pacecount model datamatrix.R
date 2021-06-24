
#Day/noload model
df_shortcourse <- df_outliersremoved %>%  filter(length=="short" ) %>% drop_na(pacecount)
lm_shortcourse <-  lm(pacecount ~ height, data=df_shortcourse)       
summary(lm_shortcourse)

df3a <- df_outliersremoved %>% 
     filter(day_night=="day", loadcarrying=="noload", length=="long" ) %>% 
     na.omit()
summary(df3a)

lm1 <-  lm(pacecount ~ height + duration, data=df3a)
summary(lm1)
lm1_duration <- median(df3a$duration)

#Day/load model
df3b <- df_outliersremoved %>% filter(day_night=="day", loadcarrying=="load", length=="long" )%>% na.omit()
lm2 <-  lm(pacecount ~ height + duration , data=df3b)
summary(lm2)
lm2_duration <- median(df3b$duration)

#Night/Noload model
df3c <- df_outliersremoved %>% filter(day_night=="night", loadcarrying=="noload", length=="long" )%>% na.omit()
lm3 <-  lm(pacecount ~ height + duration , data=df3c)
summary(lm3)
lm3_duration <- median(df3c$duration)

#Night/load model
df3d <- df_outliersremoved %>% filter(day_night=="night", loadcarrying=="load", length=="long" )%>% na.omit()
lm4 <-  lm(pacecount ~ height + duration , data=df3d)
summary(lm4)
lm4_duration <- median(df3d$duration)

#Data table based on height and model output
height <- c(58:76) 
height <- data.frame(height)
duration <- lm1_duration
pacecount_table1 <- data.frame(height, duration)
day_noload <- round(predict(lm1, pacecount_table1), 0)

duration <- lm2_duration
pacecount_table2 <- data.frame(height, duration)
day_load <- round(predict(lm2, pacecount_table2), 0)

duration <- lm3_duration
pacecount_table3 <- data.frame(height, duration)
night_noload <- round(predict(lm3, pacecount_table3),0)

duration <- lm4_duration
pacecount_table4 <- data.frame(height, duration)
night_load <- round(predict(lm4, pacecount_table4),0)

shortcourse <- round(predict(lm_shortcourse, height), 0)

pacecount_table_final <- height %>% cbind(shortcourse, day_noload, day_load, night_noload, night_load)

write.csv(pacecount_table_final, "PacecountTable.csv")

library(tidyverse)
summary(pacecount_table_final %>% filter(height <72 & height>64) %>% mutate(error = night_load/day_noload))
