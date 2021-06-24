library(tidyverse)
library(ppsr)
#Multiple linear regression (Day & Night with duration)
quantile(df_outliersremoved$height, .1)

# df_comp <- df_outliersremoved2 %>% filter(loadcarrying=="noload", length=="long") %>% select(ID,course, ExperienceLevel, day_night,pacecount) %>% spread(key=day_night, value=pacecount) %>% na.omit() %>% mutate(day_night_noload_delta=night-day) %>% select(ID, course, day_night_noload_delta, ExperienceLevel) %>% 
#         left_join(df_outliersremoved2 %>% filter(loadcarrying=="load", length=="long") %>% select(ID, course, day_night,pacecount) %>% spread(key=day_night, value=pacecount) %>% na.omit() %>% mutate(day_night_load_delta=night-day) %>% select(ID, day_night_load_delta) ) %>% 
#         dplyr::group_by(ID) %>% 
#         mutate(day_night_delta = mean (c(day_night_load_delta, day_night_noload_delta), na.rm = TRUE)) %>% 
#         ungroup() %>% 
#         mutate_at(vars(day_night_delta), scale) %>% 
#         mutate(ExperienceLevel2 = if_else(day_night_delta <= -.75, "advanced", if_else(day_night_delta <.8, "intermediate", "beginner"))) %>% select(ID, ExperienceLevel2)

pps <- df_outliersremoved %>%  select(gender:ExperienceLevel) %>% select(-length, -loadcarrying, -duration)
visualize_pps(pps, y="pacecount", do_parallel = TRUE, n_cores = 2)

pacecount_model_data <-  df_outliersremoved  %>% 
        filter(length=="long") %>%
        mutate(day_night = if_else(day_night=="day", 0, 1),
               loadcarrying = if_else(loadcarrying=="noload", 0, 1),
               gender=if_else(gender=="F", 0, 1),
               NVG = if_else(NVG =="No", 0, 1),
               experience = if_else(ExperienceLevel=="advanced", 0, if_else(ExperienceLevel=="intermediate", 1, 2))) 


#Day/noload model (height)
df_height <- df_outliersremoved %>%  filter(length=="long", loadcarrying=="load", day_night=="day" ) %>%         drop_na(pacecount)
df_height$height %>% quantile(probs=c(.05,.95), na.rm=TRUE)
lm_height <-  lm(pacecount ~ height, data=df_height)       
summary(lm_height)

pacecount_model <- lm(pacecount ~ gender + height + day_night + ruckweight + NVG + duration , data = df_outliersremoved  %>% filter(length=="long"))
summary(pacecount_model)

library(tidyverse)
library(moderndive)
library(skimr)
library(ISLR)
get_regression_table(pacecount_model)
get_regression_points(pacecount_model)

gender + height + day_night + ruckweight + NVG + ExperienceLevel
#robust linear regression rlm
#ridge regression linearRidge
pacecount_model_b <- lm(pacecount ~ day_night + height +  ruckweight , data = df_outliersremoved  %>% filter(length=="long"))
summary(pacecount_model_b)

pacecount_model_c <- lm(pacecount ~ day_night + ruckweight + height + experience , data = pacecount_model_data)
summary(pacecount_model_c)

model <- stepAIC(pacecount_model_b, direction='both')
summary(model)

library(MASS)
library(car)
library(caret)
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(pacecount ~., data = (df_outliersremoved  %>% filter(length=="long") %>% select(pacecount, day_night, height, weight, ExperienceLevel, ruckweight, gender, NVG) %>% drop_na()) ,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:8),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)

MASS::stepAIC(pacecount_model, direction = 'both' )



#plot of residuals (ALL Day and Night)

get_regression_points(pacecount_model) %>% 
        mutate(day_night = as.factor(day_night)) %>% 
        ggplot(aes(x=height, y=residual, color=day_night)) + 
        geom_point() + 
        geom_smooth() + 
        ylim(-15,15)

#####################
#Regression Trees
df_regressiontrees <- df_outliersremoved %>%  
        filter(length=="long") 
m <- rpart (pacecount ~ ExperienceLevel + height + day_night + ruckweight   , data = df_regressiontrees)
rpart.plot(m, digits=2, fallen.leaves = TRUE, type=5, extra=101)
summary(m)
predict_trees <- predict(m, df_regressiontrees)
regtree_importance <- as.data.frame(m$variable.importance) %>% as.data.frame()
max_importance <- max(regtree_importance$`m$variable.importance`)
regtree_report <- rownames_to_column(regtree_importance, var="Feature") %>%
        mutate(Importance = round(`m$variable.importance`/max_importance,1)) %>% 
        select ( Feature, Importance) %>%  arrange(-Importance)
regtree_report %>% ggplot () + geom_col(aes(x=reorder(Feature, Importance, fun=max), y=Importance), fill= "skyblue") + coord_flip() + xlab("")

regtree_report

library(Cubist)
df_modeltrees <- df_regressiontrees %>% select(ExperienceLevel, height, day_night, ruckweight)
m_2 <- cubist (y=df_regressiontrees$pacecount, x=df_modeltrees)
summary(m_2)

###############################################################
#plot of residuals (100m vs 600m)
pacecount_model_short <- lm(pacecount ~  height , 
                            data = (df %>% 
                                            filter(day_night=="day", loadcarrying=="noload", length=="short") %>%
                                            select(height, pacecount)))

pacecount_model_long <- lm(pacecount ~  height , 
                           data = (df %>%
                                           filter(day_night=="day", loadcarrying=="noload", length=="long") %>%
                                           select(height, pacecount)))

summary(pacecount_model_long)
get_regression_points(pacecount_model_short) %>% summarySE(measurevar = "residual")
get_regression_points(pacecount_model_long) %>% summarySE(measurevar = "residual")        
        
get_regression_points(pacecount_model_short) %>%
        mutate(residual = abs(residual)) %>% 
        rename(short = residual ) %>% 
        select(ID, short) %>%
        left_join(get_regression_points(pacecount_model_long) %>% 
                          mutate(residual = abs(residual)) %>% 
                      rename(long = residual ) %>% 
                      select(ID, long) ) %>% 
        gather(short:long, key= length, value=error) %>% 
        ggplot(aes(x=error, fill=length)) + 
        geom_density(alpha = .5) +
        xlab("residual (error)") +
        scale_fill_manual(name = "course length", values = c("green", "skyblue")) +
        theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
        ggsave("course_residuals.png", height = 4 , width = 6.5, units = "in")

residuals_shortandlong %>% mutate(residual = abs(residual)) %>% summarySE(measurevar = "residual", groupvars = "length")

#################################
#Modeling
df_outliersremoved %>% ggplot() + geom_density(aes(x=height, fill=gender), alpha = .5)
#Day/noload model
df_outliersremoved %>% select(ID, gender, height, pacecount, ExperienceLevel) %>% unique() %>% summarySE(groupvars = c("gender","height", "ExperienceLevel"), measurevar = "pacecount" ) %>% ggplot() + geom_col(aes(x=height, y=N, fill=gender)) +facet_grid(gender~ExperienceLevel)

##################
df3a <- pacecount_model_data2 %>% filter(height>=64 & height<=76, day_night==0, loadcarrying==0, length=="long") %>%  na.omit() 

summary(df3a)
lm1 <-  lm(pacecount ~ height + experience, data=df3a)

#Day/load model
df3b <- pacecount_model_data2 %>% filter(height>=64 & height<=76, day_night==0, loadcarrying==1, length=="long")%>% na.omit()
lm2 <-  lm(pacecount ~ height + experience , data=df3b)

#Night/Noload model
df3c <- pacecount_model_data2%>% filter(height>=64 & height<=76, day_night==1, loadcarrying==0, length=="long" )%>% na.omit()
lm3 <-  lm(pacecount ~ height + experience , data=df3c)

#Night/load model
df3d <- pacecount_model_data2 %>% filter(height>=64 & height<=76,day_night==1, loadcarrying==1, length=="long")%>% na.omit()
lm4 <-  lm(pacecount ~ height + experience , data=df3d)

#Data table based on height and model output
height <- c(58:76) 
height <- data.frame(height)
experience <- 1
pacecount_table1 <- data.frame(height, experience)
day_noload <- round(predict(lm1, pacecount_table1), 0)

experience <- 1
pacecount_table2 <- data.frame(height, experience)
day_load <- round(predict(lm2, pacecount_table2), 0)

experience <- 1
pacecount_table3 <- data.frame(height, experience)
night_noload <- round(predict(lm3, pacecount_table3),0)

experience <- 1
pacecount_table4 <- data.frame(height, experience)
night_load <- round(predict(lm4, pacecount_table4),0)

pacecount_table_final <- height %>% cbind(day_noload, day_load, night_noload, night_load) 

experience <- 0
pacecount_table1 <- data.frame(height, experience)
day_noload2 <- round(predict(lm1, pacecount_table1), 0)

experience <- 0
pacecount_table2 <- data.frame(height, experience)
day_load2 <- round(predict(lm2, pacecount_table2), 0)

experience <- 0
pacecount_table3 <- data.frame(height, experience)
night_noload2 <- round(predict(lm3, pacecount_table3),0)

experience <- 0
pacecount_table4 <- data.frame(height, experience)
night_load2 <- round(predict(lm4, pacecount_table4),0)

pacecount_table_final2 <- height %>% cbind(day_noload2, day_load2, night_noload2, night_load2)

experience <- 2
pacecount_table1 <- data.frame(height, experience)
day_noload3 <- round(predict(lm1, pacecount_table1), 0)

experience <- 2
pacecount_table2 <- data.frame(height, experience)
day_load3 <- round(predict(lm2, pacecount_table2), 0)

experience <- 2
pacecount_table3 <- data.frame(height, experience)
night_noload3 <- round(predict(lm3, pacecount_table3),0)

experience <- 2
pacecount_table4 <- data.frame(height, experience)
night_load3 <- round(predict(lm4, pacecount_table4),0)

pacecount_table_final3 <- height %>% cbind(day_noload3, day_load3, night_noload3, night_load3)

comparison_advanced <- pacecount_table_final %>%  
        left_join(pacecount_table_final2, by="height") %>% 
        mutate(day_noload_delta=day_noload2-day_noload,
               day_load_delta=day_load2-day_load, 
               night_noload_delta=night_noload2-night_noload, 
               night_load_delta=night_load2-night_load) %>% 
        select(height, day_noload_delta, day_load_delta, night_noload_delta, night_load_delta)

comparison_beginner <- pacecount_table_final %>%  
        left_join(pacecount_table_final3, by="height") %>% 
        mutate(day_noload_delta=day_noload3-day_noload,
               day_load_delta=day_load3-day_load, 
               night_noload_delta=night_noload3-night_noload, 
               night_load_delta=night_load3-night_load) %>% 
        select(height, day_noload_delta, day_load_delta, night_noload_delta, night_load_delta)
##################################
#Modeling height 58 to 76 inches
#Day/noload model
df3a2 <- pacecount_model_data2 %>% filter(height>=58 & height<=76, day_night==0, loadcarrying==0, length=="long") %>%  na.omit()
summary(df3a2)
lm1 <-  lm(pacecount ~ height + experience, data=df3a2)


#Day/load model
df3b2 <- pacecount_model_data2 %>% filter(height>=58 & height<=76, day_night==0, loadcarrying==1, length=="long")%>% na.omit()
lm2 <-  lm(pacecount ~ height + experience , data=df3b2)


#Night/Noload model
df3c2 <- pacecount_model_data2%>% filter(height>=58 & height<=76, day_night==1, loadcarrying==0, length=="long" )%>% na.omit()
lm3 <-  lm(pacecount ~ height + experience , data=df3c2)


#Night/load model
df3d2 <- pacecount_model_data2 %>% filter(height>=58 & height<=76,day_night==1, loadcarrying==1, length=="long")%>% na.omit()
lm4 <-  lm(pacecount ~ height + experience , data=df3d2)

#Data table based on height and model output
height <- c(58:76) 
height <- data.frame(height)
experience <- 1
pacecount_table1 <- data.frame(height, experience)
day_noload <- round(predict(lm1, pacecount_table1), 0)

experience <- 1
pacecount_table2 <- data.frame(height, experience)
day_load <- round(predict(lm2, pacecount_table2), 0)

experience <- 1
pacecount_table3 <- data.frame(height, experience)
night_noload <- round(predict(lm3, pacecount_table3),0)

experience <- 1
pacecount_table4 <- data.frame(height, experience)
night_load <- round(predict(lm4, pacecount_table4),0)

pacecount_table_final_b <- height %>% cbind(day_noload, day_load, night_noload, night_load) %>% filter(height !="65")

experience <- 0
pacecount_table1 <- data.frame(height, experience)
day_noload2 <- round(predict(lm1, pacecount_table1), 0)

experience <- 0
pacecount_table2 <- data.frame(height, experience)
day_load2 <- round(predict(lm2, pacecount_table2), 0)

experience <- 0
pacecount_table3 <- data.frame(height, experience)
night_noload2 <- round(predict(lm3, pacecount_table3),0)

experience <- 0
pacecount_table4 <- data.frame(height, experience)
night_load2 <- round(predict(lm4, pacecount_table4),0)

pacecount_table_final2_b <- height %>% cbind(day_noload2, day_load2, night_noload2, night_load2) %>% filter(height !="65")

experience <- 2
pacecount_table1 <- data.frame(height, experience)
day_noload3 <- round(predict(lm1, pacecount_table1), 0)

experience <- 2
pacecount_table2 <- data.frame(height, experience)
day_load3 <- round(predict(lm2, pacecount_table2), 0)

experience <- 2
pacecount_table3 <- data.frame(height, experience)
night_noload3 <- round(predict(lm3, pacecount_table3),0)

experience <- 2
pacecount_table4 <- data.frame(height, experience)
night_load3 <- round(predict(lm4, pacecount_table4),0)

pacecount_table_final3_b <- height %>% cbind(day_noload3, day_load3, night_noload3, night_load3) %>% filter(height !="65")

comparison_advanced_b <- pacecount_table_final_b %>%  
        left_join(pacecount_table_final2_b, by="height") %>% 
        mutate(day_noload_delta=day_noload2-day_noload,
               day_load_delta=day_load2-day_load, 
               night_noload_delta=night_noload2-night_noload, 
               night_load_delta=night_load2-night_load) %>% 
        select(height, day_noload_delta, day_load_delta, night_noload_delta, night_load_delta)

comparison_beginner_b <- pacecount_table_final_b %>%  
        left_join(pacecount_table_final3_b, by="height") %>% 
        mutate(day_noload_delta=day_noload3-day_noload,
               day_load_delta=day_load3-day_load, 
               night_noload_delta=night_noload3-night_noload, 
               night_load_delta=night_load3-night_load) %>% 
        select(height, day_noload_delta, day_load_delta, night_noload_delta, night_load_delta)

###Write datafiles#############################
pacecount_table_final_intermediate <- pacecount_table_final
write.csv(pacecount_table_final_intermediate, "pacecount_table_intermediate.csv")

pacecount_table_final_advanced <- pacecount_table_final2
write.csv(pacecount_table_final_advanced, "pacecount_table_advanced.csv")

pacecount_table_final_beginner <-pacecount_table_final3
write.csv(pacecount_table_final_beginner, "pacecount_table_beginner.csv")
