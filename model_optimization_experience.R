
experience <- function(advanced_z, beginner_z){ 
     summary(

     lm(pacecount ~  height + day_night + ruckweight + experience  , data = 
        read_xlsx("PaceCountData_1Sept.xlsx") %>%
        rbind(read_xlsx("PaceCountData_13OCT.xlsx") %>%
                   mutate(duration=duration_min+(duration_sec/60)) %>% 
                   select(-duration_min, -duration_sec)) %>%
        rbind(read_xlsx("PaceCountData_19Oct.xlsx") %>% 
                   mutate(duration=duration_min+(duration_sec/60)) %>% 
                   select(-duration_min, -duration_sec)) %>% 
        rbind(read_xlsx("PaceCountData_09NOV.xlsx") %>% 
                   mutate(duration=duration_min+(duration_sec/60)) %>% 
                   select(-duration_min, -duration_sec)) %>%
        mutate(duration=round(duration, 1)) %>%
        drop_na(pacecount)  %>%
        filter(course!="ALC")  %>% #remove ALC
        mutate(NVG = if_else(course =="RSLC_Cadre" & day_night=="night"|course == "RSLC_Students" & day_night=="night", "Yes", "No")) %>% 
        mutate(loadcarrying = factor(loadcarrying, levels = c("noload", "load"))) %>% 
        mutate(gender = factor(gender, levels=c("M", "F"))) %>% 
        mutate(duration = if_else(course=="RSLC_Cadre" & day_night=="night" & loadcarrying=="load", 0, duration)) %>% 
        mutate(duration = ifelse(duration==0,NA,duration)) %>% 
        right_join(
             df %>%  
                  filter(loadcarrying=="noload", day_night =="night", length=="long") %>% 
                  mutate_at(vars(duration), scale)  %>% 
                  rbind(
                       df %>%  
                            filter(loadcarrying=="noload", day_night =="day", length=="long") %>% 
                            mutate_at(vars(duration), scale)  
                  ) %>% 
                  select(ID, day_night, duration) %>% 
                  pivot_wider(names_from = "day_night", values_from = "duration")  %>% 
                  group_by(ID) %>% 
                  mutate(duration = if_else(is.na(night), day, night )) %>% 
                  mutate(ExperienceLevel = if_else(duration <= advanced_z, "advanced", if_else(duration< beginner_z, "intermediate", "beginner"))) %>% 
                  mutate(ExperienceLevel =  replace_na(ExperienceLevel, "intermediate")) %>% 
                  select(ID, ExperienceLevel) 
        ) %>% 
             mutate(day_night = if_else(day_night=="day", 0, 1),
                    loadcarrying = if_else(loadcarrying=="noload", 0, 1),
                    gender=if_else(gender=="F", 0, 1),
                    NVG = if_else(NVG =="No", 0, 1),
                    experience = if_else(ExperienceLevel=="advanced", 0, if_else(ExperienceLevel=="intermediate", 1, 2)))
        )
     )
}

experience(-.3,.8)
