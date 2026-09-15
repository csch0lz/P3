library('tidyverse')
library('writexl')

file=list.files('~/Nextcloud/ASCOR-FMG-17214-P3-LayPsychology (Projectfolder)/data/', pattern = 'P3-Lay', full.names = TRUE)
df=read_csv(file,col_types=cols()) |> 
  slice(-c(1,2)) |> 
  filter(DistributionChannel=='anonymous') 

df=df |> 
  #Screen
  mutate(screenOut=case_when(Dutch==1 & country %in% c(1,121) & age_1>=16 & consent=="1"~0,
                             TRUE~1)) |> 
  #Attention checks
  mutate(attention=0,
         attention=if_else(attention_1==4, attention+1, attention, missing=attention),
         attention=if_else(attention_2==4, attention+1, attention,missing=attention),
         number_video=as.character(number_video),
         attention=if_else(grepl('3',number_video) & grepl('1',number_video) & grepl('6',number_video) & grepl('9',number_video), attention+1, attention, missing=attention),
         attentionPassed=if_else(attention>=1,1,0), 
         completed=if_else(!is.na(political_leaning),1,0),
         dur_min=as.numeric(`Duration (in seconds)`)/60
        ) 

# From Pre-Registration
#In addition, before analyzing any of the variables of interest, we will examine the distribution 
#of the duration of each person's participation in concert with other quality measures like 
#straightlining and answers to open questions. 
#Based on these quality checks, we will make an informed decision and potentially exclude 
#participants who were speeding through the survey, which is estimated to take about 15-20 min. 

df |> 
  filter(screenOut==0,
         #From pre-reg: #Participants who fail two out of three attention checks while completing the survey will be excluded.
         attentionPassed==1,
         completed==1,
         !is.na(psid), psid!='') |> 
  filter(dur_min<30) |> 
  ggplot(aes(x=dur_min))+
  geom_histogram()+
  labs(x='Duration in min') +
  #add breaks to x-axis such that seq(1,30,5)
  scale_x_continuous(breaks = seq(0,30,5)) 

# Identify the cutoff for the lowest 90% in duration (excluding extreme outliers who just left the survey on for a long time)
cutoff=df |> 
  filter(screenOut==0,
         #From pre-reg: #Participants who fail two out of three attention checks while completing the survey will be excluded.
         attentionPassed==1,
         completed==1,
         !is.na(psid), psid!='') |> 
  summarize(q=quantile(dur_min,0.90, na.rm=T)) |> pull(q)

# Get the mean and sd of the lowest 90% in duration. 
#People who are faster than mean-1.5*sd (based on fasted 90%) will be removed
min_dur=df |> 
  filter(screenOut==0,
         #From pre-reg: #Participants who fail two out of three attention checks while completing the survey will be excluded.
         attentionPassed==1,
         completed==1,
         !is.na(psid), psid!='') |> 
  filter(dur_min <= cutoff) |> summarize(mean_dur = mean(dur_min, na.rm=TRUE), sd_dur=sd(dur_min,na.rm=TRUE), cut_at=mean_dur-1.5*sd_dur) |> pull(cut_at)

#People who are faster than mean-sd (based on fasted 90% will be investigated)
inv_dur=df |> 
  filter(screenOut==0,
         #From pre-reg: #Participants who fail two out of three attention checks while completing the survey will be excluded.
         attentionPassed==1,
         completed==1,
         !is.na(psid), psid!='') |> 
  filter(dur_min <= cutoff) |> 
  summarise(mean_dur = mean(dur_min, na.rm = TRUE), sd_dur = sd(dur_min, na.rm = TRUE), cut_at=mean_dur-sd_dur) |> pull(cut_at)

df=df |> 
  mutate(time_exclusion=if_else(dur_min<min_dur,1,0,missing=0),
         time_straightliner_exclusion=if_else((dur_min<inv_dur & dur_min>=min_dur & Q_StraightliningPercentage>0.75), 1, 0, missing=0))

df |> 
  filter(screenOut==0,
         #From pre-reg: #Participants who fail two out of three attention checks while completing the survey will be excluded.
         attentionPassed==1,
         completed==1,
         !is.na(psid), psid!='') |> summarize(min_dur_exclusions=sum(dur_min<min_dur),#30 poeple excluded
                                              investigated=sum(dur_min<inv_dur & dur_min>=min_dur),# 64 people
                                              excluded_after_investigation=sum(dur_min<inv_dur & dur_min>=min_dur & Q_StraightliningPercentage>0.75))  # 3 people


#Write dataset for coding
#write_xlsx(df|>  filter(screenOut==0,
         #attentionPassed==1,
         #completed==1,
         #!is.na(psid), psid!='', time_exclusion==0, time_straightliner_exclusion==0)>  select(ResponseId, number_1,starts_with('conv_topic')), '/data/clean_data_CODING_31_3_26.xlsx')
#old=readxl::read_xlsx('/data/clean_data_CODING_12_2_26.xlsx')  
#new=df_clean |> select(ResponseId, number_1,starts_with('conv_topic'))
#new |> filter(!(ResponseId %in% old$ResponseId)) |> write_xlsx('../data/clean_data_RestCODING_31_3_26.xlsx')

# Incorporate coding data
library('readxl')
coded_data=read_xlsx('data/combined_coded_data.xlsx') |> 
  select(ResponseId,number_1_cleaned,conv_topic_REASSIGN,conv_topic_FLAG) 

  #figure out who hasn't been coded
df= df |>  left_join(coded_data)

df = df |>  
  mutate(conv_topic=if_else(conv_topic_FLAG==1,NA,conv_topic,missing=conv_topic),
        conv_topic=if_else(!is.na(conv_topic_REASSIGN),conv_topic_REASSIGN, conv_topic),
        conv_topic_REASSIGNED=if_else(!is.na(conv_topic_REASSIGN),1, 0)) |> 
  select(-number_1,-conv_topic_REASSIGN) 

#Clean and Score Variables
df = df |> 
  mutate(political_leaning=na_if(political_leaning,'-99'))

df=df|> mutate(conv_topic_char=case_when(!is.na(conv_topic) & conv_topic=='1'~"Health",
                                     !is.na(conv_topic) & conv_topic=='2'~'Politics',
                                     !is.na(conv_topic) & conv_topic=='3'~'Environment',
                                     !is.na(conv_topic) & conv_topic=='4'~'Other',
                                     TRUE~NA_character_),
                conv_topic_char=factor(conv_topic_char,levels=c('Health','Politics','Environment','Other')))

df = df |> 
  mutate(across(starts_with("TFD"), as.numeric))|> 
  rowwise() |>
  mutate(
  #Tolerance for Disagreement
   TFD_pos = sum(c_across(c(TFD_1, TFD_2, TFD_5, TFD_7, TFD_8, TFD_14, TFD_15)), na.rm = TRUE),
   TFD_neg = sum(c_across(c(TFD_3, TFD_4, TFD_6, TFD_9, TFD_10, TFD_11, TFD_12, TFD_13)), na.rm = TRUE),
   TFD = 48 + TFD_pos - TFD_neg) |>  
  ungroup()

df = df |>
  mutate(across(starts_with("depth_"), as.numeric))|> 
  rowwise() |>
  mutate(
    depth = mean(c_across(c(depth_1,depth_2,depth_3,depth_4,depth_5,depth_6,depth_7)), na.rm = TRUE)
  ) |> ungroup()

df = df |> 
  mutate(across(starts_with("process_PDQI_"), as.numeric))|> 
  rowwise() |>
  mutate(
    PDQI = mean(c_across(c(process_PDQI_1,process_PDQI_2,process_PDQI_3,process_PDQI_4,process_PDQI_5,process_PDQI_6,process_PDQI_7)), na.rm = TRUE)
  ) |> ungroup()

df = df |> 
  mutate(across(starts_with('process_general'), as.numeric))|> 
  rowwise() |>
  mutate(
    PDQI_general = mean(c_across(c(process_general_1, process_general_3)), na.rm = TRUE)
  ) |>
  ungroup()

df = df |> 
  mutate(across(starts_with('IH_'), as.numeric))|> 
  rowwise() |>
  mutate(
    IH = mean(c_across(c(IH_1:IH_6)), na.rm = TRUE)
  ) |> ungroup()

#Outcomes
# small helper function to deal with situations where both values are NA (participants that will be excluded anyways)
max_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

df = df |> 
  mutate(across(starts_with('outcome_'), as.numeric)) |> 
  #set Don't know to NA
  mutate(across(starts_with("outcome_"), ~ na_if(., -99))) |> 
  rowwise() |>
  mutate(
    outcome_attitude_change = max_or_na(c_across(c(outcome_3, outcome_4))),
    outcome_behavior_change = max_or_na(c_across(c(outcome_8, outcome_9))),
    outcome_general_attitude_change = max_or_na(c_across(c(outcome_general_3, outcome_general_4))),
    outcome_general_behavior_change = max_or_na(c_across(c(outcome_general_8, outcome_general_9)))
  )  |> ungroup()

write_csv(df, 'data/clean_study1_data.csv')
