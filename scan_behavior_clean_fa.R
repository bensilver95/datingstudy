library(tidyverse)
library(psych)

setwd('/Users/benjaminsilver/Google Drive/My Drive/Grad School/Projects/Motivated_Replay/data/')
d_raw <- read_csv('real/fmri_ratings_allqs.csv')

d <- d_raw %>% 
  select(-c(assess_video_onset,assess_video_duration,
            reveal_video_onset,reveal_video_duration,
            assess_memory_onset,reveal_memory_onset,
            slider_assess.started,slider_reveal.started,
            slider_assess.ended,slider_reveal.ended,
            feedback_onset)) %>% 
  rename(assess_slider = "slider_assess.response",
         reveal_slider = "slider_reveal.response",
         assess_memorytime = "assess_memory_duration",
         reveal_memorytime = "reveal_memory_duration",
         assess_memoryacc = "assess_memory_acc",
         reveal_memoryacc = "reveal_memory_acc") %>% 
  mutate(start.d = if_else(assess_slider < 5, 0, 1),
         feedback.d = if_else(feedback == "neg",0,1),
         feedback = recode(feedback, "neg" = "Negative Feedback",
                           "pos" = "Positive Feedback"),
         congruence = if_else(start.d == feedback.d, "Congruent", "Incongruent"),
         congruence.d = if_else(congruence == "Congruent",1,0)) %>% 
  pivot_longer(cols = c(contains("assess"),contains("reveal")),
               names_to = c("RatingType",".value"),
               values_to = "Difficulty",
               names_sep = "_"
  ) %>% 
  mutate(#slider = as.double(na_if(slider, "None")),
    slider.c = scale(slider,center = T,scale = F),
    RatingType = recode(RatingType, "assess" = "Pre-Feedback video",
                        "reveal" = "Post-Feedback video"),
    RatingType = fct_relevel(RatingType,"Pre-Feedback video","Post-Feedback video"),
    RatingType.d = if_else(RatingType == "Pre-Feedback video",0,1),
    question_short = recode(question,
                            "How romantically compatible do you believe you are with this person?" = "Compatible",
                            "How similar do you believe you are to this person?" = "Similar",
                            "How physically attractive do you find this person?" = 'Attractive',
                            "How interested were you in what this person was talking about?" = "Interested",
                            "How much do you like this person?" = "Liking"))

## reduce down to one dimension with factor analysis ####

pca_fa <- d %>% 
  #filter(question_short != 'Attractive') %>% 
  select(sub, name, question, RatingType,slider) %>% 
  pivot_wider(id_cols = c(sub,name,RatingType),
              names_from = question,
              values_from = slider) %>% 
  ungroup()
pca_fa_raw <- pca_fa %>% 
  select(-c(sub,name,RatingType))

eigenvalues <- principal(pca_fa_raw, nfactors = ncol(pca_fa_raw))
# only first has eigenvalue greater than 1, which is what ChatGPT says to do

fa_result <- fa(pca_fa_raw, missing = TRUE, rotate = "none")
factor_scores <- fa_result$scores

original_mean <- mean(unlist(pca_fa_raw), na.rm = TRUE)
original_sd <- sd(unlist(pca_fa_raw), na.rm = TRUE)

rescaled_factor_scores <- factor_scores * original_sd + original_mean

d <- d %>% 
  filter(question_short == 'Compatible') %>% 
  mutate(slider = as.vector(rescaled_factor_scores)) %>% 
  select(sub,name,RatingType,slider) %>% 
  mutate(RatingType = recode(RatingType,
                             `Pre-Feedback video` = 'slider_assess.response',
                             `Post-Feedback video` = 'slider_reveal.response')) %>% 
  pivot_wider(names_from = RatingType, values_from = slider)

write_csv(d,'real/fmri_ratings_fa_TEMP.csv')
  