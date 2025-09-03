library(tidyverse)
library(lmerTest)
library(brms)
library(corrplot)
library(ggpmisc)
library(rstatix)
library(ggtext)
library(cowplot)
library(ggpubr)
setwd('/home/bms2202/dating/')

##### Behavioral analyses #####
#### Video norming ######
d_norm <- read_csv('data/norming/video_norming.csv')
## attractiveness #####
ggplot(d_norm %>% 
         group_by(target, gender) %>% 
         summarize(Attractiveness = mean(Attractiveness)),
       aes(x = Attractiveness)) +
  geom_histogram(bins = 10,color = 'black', aes(fill = gender)) +
  facet_grid(rows = vars(gender)) +
  theme_bw()

ggplot(d_norm %>% 
         group_by(target, gender) %>% 
         summarize(Attractiveness = mean(Attractiveness)),
       aes(x = Attractiveness)) +
  geom_boxplot(color = 'black', aes(fill = gender)) +
  facet_grid(rows = vars(gender)) +
  theme_bw()

d_norm %>% 
  group_by(gender) %>% 
  summarize(Attractiveness = mean(Attractiveness))

d_norm %>% 
  group_by(gender) %>% 
  summarize(Attractiveness = sd(Attractiveness))

d_norm %>% 
  group_by(gender) %>% 
  summarize(Attractiveness = quantile(Attractiveness,.25))

d_norm %>% 
  group_by(gender) %>% 
  summarize(Attractiveness = quantile(Attractiveness,.75))

# boxplot graph uses IQR so can just visually inspect that
d_norm %>% 
  filter(gender == 'F') %>% 
  group_by(target) %>% 
  summarize(Attractiveness = mean(Attractiveness)) %>% 
  arrange(Attractiveness)

d_norm %>% 
  filter(gender == 'M') %>% 
  group_by(target) %>% 
  summarize(Attractiveness = mean(Attractiveness)) %>% 
  arrange(Attractiveness)

## perceived age ######
ggplot(d_norm %>% 
         group_by(target, gender) %>% 
         summarize(Age = mean(Age)),
       aes(x = Age)) +
  geom_histogram(bins = 10,color = 'black', aes(fill = gender)) +
  facet_grid(rows = vars(gender)) +
  theme_bw()

ggplot(d_norm %>% 
         group_by(target, gender) %>% 
         summarize(Age = mean(Age)),
       aes(x = Age)) +
  geom_boxplot(color = 'black', aes(fill = gender)) +
  facet_grid(rows = vars(gender)) +
  theme_bw()

d_norm %>% 
  group_by(gender) %>% 
  summarize(Age = mean(Age))

d_norm %>% 
  group_by(gender) %>% 
  summarize(Age = sd(Age))

d_norm %>% 
  filter(gender == 'F') %>% 
  group_by(target) %>% 
  summarize(Age = mean(Age)) %>% 
  arrange(Age)

d_norm %>% 
  filter(gender == 'M') %>% 
  group_by(target) %>% 
  summarize(Age = mean(Age)) %>% 
  arrange(Age)

## perceived sexuality ####
ggplot(d_norm %>% 
         group_by(target, gender) %>% 
         summarize(Sexuality = mean(Sexuality)),
       aes(x = Sexuality)) +
  geom_histogram(bins = 10,color = 'black', aes(fill = gender)) +
  facet_grid(rows = vars(gender)) +
  theme_bw()

ggplot(d_norm %>% 
         group_by(target, gender) %>% 
         summarize(Sexuality = mean(Sexuality)),
       aes(x = Sexuality)) +
  geom_boxplot(color = 'black', aes(fill = gender)) +
  facet_grid(rows = vars(gender)) +
  theme_bw()

d_norm %>% 
  group_by(gender) %>% 
  summarize(Sexuality = mean(Sexuality))

d_norm %>% 
  group_by(gender) %>% 
  summarize(Sexuality = sd(Sexuality))

d_norm %>% 
  filter(gender == 'F') %>% 
  group_by(target) %>% 
  summarize(Sexuality = mean(Sexuality)) %>% 
  arrange(Sexuality)

d_norm %>% 
  filter(gender == 'M') %>% 
  group_by(target) %>% 
  summarize(Sexuality = mean(Sexuality)) %>% 
  arrange(Sexuality)


#### Descriptive statistics ######
d_descrip <- read_csv('data/real/fmri_ratings_allqs.csv')

d_descrip %>% 
  group_by(question) %>% 
  summarize(mean = mean(slider_assess.response,na.rm = T),
            sd = sd(slider_assess.response,na.rm = T))

#### How did participants’ romantic interest change in response to social feedback? #####
## prep #####
d_raw <- read_csv('data/real/fmri_ratings_fa.csv')

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
  mutate(feedback = recode(feedback, "neg" = "Negative Feedback",
                           "pos" = "Positive Feedback"),
         feedback.e = if_else(feedback.d == 0,-.5,.5),
         start.e = if_else(start.d == 0,-.5,.5),
         start.cont = assess_slider,
         congruence.e = if_else(congruence.d == 0,-.5,.5)) %>% 
  pivot_longer(cols = c(contains("assess"),contains("reveal")),
               names_to = c("RatingType",".value"),
               values_to = "Difficulty",
               names_sep = "_"
  ) %>% 
  mutate(#slider = as.double(na_if(slider, "None")),
    slider.c = as.vector(scale(slider,center = T,scale = F)),
    RatingType = recode(RatingType, "assess" = "Pre-Feedback video",
                        "reveal" = "Post-Feedback video"),
    RatingType = fct_relevel(RatingType,"Pre-Feedback video","Post-Feedback video"),
    RatingType.d = if_else(RatingType == "Pre-Feedback video",0,1),
    question_short = recode(question,
                            "How romantically compatible do you believe you are with this person?" = "Compatible",
                            "How similar do you believe you are to this person?" = "Similar",
                            "How physically attractive do you find this person?" = 'Attractive',
                            "How interested were you in what this person was talking about?" = "Interested",
                            "How much do you like this person?" = "Liking")) %>% 
  group_by(sub,name, question_short) %>% 
  mutate(Difference_slider = slider - lag(slider),
         Difference_memacc = memoryacc - lag(memoryacc),
         Difference_memtime = memorytime - lag(memorytime),
         Difference_memsum = memorysum - lag(memorysum),
         Difference_memperc = memoryperc - lag(memoryperc)) 
## models #####
M1A <- lmer(Difference_slider ~ congruence.e*feedback.e +
              (1 + congruence.e*feedback.e | sub),
            data = d)

temp <- d %>% 
  filter(congruence.e == -.5,feedback.e == -.5)
t.test(temp$Difference_slider)

M1B <- lmer(Difference_slider ~ congruence.e*feedback.e +
              (1 + congruence.e*feedback.e | sub),
            data = d %>% 
              mutate(Difference_slider = if_else(feedback.d == 0,
                                                 -1*Difference_slider,
                                                 Difference_slider)))

M1B1 <- lmer(Difference_slider ~ feedback.e +
               (1 + feedback.e | sub),
             data = d %>% 
               filter(congruence == 'Incongruent') %>% 
               mutate(Difference_slider = if_else(feedback.d == 0,
                                                  -1*Difference_slider,
                                                  Difference_slider)))

## figures ####
# Figure 1A
F1A <- ggplot(d %>% 
                filter(!is.na(start.d)) %>% 
                mutate(feedback = recode(feedback,
                                         "Negative Feedback" = "Neg",
                                         "Positive Feedback" = "Pos")), 
              aes(x = congruence.d, y = Difference_slider, group = feedback, color = feedback)) +
  geom_hline(yintercept = 0, linewidth = 1.2,linetype = "dashed", 
  ) +
  geom_jitter(width = .1, alpha = .4, size = 2) +
  geom_smooth(method = "lm", size = 2) +
  theme_bw() +
  labs(y = "Change in romantic interest",
       x = "Feedback congruence with participant",
       color = "Feedback valence") +
  scale_x_continuous(breaks = c(0,1),labels = c('Incongruent',
                                                'Congruent')) +
  scale_y_continuous(limits = c(-6,6)) +
  scale_color_manual(values = c("Neg" = "skyblue2",
                                "Pos" = "lightpink2"),
                     limits = c("Pos","Neg")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 22))

d_F1B <- d %>% 
  filter(!is.na(start.d)) %>% 
  mutate(Difference_slider = if_else(feedback.d == 0,
                                     -1*Difference_slider,
                                     Difference_slider)) %>% 
  group_by(congruence.d,feedback.d) %>% 
  mutate(mean_sim = mean(Difference_slider, na.rm = T),
         sem_sim = sd(Difference_slider, na.rm = T)/ sqrt(n()),
         feedback = recode(feedback,
                           "Negative Feedback" = "Neg",
                           "Positive Feedback" = "Pos")) %>% 
  distinct(congruence.d,feedback.d,.keep_all = T)

# using .189 because that is the SE of congruence according to model
F1B <- ggplot(d_F1B, 
              aes(x = congruence.d, y = mean_sim, group = feedback, fill = feedback)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_sim - (1.96*.189), ymax = mean_sim + (1.96*.189)),
                position = position_dodge(width = .9), width = .25) +
  theme_bw() +
  lims(y = c(-.2,2.1)) +
  geom_segment(x = 0, xend = 1, y = 2, yend = 2) +
  annotate('text',label = '**', x = .5, y = 2.02, size = 10) +
  labs(y = "Change in romantic interest\nin the direction of feedback",
       x = "Feedback congruence with participant",
       fill = "Feedback valence") +
  scale_x_continuous(breaks = c(0,1),labels = c('Incongruent',
                                                'Congruent')) +
  scale_fill_manual(values = c("Neg" = "skyblue2",
                               "Pos" = "lightpink2"),
                    limits = c("Pos","Neg")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 22))

F1 <- ggarrange(F1A,F1B,
                labels = c("A","B"),
                nrow = 1, ncol = 2,
                font.label = list(size = 20),
                common.legend = T, legend = 'bottom')
annotate_figure(F1, 
                top = text_grob("Effect of target feedback on romantic interest",
                                face = "bold",size = 28))
ggsave("manuscript/figs/Fig1.jpg", width = 12, height = 6)

F1B +
  labs(title = "Effect of target feedback on romantic interest",
       fill = 'Feedback\nvalence',
       y = "Post-feedback absolute\nchange in romantic interest") +
  lims(y = c(-.2,2.2)) +
  theme(plot.title = element_text(size = 24, hjust = .5))
ggsave("manuscript/figs/Fig1B_talk.jpg", width = 9, height = 6)




##### fMRI analyses ########
#### preparing the labels #####
p = 'fmriprep'
top_rois_strQ1 = c('insula',
                   'neurosynth_mentalizing','ns_mentalizing_dmPFC',
                   'ns_mentalizing_vmPFC','ns_mentalizing_Precuneus',
                   'ns_mentalizing_LTPJ','ns_mentalizing_RTPJ',
                   'ns_mentalizing_LTempPole','ns_mentalizing_RTempPole')
top_rois_strQ2 = c("hippocampus","hippocampus_anterior",
                   "hippocampus_posterior",
                   'neurosynth_mentalizing','ns_mentalizing_dmPFC',
                   'ns_mentalizing_vmPFC')

template_ = 'wholevideo'

# create ROI labellers
rois <- tibble(space = 'fmriprep',
               roi = c("ACC","courtney_dmpfc","courtney_vmpfc",
                       "insula",
                       "neurosynth_dmpfc","neurosynth_vmpfc",
                       'neurosynth_conflict','neurosynth_mentalizing',
                       'neurosynth_value',
                       "hippocampus","hippocampus_anterior",
                       "hippocampus_posterior",
                       'ns_mentalizing_dmPFC',
                       'ns_mentalizing_vmPFC','ns_mentalizing_Precuneus',
                       'ns_mentalizing_LTPJ','ns_mentalizing_RTPJ',
                       'ns_mentalizing_LTempPole','ns_mentalizing_RTempPole'),
               roi_label = c("ACC","courtney_dmpfc","courtney_vmpfc",
                             "insula",
                             "neurosynth_dmpfc","neurosynth_vmpfc",
                             'neurosynth_conflict','neurosynth_mentalizing',
                             'neurosynth_value',
                             "hippocampus","hippocampus_anterior",
                             "hippocampus_posterior",
                             'ns_mentalizing_dmPFC',
                             'ns_mentalizing_vmPFC','ns_mentalizing_Precuneus',
                             'ns_mentalizing_LTPJ','ns_mentalizing_RTPJ',
                             'ns_mentalizing_LTempPole','ns_mentalizing_RTempPole'),
               roi_label_paper = c("ACC","courtney_dmpfc","courtney_vmpfc",
                                   "insula",
                                   "neurosynth_dmpfc","neurosynth_vmpfc",
                                   'neurosynth_conflict','Mentalizing\nnetwork',
                                   'neurosynth_value',
                                   "Hippocampus","Anterior\nHipp",
                                   "Posterior\nHipp",
                                   'dmPFC',
                                   'vmPFC','Precuneus',
                                   'L TPJ','R TPJ',
                                   'L Temporal\nPole','R Temporal\nPole'))

parcels <- read_csv('masks/original/shen_parcellation_labels.csv')
parcels <- parcels %>% 
  mutate(space = 'fmriprep') %>% 
  rename(roi = 'Parcel Number',roi_label = 'Brain Region') %>% 
  select(space, roi, roi_label) %>% 
  mutate(roi = as.character(roi),
         roi_label = str_c(roi,": ",str_extract(roi_label, "^[^/]+")),
  )

parcels_char <- setNames(parcels$roi_label, parcels$roi)
rois_char <- setNames(rois$roi, rois$roi)
rois_char_paper <- setNames(rois$roi_label_paper, rois$roi_label_paper)

#### Did the mentalizing network represent romantic interest? ######
### prep #####
df2 <- read_csv('neurbehav_allqs_noruns_spatial.csv')

df2_sum <- df2 %>% 
  filter(template2 == '10secs' | template2 == 'last10secs' | is.na(template2)) %>% 
  mutate(template2 = if_else(is.na(template2), 'Whole',template2)) %>% 
  group_by(template2, roi) %>% 
  mutate(na_check = mean(brain_correlation,na.rm = T)) %>% 
  filter(!is.na(na_check)) %>% 
  ungroup() %>% 
  group_by(template2,roi,profile,video) %>% 
  summarize(cor = cor(1 - brain_correlation,behavior_distance, 
                      method = "spearman",
                      use = "complete.obs")) %>% 
  mutate(roi = as.character(roi)) 


### stats ####
## t tests #####
# whole video ####
# along with visual inspection of figure
df2_sum_ttests <- df2_sum %>% 
  filter(is.na(template2))

t1 <- t.test(df2_sum_ttests[df2_sum_ttests$roi == 'ns_mentalizing_RTPJ',]$cor,alternative= "greater", mu=0)
t3 <- t.test(df2_sum_ttests[df2_sum_ttests$roi == 'neurosynth_mentalizing',]$cor,alternative= "greater", mu=0)

# first 10 seconds ####
df2_sum_ttests <- df2_sum %>% 
  filter(template2 == '10secs')

t.test(df2_sum_ttests[df2_sum_ttests$roi == 'neurosynth_mentalizing',]$cor,alternative= "greater", mu=0)
t.test(df2_sum_ttests[df2_sum_ttests$roi == 'ns_mentalizing_dmPFC',]$cor,alternative= "greater", mu=0)
t.test(df2_sum_ttests[df2_sum_ttests$roi == 'ns_mentalizing_RTPJ',]$cor,alternative= "greater", mu=0)

# last 10 seconds ####
df2_sum_ttests <- df2_sum %>% 
  filter(template2 == 'last10secs')

t.test(df2_sum_ttests[df2_sum_ttests$roi == 'neurosynth_mentalizing',]$cor,alternative= "greater", mu=0)
t.test(df2_sum_ttests[df2_sum_ttests$roi == 'ns_mentalizing_dmPFC',]$cor,alternative= "greater", mu=0)
t.test(df2_sum_ttests[df2_sum_ttests$roi == 'ns_mentalizing_RTPJ',]$cor,alternative= "greater", mu=0)

# comparisons ####
t.test(cor ~ template2, data = df2_sum %>% filter(roi == 'ns_mentalizing_RTPJ',
                                                  template2 != 'Whole'))
t.test(cor ~ template2, data = df2_sum %>% filter(roi == 'neurosynth_mentalizing',
                                                  template2 != 'Whole'))
t.test(cor ~ template2, data = df2_sum %>% filter(roi == 'ns_mentalizing_dmPFC',
                                                  template2 != 'Whole'))

t.test(cor ~ template2, data = df2_sum %>% filter(roi == 'ns_mentalizing_RTPJ',
                                                  template2 != 'last10secs'))
t.test(cor ~ template2, data = df2_sum %>% filter(roi == 'neurosynth_mentalizing',
                                                  template2 != 'last10secs'))
t.test(cor ~ template2, data = df2_sum %>% filter(roi == 'ns_mentalizing_dmPFC',
                                                  template2 != 'last10secs'))
## permutations ####
# whole video ######
df2 <- read_csv('neurbehav_allqs_noruns_spatial.csv')
df2 <- df2 %>% 
  filter(is.na(template2))

df2_sum <- df2 %>% 
  filter(is.na(template2)) %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(brain_correlation,na.rm = T)) %>% 
  filter(!is.na(na_check)) %>% 
  ungroup() %>% 
  group_by(roi,profile,video) %>% 
  summarize(cor = cor(brain_correlation,behavior_distance, 
                      method = "spearman",
                      use = "complete.obs")) %>% 
  mutate(roi = as.character(roi)) 

df2_permute <- vector("list", 1000)
for (i in 1:1000) {
  set.seed(i)
  df2_permute[[i]] <- df2 %>% 
    group_by(roi) %>% 
    mutate(na_check = mean(brain_correlation,na.rm = T)) %>% 
    filter(!is.na(na_check)) %>% 
    ungroup() %>% 
    group_by(roi,profile,video) %>% 
    mutate(brain_correlation = sample(brain_correlation)) %>% 
    summarize(cor = cor(1 - brain_correlation,behavior_distance, 
                        method = "spearman",
                        use = "complete.obs")) %>% 
    ungroup() %>% 
    group_by(roi) %>% 
    summarize(cor = mean(cor)) %>%
    mutate(iteration = i)
}

df2_permute <- bind_rows(df2_permute)

df2_permute <- df2_permute %>% 
  bind_rows(df2_sum %>% 
              group_by(roi) %>% 
              summarize(cor = mean(cor)) %>% 
              mutate(iteration = 0)) %>% 
  mutate(real = if_else(iteration == 0,1,0)) %>% 
  group_by(roi) %>% 
  mutate(mean = mean(cor),
         sd = sd(cor),
         cutoff = mean - 1.645*sd)

# replace with relevant ROI to calculate # of higher permutations 
df2_testing <- df2_permute %>% 
  filter(roi == 'ns_mentalizing_RTempPole')

# first 10 seconds ####
df2 <- read_csv('neurbehav_allqs_noruns_spatial.csv')
df2 <- df2 %>% 
  filter(template2 == '10secs')

df2_sum <- df2 %>% 
  filter(template2 == '10secs') %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(brain_correlation,na.rm = T)) %>% 
  filter(!is.na(na_check)) %>% 
  ungroup() %>% 
  group_by(roi,profile,video) %>% 
  summarize(cor = cor(1 - brain_correlation,behavior_distance, 
                      method = "spearman",
                      use = "complete.obs")) %>% 
  mutate(roi = as.character(roi),
         #roi = str_replace(roi,"_","\n")
  ) 

df2a_permute <- vector("list", 100)
for (i in 1:1000) {
  set.seed(i)
  df2a_permute[[i]] <- df2 %>% 
    group_by(roi) %>% 
    mutate(na_check = mean(brain_correlation,na.rm = T)) %>% 
    filter(!is.na(na_check)) %>% 
    ungroup() %>% 
    group_by(roi,profile,video) %>% 
    mutate(brain_correlation = sample(brain_correlation)) %>% 
    summarize(cor = cor(1 - brain_correlation,behavior_distance, 
                        method = "spearman",
                        use = "complete.obs")) %>% 
    ungroup() %>% 
    group_by(roi) %>% 
    summarize(cor = mean(cor)) %>%
    mutate(iteration = i)
}

df2a_permute <- bind_rows(df2a_permute)

df2a_permute <- df2a_permute %>% 
  bind_rows(df2_sum %>% 
              group_by(roi) %>% 
              summarize(cor = mean(cor)) %>% 
              mutate(iteration = 0)) %>% 
  mutate(real = if_else(iteration == 0,1,0)) %>% 
  group_by(roi) %>% 
  mutate(mean = mean(cor),
         sd = sd(cor),
         cutoff = mean - 1.645*sd)

# last 10 seconds ####
df2 <- read_csv('neurbehav_allqs_noruns_spatial.csv')
df2 <- df2 %>% 
  filter(template2 == 'last10secs')

df2_sum <- df2 %>% 
  filter(template2 == 'last10secs') %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(brain_correlation,na.rm = T)) %>% 
  filter(!is.na(na_check)) %>% 
  ungroup() %>% 
  group_by(roi,profile,video) %>% 
  summarize(cor = cor(1 - brain_correlation,behavior_distance, 
                      method = "spearman",
                      use = "complete.obs")) %>% 
  mutate(roi = as.character(roi),
         #roi = str_replace(roi,"_","\n")
  ) 

df2b_permute <- vector("list", 100)
for (i in 1:1000) {
  set.seed(i)
  df2b_permute[[i]] <- df2 %>% 
    group_by(roi) %>% 
    mutate(na_check = mean(brain_correlation,na.rm = T)) %>% 
    filter(!is.na(na_check)) %>% 
    ungroup() %>% 
    group_by(roi,profile,video) %>% 
    mutate(brain_correlation = sample(brain_correlation)) %>% 
    summarize(cor = cor(1 - brain_correlation,behavior_distance, 
                        method = "spearman",
                        use = "complete.obs")) %>% 
    ungroup() %>% 
    group_by(roi) %>% 
    summarize(cor = mean(cor)) %>%
    mutate(iteration = i)
}

df2b_permute <- bind_rows(df2b_permute)

df2b_permute <- df2b_permute %>% 
  bind_rows(df2_sum %>% 
              group_by(roi) %>% 
              summarize(cor = mean(cor)) %>% 
              mutate(iteration = 0)) %>% 
  mutate(real = if_else(iteration == 0,1,0)) %>% 
  group_by(roi) %>% 
  mutate(mean = mean(cor),
         sd = sd(cor),
         cutoff = mean - 1.645*sd)
### Figure ######
# prep #####
df2 <- read_csv('neurbehav_allqs_noruns_spatial.csv')

df2_sum <- df2 %>% 
  filter(template2 == '10secs' | template2 == 'last10secs' | is.na(template2)) %>% 
  mutate(template2 = if_else(is.na(template2), 'Whole',template2)) %>% 
  group_by(template2, roi) %>% 
  mutate(na_check = mean(brain_correlation,na.rm = T)) %>% 
  filter(!is.na(na_check)) %>% 
  ungroup() %>% 
  group_by(template2,roi,profile,video) %>% 
  summarize(cor = cor(1 - brain_correlation,behavior_distance, 
                      method = "spearman",
                      use = "complete.obs")) %>% 
  mutate(roi = as.character(roi)) 
# permutation prep ####
df2_permute_paper <- df2_permute %>% 
  left_join(rois) %>% 
  mutate(roi_label_paper = factor(roi_label_paper, levels = rev(c('Mentalizing\nnetwork','L TPJ',
                                                                  'R TPJ', 'L Temporal\nPole',
                                                                  'R Temporal\nPole','Precuneus',
                                                                  'dmPFC','vmPFC'))),
         template2 = 'Whole') %>% 
  filter(!is.na(roi_label_paper))

df2a_permute_paper <- df2a_permute %>% 
  left_join(rois) %>% 
  mutate(roi_label_paper = factor(roi_label_paper, levels = rev(c('Mentalizing\nnetwork','L TPJ',
                                                                  'R TPJ', 'L Temporal\nPole',
                                                                  'R Temporal\nPole','Precuneus',
                                                                  'dmPFC','vmPFC'))),
         template2 = '10secs') %>% 
  filter(!is.na(roi_label_paper))

df2b_permute_paper <- df2b_permute %>% 
  left_join(rois) %>% 
  mutate(roi_label_paper = factor(roi_label_paper, levels = rev(c('Mentalizing\nnetwork','L TPJ',
                                                                  'R TPJ', 'L Temporal\nPole',
                                                                  'R Temporal\nPole','Precuneus',
                                                                  'dmPFC','vmPFC'))),
         template2 = 'last10secs') %>% 
  filter(!is.na(roi_label_paper))

df2all_permute_paper <- df2_permute_paper %>% 
  bind_rows(df2a_permute_paper,df2b_permute_paper) %>% 
  mutate(Main = if_else(template2 == 'Whole',0,1))

# F2A #####
df2_sum_paper <- df2_sum %>% 
  mutate(template2 = factor(template2, levels = c('Whole','10secs','last10secs'))) %>% 
  filter(roi %in% top_rois_strQ1,
         roi != 'insula') %>% 
  left_join(rois) %>% 
  mutate(roi_label_paper = factor(roi_label_paper, levels = c('Mentalizing\nnetwork','L TPJ',
                                                              'R TPJ', 'L Temporal\nPole',
                                                              'R Temporal\nPole','Precuneus',
                                                              'dmPFC','vmPFC'))) %>% 
  group_by(template2,roi) %>% 
  mutate(mean_cor = mean(cor, na.rm = T),
         sem_cor = sd(cor, na.rm = T)/ sqrt(n()),
         Main = if_else(template2 == 'Whole',0,1))

F2A <- ggplot(df2_sum_paper %>% 
                filter(roi_label_paper == 'Mentalizing\nnetwork'),
              aes(x = roi_label_paper, y = cor, 
                  group = template2, color = template2,fill = template2)) +
  geom_jitter(alpha = .15,position = position_jitterdodge()) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .4) +
  geom_violin(data = df2all_permute_paper %>% 
                filter(roi_label_paper == 'Mentalizing\nnetwork'),aes(x = roi_label_paper,
                                                                      y = cor,
                                                                      group = template2,
                                                                      color = template2,
                                                                      fill = template2),
              alpha = .2, width = .6,
              position = position_dodge(width = .9)) +
  facet_grid(cols = vars(Main)) +
  theme_bw() +
  scale_x_discrete() +
  scale_color_discrete(labels = c('Whole\nvideo','First\n10 secs','Last\n10 secs')) +
  scale_fill_discrete(labels = c('Whole\nvideo','First\n10 secs','Last\n10 secs')) +
  geom_point(aes(y = mean_cor), size = 3,position = position_dodge(width = .9)) +
  labs(y = "Neural dissimilarity-rating\ndistance correlation",
       color = 'Template',fill = 'Template',
       title = 'Brain-behavior correlations across ROIs') +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5,size = 14),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = 'bottom')

# F2B ####
F2B <- ggplot(df2_sum_paper %>% 
                filter(roi_label_paper != 'Mentalizing\nnetwork'),
              aes(x = roi_label_paper, y = cor, 
                  group = template2, color = template2,fill = template2)) +
  geom_jitter(alpha = .15,position = position_jitterdodge()) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .4) +
  geom_violin(data = df2all_permute_paper %>% 
                filter(roi_label_paper != 'Mentalizing\nnetwork'),aes(x = roi_label_paper,
                                                                      y = cor,
                                                                      group = interaction(template2,roi_label_paper),
                                                                      color = template2,
                                                                      fill = template2),
              alpha = .2, width = .8,
              position = position_dodge(width = .9)) +
  facet_grid(rows = vars(Main)) +
  theme_bw() +
  scale_x_discrete() +
  scale_color_discrete(labels = c('Whole\nvideo','First\n10 secs','Last\n10 secs')) +
  scale_fill_discrete(labels = c('Whole\nvideo','First\n10 secs','Last\n10 secs')) +
  geom_point(aes(y = mean_cor), size = 2,position = position_dodge(width = .9)) +
  labs(y = "Neural dissimilarity-rating\ndistance correlation",
       color = 'Template',fill = 'Template') +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5,size = 14),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = 'None')

# F2 #####
blank <- ggplot() + theme_void()

top_row <- ggarrange(
  blank, F2A, blank,
  ncol = 3,
  widths = c(1, 4, 1)
)
ggarrange(top_row,F2B,
          nrow = 2, ncol = 1,
          heights = c(1,1))
ggsave("Fig2.jpg", width = 9, height = 6)
#### How were neural representations of potential romantic partners updated in response to feedback? ######
### prep #####
df4 <- read_csv('acrossrun.csv')
### Stats #####
F3A1_stats <- df4 %>% 
  filter(comp_type == 'within_pro',
         template == template_) %>% 
  mutate(feedback_valence.d = if_else(feedback_valence == "neg",0,1)) %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(similarity,na.rm = T)) %>% 
  filter(!is.na(na_check)) %>% 
  ungroup() %>% 
  group_by(roi) %>%
  mutate(beta = round(summary(lmer(similarity ~ feedback_valence.d + 
                                     (1 + feedback_valence.d | sub)))$coefficients[2,1],3),
         SE = round(summary(lmer(similarity ~ feedback_valence.d + 
                                   (1 + feedback_valence.d | sub)))$coefficients[2,2],3),
         P = round(summary(lmer(similarity ~ feedback_valence.d + 
                                  (1 + feedback_valence.d | sub)))$coefficients[2,5],3)) 

F3A1_stats_unique = F3A1_stats %>% 
  group_by(roi) %>% 
  summarize(beta = unique(beta),
            SE = unique(SE),
            P = unique(P)) %>% 
  filter(roi %in% top_rois_strQ1,
         roi != 'insula') %>% 
  left_join(rois) %>% 
  mutate(roi_label_paper = factor(roi_label_paper, levels = rev(c('Mentalizing\nnetwork','L TPJ',
                                                                  'R TPJ', 'L Temporal\nPole',
                                                                  'R Temporal\nPole','Precuneus',
                                                                  'dmPFC','vmPFC'))),
         Comparison = 'Feedback valence',
         lower = beta - (1.96*SE),
         upper = beta + (1.96*SE))

F3A2_stats <- df4 %>% 
  filter(comp_type == 'within_pro',
         template == template_) %>% 
  mutate(feedback_alignment.d = if_else(feedback_alignment == "Incongruent",0,1)) %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(similarity,na.rm = T)) %>% 
  filter(!is.na(na_check)) %>% 
  ungroup() %>% 
  group_by(roi) %>%
  mutate(beta = round(summary(lmer(similarity ~ feedback_alignment.d + 
                                     (1 + feedback_alignment.d | sub)))$coefficients[2,1],3),
         SE = round(summary(lmer(similarity ~ feedback_alignment.d + 
                                   (1 + feedback_alignment.d | sub)))$coefficients[2,2],3),
         P = summary(lmer(similarity ~ feedback_alignment.d + 
                            (1 + feedback_alignment.d | sub)))$coefficients[2,5])

F3A2_stats_unique = F3A2_stats %>% 
  group_by(roi) %>% 
  summarize(beta = unique(beta),
            SE = unique(SE),
            P = unique(P)) %>% 
  filter(roi %in% top_rois_strQ1,
         roi != 'insula') %>% 
  left_join(rois) %>% 
  mutate(roi_label_paper = factor(roi_label_paper, levels = rev(c('Mentalizing\nnetwork','L TPJ',
                                                                  'R TPJ', 'L Temporal\nPole',
                                                                  'R Temporal\nPole','Precuneus',
                                                                  'dmPFC','vmPFC'))),
         Comparison = 'Feedback congruence',
         lower = beta - (1.645*SE),
         upper = beta + (1.645*SE),
         P = P/2) # one tailed

F3A3_stats <- df4 %>% 
  filter(comp_type == 'within_pro',
         template == template_) %>% 
  mutate(feedback_alignment.d = if_else(feedback_alignment == "Incongruent",0,1),
         feedback_valence.d = if_else(feedback_valence == "neg",0,1),
         start.d = if_else(feedback_alignment.d == 0 & feedback_valence.d == 0,1,
                           if_else(feedback_alignment.d == 0 & feedback_valence.d == 1,0,
                                   if_else(feedback_alignment.d == 1 & feedback_valence.d == 0,0,1)
                           )
         )
  ) %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(similarity,na.rm = T)) %>% 
  filter(!is.na(na_check)) %>% 
  ungroup() %>% 
  group_by(roi) %>%
  mutate(beta = round(summary(lmer(similarity ~ start.d + 
                                     (1 + start.d | sub)))$coefficients[2,1],3),
         SE = round(summary(lmer(similarity ~ start.d + 
                                   (1 + start.d | sub)))$coefficients[2,2],3),
         P = round(summary(lmer(similarity ~ start.d + 
                                  (1 + start.d | sub)))$coefficients[2,5],3))

F3A3_stats_unique = F3A3_stats %>% 
  group_by(roi) %>% 
  summarize(beta = unique(beta),
            SE = unique(SE),
            P = unique(P)) %>% 
  filter(roi %in% top_rois_strQ1,
         roi != 'insula') %>% 
  left_join(rois) %>% 
  mutate(roi_label_paper = factor(roi_label_paper, levels = rev(c('Mentalizing\nnetwork','L TPJ',
                                                                  'R TPJ', 'L Temporal\nPole',
                                                                  'R Temporal\nPole','Precuneus',
                                                                  'dmPFC','vmPFC'))),
         Comparison = 'Initial romantic interest',
         lower = beta - (1.96*SE),
         upper = beta + (1.96*SE))

F3_stats_unique <- F3A1_stats_unique %>% 
  bind_rows(F3A2_stats_unique) %>% 
  bind_rows(F3A3_stats_unique) %>% 
  mutate(Comparison = factor(Comparison,levels = c('Feedback valence',
                                                   'Feedback congruence',
                                                   'Initial romantic interest')))
### Figure #####
# F3A #####


F3A <- ggplot(F3_stats_unique %>% 
                filter(roi_label_paper == 'Mentalizing\nnetwork'), 
              aes(x = beta, y = roi_label_paper)) +
  geom_errorbar(aes(xmin = lower, xmax = upper,
                    color = Comparison),
                width = 0,
                position = position_dodge(width = .8)) +
  geom_point(aes(color = Comparison),
             size = 3, position = position_dodge(width = .8)) +
  facet_grid(cols = vars(Comparison)) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta") +
  scale_x_continuous(n.breaks = 10,
                     limits = c(-0.08,0.08)) +
  scale_y_discrete(expand = expansion(mult = .01)) +
  guides(color = guide_legend(reverse = T)) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title = element_blank(),
        strip.text = element_text(size = 14),
        plot.title = element_text(hjust = .5, size = 16),
        legend.position = "none")


# F3B #####
F3B <- ggplot(F3_stats_unique %>% 
                filter(roi_label_paper != 'Mentalizing\nnetwork'), 
              aes(x = beta, y = roi_label_paper)) +
  geom_errorbar(aes(xmin = lower, xmax = upper,
                    color = Comparison),
                width = 0,
                position = position_dodge(width = .8)) +
  geom_point(aes(color = Comparison),
             size = 3, position = position_dodge(width = .8)) +
  facet_grid(cols = vars(Comparison)) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_discrete(expand = expansion(mult = .01)) +
  guides(color = guide_legend(reverse = T)) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(hjust = .5, size = 16),
        legend.position = "none")


# F3 ####

F3 <- ggarrange(F3A, F3B,
                ncol = 1,nrow = 2,heights = c(1, 3),labels = NULL           
)

# Add a super title
annotate_figure(F3,
                top = text_grob("Impact of feedback valence, feedback congruence,\nand initial romantic interest on neural template change", 
                                face = "bold", size = 18, hjust = .4))


ggsave("Fig3.jpg", width = 12, height = 6)



#### How often were neural representations of potential romantic partners reactivated during rest in response to feedback? ####
### prep ####
df5 <- read_csv('reactivation_wholerest.csv')
df5 <- df5 %>% 
  filter(!(templaterun == 1 & run == 2) &
           !(templaterun == 2 & run == 1))
### Stats #####
## Increase over baseline rest ####
F4A_stats01 <- df5 %>% 
  filter(run != 2,
         template == template_,
         profile != 'Any') %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(rf,na.rm = T)) %>% 
  filter(na_check != 0) %>% 
  ungroup() %>% 
  group_by(roi) %>% 
  mutate(beta01 = round(summary(lmer(rf ~ run + 
                                       (1 + run | sub)))$coefficients[2,1],3),
         SE01 = round(summary(lmer(rf ~ run + 
                                     (1 + run | sub)))$coefficients[2,2],3),
         P01 = round(summary(lmer(rf ~ run + 
                                    (1 + run | sub)))$coefficients[2,5],3))

F4A_stats12 <- df5 %>% 
  filter(run != 0,
         template == template_,
         profile != 'Any') %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(rf,na.rm = T)) %>% 
  filter(na_check != 0) %>% 
  ungroup() %>% 
  group_by(roi) %>% 
  mutate(beta12 = round(summary(lmer(rf ~ run + 
                                       (1 + run | sub)))$coefficients[2,1],3),
         SE12 = round(summary(lmer(rf ~ run + 
                                     (1 + run | sub)))$coefficients[2,2],3),
         P12 = round(summary(lmer(rf ~ run + 
                                    (1 + run | sub)))$coefficients[2,5],3))

F4A_stats02 <- df5 %>% 
  filter(run != 1,
         template == template_,
         profile != 'Any') %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(rf,na.rm = T)) %>% 
  filter(na_check != 0) %>% 
  ungroup() %>% 
  mutate(run_stats = if_else(run == 0,0,1)) %>% 
  group_by(roi) %>% 
  mutate(beta02 = round(summary(lmer(rf ~ run_stats + 
                                       (1 + run_stats | sub)))$coefficients[2,1],3),
         SE02 = round(summary(lmer(rf ~ run_stats + 
                                     (1 + run_stats | sub)))$coefficients[2,2],3),
         P02 = round(summary(lmer(rf ~ run_stats + 
                                    (1 + run_stats | sub)))$coefficients[2,5],3))

F4A_stats <- F4A_stats01 %>% 
  full_join(F4A_stats12) %>% 
  full_join(F4A_stats02 %>% 
              select(-run_stats)) %>% 
  group_by(roi) %>% 
  summarize(beta01 = mean(beta01, na.rm = T),
            SE01 = mean(SE01, na.rm = T),
            P01 = mean(P01, na.rm = T),
            beta12 = mean(beta12, na.rm = T),
            SE12 = mean(SE12, na.rm = T),
            P12 = mean(P12, na.rm = T),
            beta02 = mean(beta02, na.rm = T),
            SE02 = mean(SE02, na.rm = T),
            P02 = mean(P02, na.rm = T)) %>% 
  ungroup()




## Change as a result of feedback #####
# feedback valence ######
df5_stats <- df5 %>% 
  left_join(behavior) %>% 
  filter(template == template_,
         profile != 'Any') %>% 
  group_by(roi,sub,profile) %>% 
  mutate(rf_diff = rf - lag(rf)) %>% 
  filter(run == 2) %>% 
  mutate(feedback.d = if_else(feedback == 'neg',0,1)) %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(rf,na.rm = T)) %>% 
  filter(na_check != 0) %>% 
  ungroup() %>% 
  group_by(roi) %>% 
  mutate(beta = round(summary(lmer(rf_diff ~ feedback.d + 
                                     (1 + feedback.d | sub)))$coefficients[2,1],3),
         P = round(summary(lmer(rf_diff ~ feedback.d + 
                                  (1 + feedback.d | sub)))$coefficients[2,5],3))

df5_stats_unique = df5_stats %>% 
  group_by(roi) %>% 
  summarize(beta = unique(beta),
            P = unique(P)) %>% 
  filter(is.na(parse_number(roi)))

# feedback congruence ######
df5_stats <- df5 %>% 
  left_join(behavior) %>% 
  filter(template == template_,
         profile != 'Any') %>% 
  group_by(roi,sub,profile) %>% 
  mutate(rf_diff = rf - lag(rf)) %>% 
  filter(run == 2) %>% 
  mutate(congruence.d = if_else(congruence == 'Incongruent',0,1)) %>% 
  group_by(roi) %>% 
  mutate(na_check = mean(rf,na.rm = T)) %>% 
  filter(na_check != 0) %>% 
  ungroup() %>% 
  group_by(roi) %>% 
  mutate(beta = round(summary(lmer(rf_diff ~ congruence.d + 
                                     (1 + congruence.d | sub)))$coefficients[2,1],3),
         P = round(summary(lmer(rf_diff ~ congruence.d + 
                                  (1 + congruence.d | sub)))$coefficients[2,5],3)) 

df5_stats_unique = df5_stats %>% 
  group_by(roi) %>% 
  summarize(beta = unique(beta),
            P = unique(P)) %>% 
  filter(is.na(parse_number(roi)))

### Figure #####
# F4A #####
F4A_sum_paper <- df5 %>% 
  left_join(behavior) %>% 
  filter(template == template_,
         profile != 'Any',
         run != 0) %>% 
  mutate(rf_perc = ((rf - 20) / 20)*100) %>% 
  group_by(roi,run,congruence) %>% 
  summarize(rf_mean = mean(rf,na.rm = T),
            rf_sem = sd(rf, na.rm = T)/ sqrt(n()),
            rf_sd = sd(rf,na.rm = T)*1.96,
            rf_perc = mean(rf_perc)) %>% 
  filter(roi %in% c('hippocampus',
                    'hippocampus_anterior','hippocampus_posterior',
                    'ns_mentalizing_dmPFC','neurosynth_mentalizing',
                    'ns_mentalizing_LTempPole','ns_mentalizing_RTPJ')) %>% 
  left_join(rois) %>% 
  left_join(F4A_stats) %>% 
  mutate(roi_label_paper = factor(roi_label_paper,levels = c('Hippocampus','Anterior\nHipp','Posterior\nHipp',
                                                             'Mentalizing\nnetwork','L Temporal\nPole',
                                                             'R TPJ','dmPFC')),
         sig01 = if_else(P01 < .01,'**',if_else(P01 < .05,'*','')),
         sig12 = if_else(P12 < .01,'**',if_else(P12 < .05,'*','')),
         sig02 = if_else(P02 < .01, '**',if_else(P02 < .05,'*','')),
         segment_startx = if_else(P12 < .05,1.1,NA),
         segment_endx = if_else(P12 < .05,1.9,NA),
         segment_starty = if_else(P12 < .05,64,NA),
         segment_endy = if_else(P12 < .05,64,NA),
         rf_sem_perc = if_else(str_detect(roi_label_paper,'Hipp') & run == 2,
                               (SE02/20)*100,(SE01/20)*100))


F4A <- ggplot(F4A_sum_paper %>% 
                filter(roi_label_paper == 'Hippocampus' | 
                         roi_label_paper == 'Mentalizing\nnetwork'), 
              aes(x = run, y = rf_perc, group = congruence, color = congruence)) +
  facet_wrap(vars(roi_label_paper),ncol = 7, labeller = as_labeller(rois_char_paper)) +
  geom_errorbar(aes(ymin = rf_perc - 1.96*rf_sem_perc, ymax = rf_perc + 1.96*rf_sem_perc), 
                width = .1, linewidth = .4,position = position_dodge(width = .2)) +
  geom_point(size = 1, position = position_dodge(width = .2)) +
  geom_line(position = position_dodge(width = .2)) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1,2),labels = c('Pre-\nfeedback','Post-\nfeedback'),
                     limits = c(.8,2.2)) +
  scale_color_manual(values = c("darkseagreen4","firebrick3"),
                     labels = c("Congruent","Incongruent")) +
  lims(y = c(-6,71)) +
  geom_text(aes(label = sig01),x = 1,y = 57.5, size = 5, color = 'black') +
  geom_text(aes(label = sig02),x = 2,y = 57.5, size = 5,color = 'black') +
  geom_text(aes(label = sig12),x = 1.5,y = 66, size = 5,color = 'black') +
  geom_segment(aes(x = segment_startx,y = segment_starty,
                   xend = segment_endx,yend = segment_endy),
               color = 'black') +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = .3) +
  labs(x = 'Resting state run',y = 'Reactivation\n% increase',
       color = 'Congruence') +
  theme(strip.text.y = element_text(size = 8),
        plot.title = element_text(hjust = .5,size = 14),
        legend.position = 'none')
# F4B #####

F4B <- ggplot(F4A_sum_paper %>% 
                filter(roi_label_paper %in% c('Anterior\nHipp','Posterior\nHipp',
                                              'R TPJ','dmPFC')), 
              aes(x = run, y = rf_perc, group = congruence, color = congruence)) +
  facet_wrap(vars(roi_label_paper),ncol = 7, labeller = as_labeller(rois_char_paper)) +
  geom_errorbar(aes(ymin = rf_perc - 1.96*rf_sem_perc, ymax = rf_perc + 1.96*rf_sem_perc), 
                width = .1, linewidth = .4,position = position_dodge(width = .2)) +
  geom_point(size = 1, position = position_dodge(width = .2)) +
  geom_line(position = position_dodge(width = .2)) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1,2),labels = c('Pre-\nfeedback','Post-\nfeedback'),
                     limits = c(.8,2.2)) +
  scale_color_manual(values = c("darkseagreen4","firebrick3"),
                     labels = c("Congruent","Incongruent")) +
  lims(y = c(-6,71)) +
  geom_text(aes(label = sig01),x = 1,y = 57.5, size = 5, color = 'black') +
  geom_text(aes(label = sig02),x = 2,y = 57.5, size = 5,color = 'black') +
  geom_text(aes(label = sig12),x = 1.5,y = 66, size = 5,color = 'black') +
  geom_segment(aes(x = segment_startx,y = segment_starty,
                   xend = segment_endx,yend = segment_endy),
               color = 'black') +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = .3) +
  labs(x = 'Resting state run',y = 'Reactivation\n% increase',
       color = 'Congruence') +
  theme(strip.text.y = element_text(size = 8),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = 'bottom')


# F4 #####
blank <- ggplot() + theme_void()

top_row <- ggarrange(
  blank, F4A, blank,
  ncol = 3,
  widths = c(1, 4, 1)
)

F4 <- ggarrange(top_row,F4B,
                nrow = 2, ncol = 1,
                heights = c(1,1))

annotate_figure(F4,
                top = text_grob('Effect of run and feedback congruence on reactivation frequency', 
                                face = "bold", size = 18))
ggsave("Fig4.jpg", width = 9, height = 6)


