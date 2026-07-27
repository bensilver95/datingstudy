# ==============================================================================
# Supplemental Analyses
# Reproduces the three analyses reported under "Supplemental Results" in
# Supplemental materials.docx:
#   1. RSA with beginning vs end of videos (5s / 15s windows)   -> Supplemental Table 1
#   2. Temporal IS-RSA (all templates x all ROIs)                -> Supplemental Figure 1
#   3. Representational change (valence/congruence/initial interest,
#      all 8 ROIs, per-condition-level cell means)                -> Supplemental Figure 2
# ==============================================================================

library(tidyverse)
library(lmerTest)
library(ggpubr)

BASE <- '/home/bms2202/dating/'
OUT  <- getwd()

##### fMRI analyses ########
#### preparing the labels #####
p = 'fmriprep'
top_rois_strQ1 = c('insula',
                   'neurosynth_mentalizing','ns_mentalizing_dmPFC',
                   'ns_mentalizing_vmPFC','ns_mentalizing_Precuneus',
                   'ns_mentalizing_LTPJ','ns_mentalizing_RTPJ',
                   'ns_mentalizing_LTempPole','ns_mentalizing_RTempPole')

template_ = 'wholevideo'

# create ROI labeller
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

# ==============================================================================
# 1. RSA with beginning vs end of videos (5s / 15s windows)
#    -> Supplemental Table 1
# ==============================================================================

#### Did the mentalizing network represent romantic interest? ######
### prep #####
df2 <- read_csv(file.path(BASE,'scripts/Reviews_testing/neurbehav_allqs_noruns_spatial_15secs.csv'))

### stats (permutations) ####
## one segment/ROI set at a time, mirroring the main-text 10s blocks #####
run_rsa_permute <- function(seg_label){
  d <- df2 %>% filter(template2 == seg_label)

  d_sum <- d %>%
    group_by(roi) %>%
    mutate(na_check = mean(brain_correlation, na.rm = T)) %>%
    filter(!is.na(na_check)) %>%
    ungroup() %>%
    group_by(roi,profile,video) %>%
    summarize(cor = cor(1 - brain_correlation,behavior_distance,
                        method = "spearman",
                        use = "complete.obs"), .groups = "drop") %>%
    mutate(roi = as.character(roi))

  permute_raw <- vector("list", 1000)
  for (i in 1:1000) {
    set.seed(i)
    permute_raw[[i]] <- d %>%
      group_by(roi) %>%
      mutate(na_check = mean(brain_correlation,na.rm = T)) %>%
      filter(!is.na(na_check)) %>%
      ungroup() %>%
      group_by(roi,profile,video) %>%
      mutate(brain_correlation = sample(brain_correlation)) %>%
      summarize(cor = cor(1 - brain_correlation,behavior_distance,
                          method = "spearman",
                          use = "complete.obs"), .groups = "drop") %>%
      ungroup() %>%
      group_by(roi) %>%
      summarize(cor = mean(cor), .groups = "drop") %>%
      mutate(iteration = i)
  }

  permute <- bind_rows(permute_raw) %>%
    bind_rows(d_sum %>%
                group_by(roi) %>%
                summarize(cor = mean(cor), .groups = "drop") %>%
                mutate(iteration = 0)) %>%
    mutate(real = if_else(iteration == 0,1,0))

  permute %>%
    group_by(roi) %>%
    summarize(obs_cor = cor[real == 1],
              p = (1 + sum(cor[real == 0] >= cor[real == 1])) / (1 + sum(real == 0)),
              .groups = "drop") %>%
    rename(cor = obs_cor) %>%
    mutate(segment = seg_label)
}

table1_roi_map <- c('neurosynth_mentalizing' = 'Mentalizing network',
                    'ns_mentalizing_RTPJ'   = 'rTPJ',
                    'ns_mentalizing_dmPFC'  = 'dmPFC')

Table1_results <- bind_rows(
  run_rsa_permute('15secs'),
  run_rsa_permute('5secs'),
  run_rsa_permute('last15secs'),
  run_rsa_permute('last5secs')
) %>%
  filter(roi %in% names(table1_roi_map)) %>%
  mutate(ROI = table1_roi_map[roi],
         Segment = if_else(segment %in% c('5secs','15secs'), 'Beginning','End'),
         `Time window` = recode(segment,
                                '15secs' = 'First 15 seconds',
                                '5secs' = 'First 5 seconds',
                                'last15secs' = 'Last 15 seconds',
                                'last5secs' = 'Last 5 seconds'),
         `Observed cor` = round(cor,3),
         p = round(p,3)) %>%
  select(ROI, Segment, `Time window`, `Observed cor`, p)

cat("\n==== Supplemental Table 1: RSA beginning vs end (5s/15s) ====\n")
print(as_tibble(Table1_results), n = Inf)

write_csv(Table1_results, file.path(OUT, "supp_table1_rsa_5s_15s.csv"))

# ==============================================================================
# 2. Temporal IS-RSA
#    -> Supplemental Figure 1 ("Brain-behavior correlations across ROIs")
# ==============================================================================

### prep #####
df2_temporal <- read_csv(file.path(BASE,'scripts/Reviews_testing/neurbehav_allqs_noruns_temporal.csv'))

df2_temporal_sum <- df2_temporal %>%
  filter(template2 == '10secs' | template2 == 'last10secs' | is.na(template2)) %>%
  mutate(template2 = if_else(is.na(template2), 'Whole',template2)) %>%
  group_by(template2, roi) %>%
  mutate(na_check = mean(brain_correlation,na.rm = T)) %>%
  filter(!is.na(na_check)) %>%
  ungroup() %>%
  group_by(template2,roi,profile,video) %>%
  summarize(cor = cor(1 - brain_correlation,behavior_distance,
                      method = "spearman",
                      use = "complete.obs"), .groups = "drop") %>%
  mutate(roi = as.character(roi))

### stats: t-tests per template x ROI #####
run_temporal_ttest <- function(seg_label){
  d <- df2_temporal_sum %>% filter(template2 == seg_label)
  out <- data.frame(roi = character(), t = numeric(), p = numeric(), cor = numeric())
  for (rr in unique(d$roi)) {
    dr <- d[d$roi == rr,]$cor
    test <- t.test(dr, alternative = "greater", mu = 0)
    out <- rbind(out, data.frame(roi = rr, t = test$statistic, p = test$p.value, cor = mean(dr)))
  }
  out$template2 <- seg_label
  out
}

Supp_Fig1_results <- bind_rows(
  run_temporal_ttest('Whole'),
  run_temporal_ttest('10secs'),
  run_temporal_ttest('last10secs')
)

cat("\n==== Supplemental Figure 1: Temporal IS-RSA, all templates, all ROIs ====\n")
print(as_tibble(Supp_Fig1_results), n = Inf)

mentalizing_temporal <- Supp_Fig1_results %>% filter(roi == 'neurosynth_mentalizing', template2 == 'Whole')
cat(sprintf("\nMentalizing network (whole video): observed cor = %.3f, p = %.2f\n",
            mentalizing_temporal$cor, mentalizing_temporal$p))

write_csv(Supp_Fig1_results, file.path(OUT, "supp_fig1_temporal_isrsa.csv"))

### permutation nulls per template x ROI (background violins) #####
run_temporal_permute <- function(seg_label){
  if (seg_label == 'Whole') {
    d <- df2_temporal %>% filter(is.na(template2))
  } else {
    d <- df2_temporal %>% filter(template2 == seg_label)
  }

  permute_raw <- vector("list", 1000)
  for (i in 1:1000) {
    set.seed(i)
    permute_raw[[i]] <- d %>%
      group_by(roi) %>%
      mutate(na_check = mean(brain_correlation,na.rm = T)) %>%
      filter(!is.na(na_check)) %>%
      ungroup() %>%
      group_by(roi,profile,video) %>%
      mutate(brain_correlation = sample(brain_correlation)) %>%
      summarize(cor = cor(1 - brain_correlation,behavior_distance,
                          method = "spearman",
                          use = "complete.obs"), .groups = "drop") %>%
      ungroup() %>%
      group_by(roi) %>%
      summarize(cor = mean(cor), .groups = "drop") %>%
      mutate(iteration = i)
  }
  bind_rows(permute_raw) %>% mutate(template2 = seg_label)
}

df2_permute_whole <- run_temporal_permute('Whole')
df2_permute_10s    <- run_temporal_permute('10secs')
df2_permute_last10 <- run_temporal_permute('last10secs')

roi_levels_fig1 <- c('Mentalizing\nnetwork','L TPJ','R TPJ','L Temporal\nPole',
                     'R Temporal\nPole','Precuneus','dmPFC','vmPFC')

df2all_permute_paper <- bind_rows(df2_permute_whole, df2_permute_10s, df2_permute_last10) %>%
  left_join(rois, by = "roi") %>%
  mutate(roi_label_paper = factor(roi_label_paper, levels = roi_levels_fig1),
         template2 = factor(template2, levels = c('Whole','10secs','last10secs')),
         Main = if_else(template2 == 'Whole',0,1)) %>%
  filter(!is.na(roi_label_paper))

## Figure: violin + jitter, all templates, all ROIs #####
df2_temporal_sum_paper <- df2_temporal_sum %>%
  mutate(template2 = factor(template2, levels = c('Whole','10secs','last10secs'))) %>%
  filter(roi %in% top_rois_strQ1, roi != 'insula') %>%
  left_join(rois, by = "roi") %>%
  mutate(roi_label_paper = factor(roi_label_paper, levels = roi_levels_fig1)) %>%
  group_by(template2,roi) %>%
  mutate(mean_cor = mean(cor, na.rm = T),
         Main = if_else(template2 == 'Whole',0,1))

F_Supp1A <- ggplot(df2_temporal_sum_paper %>%
                filter(roi_label_paper == 'Mentalizing\nnetwork'),
              aes(x = roi_label_paper, y = cor,
                  group = template2, color = template2,fill = template2)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .4) +
  geom_violin(data = df2all_permute_paper %>%
                filter(roi_label_paper == 'Mentalizing\nnetwork'),aes(x = roi_label_paper,
                                                                      y = cor,
                                                                      group = template2,
                                                                      color = template2,
                                                                      fill = template2),
              alpha = .6, width = .6,
              position = position_dodge(width = .9), color = NA, fill = 'gray') +
  geom_jitter(alpha = .2,position = position_jitterdodge()) +
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

F_Supp1B <- ggplot(df2_temporal_sum_paper %>%
                filter(roi_label_paper != 'Mentalizing\nnetwork'),
              aes(x = roi_label_paper, y = cor,
                  group = template2, color = template2,fill = template2)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .4) +
  geom_violin(data = df2all_permute_paper %>%
                filter(roi_label_paper != 'Mentalizing\nnetwork'),aes(x = roi_label_paper,
                                                                      y = cor,
                                                                      group = interaction(template2,roi_label_paper),
                                                                      color = template2,
                                                                      fill = template2),
              alpha = .6, width = .6,
              position = position_dodge(width = .9), color = NA, fill = 'gray') +
  geom_jitter(alpha = .2,position = position_jitterdodge()) +
  facet_grid(cols = vars(Main)) +
  theme_bw() +
  scale_x_discrete() +
  geom_point(aes(y = mean_cor), size = 2,position = position_dodge(width = .9)) +
  labs(y = "Neural dissimilarity-rating\ndistance correlation",
       color = 'Template',fill = 'Template') +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5,size = 14),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = 'none')

F_Supp1 <- ggarrange(F_Supp1A, F_Supp1B, nrow = 2, ncol = 1, heights = c(1,1))
ggsave(file.path(OUT, "supp_fig1_temporal_isrsa.jpg"), F_Supp1, width = 13, height = 7)

# ==============================================================================
# 3. Representational change: pre-post similarity by feedback valence,
#    feedback congruence, and initial romantic interest
#    -> Supplemental Figure 2 ("Impact of feedback valence, feedback
#       congruence, and initial romantic interest on neural template change")
# ==============================================================================

### prep #####
df4 <- read_csv(file.path(BASE,'scripts/Reviews_testing/acrossrun.csv'))

fit_cellmeans <- function(rr, comp){

  d <- df4 %>%
    filter(comp_type == 'within_pro',
           template  == template_,
           roi       == rr)

  if (comp == 'valence') {
    d$cond <- factor(if_else(d$feedback_valence == 'pos', 'Positive', 'Negative'),
                     levels = c('Negative','Positive'))
    comp_lab <- 'Feedback valence'

  } else if (comp == 'congruence') {
    d$cond <- factor(if_else(d$feedback_alignment == 'Congruent', 'Congruent', 'Incongruent'),
                     levels = c('Incongruent','Congruent'))
    comp_lab <- 'Feedback congruence'

  } else {
    fa <- if_else(d$feedback_alignment == 'Incongruent', 0, 1)
    fv <- if_else(d$feedback_valence   == 'neg', 0, 1)
    start.d <- if_else(fa == 0 & fv == 0, 1,
                       if_else(fa == 0 & fv == 1, 0,
                               if_else(fa == 1 & fv == 0, 0, 1)))
    d$cond <- factor(if_else(start.d == 1, 'High', 'Low'), levels = c('Low','High'))
    comp_lab <- 'Initial romantic interest'
  }

  d <- d %>% filter(!is.na(similarity))
  m  <- lmer(similarity ~ 0 + cond + (0 + cond | sub),
             data = d, control = lmerControl(optimizer = 'bobyqa'))
  co <- summary(m)$coefficients

  tibble(roi        = rr,
         Comparison = comp_lab,
         level      = sub('^cond', '', rownames(co)),
         beta       = round(co[, 'Estimate'], 3),
         SE         = round(co[, 'Std. Error'], 3),
         P          = co[, 'Pr(>|t|)'])
}

### stats: all 8 ROIs x 3 comparisons #####
roi_levels_supp <- rev(c('Mentalizing\nnetwork','L TPJ','R TPJ','L Temporal\nPole',
                         'R Temporal\nPole','Precuneus','dmPFC','vmPFC'))

keep_rois <- setdiff(intersect(unique(df4$roi), top_rois_strQ1), 'insula')

Supp_Fig2_valence <- map_dfr(keep_rois, ~ fit_cellmeans(.x, 'valence')) %>%
  left_join(rois, by = 'roi') %>%
  mutate(roi_label_paper = factor(roi_label_paper, levels = roi_levels_supp),
         Comparison = 'Feedback valence',
         lower = beta - 1.96 * SE,
         upper = beta + 1.96 * SE)

Supp_Fig2_congruence <- map_dfr(keep_rois, ~ fit_cellmeans(.x, 'congruence')) %>%
  left_join(rois, by = 'roi') %>%
  mutate(roi_label_paper = factor(roi_label_paper, levels = roi_levels_supp),
         Comparison = 'Feedback congruence',
         lower = beta - 1.96 * SE,
         upper = beta + 1.96 * SE)

Supp_Fig2_initial <- map_dfr(keep_rois, ~ fit_cellmeans(.x, 'initial')) %>%
  left_join(rois, by = 'roi') %>%
  mutate(roi_label_paper = factor(roi_label_paper, levels = roi_levels_supp),
         Comparison = 'Initial romantic interest',
         lower = beta - 1.96 * SE,
         upper = beta + 1.96 * SE)

Supp_Fig2_results <- bind_rows(Supp_Fig2_valence, Supp_Fig2_congruence, Supp_Fig2_initial) %>%
  mutate(Comparison = factor(Comparison,
                             levels = c('Feedback valence',
                                        'Feedback congruence',
                                        'Initial romantic interest')),
         sig   = P < .05,
         level = factor(level, levels = c('Negative','Positive',
                                          'Incongruent','Congruent',
                                          'Low','High')))

cat("\n==== Supplemental Figure 2: Representational change (all ROIs x all comparisons) ====\n")
print(as_tibble(Supp_Fig2_results %>% select(roi_label_paper, Comparison, level, beta, SE, P)), n = Inf)

write_csv(Supp_Fig2_results %>% select(ROI = roi_label_paper, Comparison, level, beta, SE, P),
          file.path(OUT, "supp_fig2_representational_change.csv"))

## Figure: full cell-means forest plot, mentalizing network + 7 sub-regions #####
pal   <- c(Negative = '#e0736f', Positive = '#b3241d',
          Incongruent = '#7fc97f', Congruent = '#1b7837',
          Low = '#9ecae1', High = '#2171b5')
dodge <- position_dodge(width = .7)

F_Supp2A <- ggplot(Supp_Fig2_results %>% filter(roi_label_paper == 'Mentalizing\nnetwork'),
              aes(x = beta, y = roi_label_paper, color = level, group = level)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, position = dodge) +
  geom_point(size = 3, position = dodge) +
  facet_grid(cols = vars(Comparison)) +
  scale_color_manual(values = pal, name = 'Feedback condition') +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta") +
  scale_x_continuous(n.breaks = 10, limits = c(-0.09, 0.09)) +
  scale_y_discrete(expand = expansion(mult = .15)) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title = element_blank(),
        strip.text = element_text(size = 14),
        plot.title = element_text(hjust = .5, size = 16),
        legend.position = "none")

F_Supp2B <- ggplot(Supp_Fig2_results %>% filter(roi_label_paper != 'Mentalizing\nnetwork'),
              aes(x = beta, y = roi_label_paper, color = level, group = level)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, position = dodge) +
  geom_point(size = 3, position = dodge) +
  facet_grid(cols = vars(Comparison)) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta") +
  scale_color_manual(values = pal, name = 'Feedback condition') +
  scale_x_continuous(n.breaks = 10, limits = c(-0.09, 0.09)) +
  scale_y_discrete(expand = expansion(mult = .15)) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(hjust = .5, size = 16),
        legend.position = "bottom")

F_Supp2 <- ggarrange(F_Supp2A, F_Supp2B, ncol = 1, nrow = 2, heights = c(1, 3), labels = NULL)

F_Supp2 <- annotate_figure(F_Supp2,
                top = text_grob("Impact of feedback valence, feedback congruence,\nand initial romantic interest on neural template change",
                                face = "bold", size = 18, hjust = .4))

ggsave(file.path(OUT, "supp_fig2_representational_change.jpg"), F_Supp2, width = 12, height = 6)

cat("\nDONE\n")
