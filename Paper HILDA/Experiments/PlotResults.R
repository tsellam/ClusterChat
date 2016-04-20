library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# Loads data
file_data = read.delim("ExperimentsResults6.log", stringsAsFactors = F)

# BLack lists
results = file_data %>%
            filter(!File %in% c('internet_usage.arff',
                                'liver.arff',
                                'diabetes.arff',#'adult.arff', 'bank.arff', 'communities.arff',
                                'glass.arff',
                                'vowel.arff',
                                'pendigits.arff'))

# Aggregates several runs
results = results %>%
            group_by(Algorithm, File) %>%
            summarise(Runtime = mean(Runtime, na.rm = T),
                      F1 = mean(F1.Score, na.rm = T),
                      Redundancy = mean(Redundancy, na.rm = T))

# Orders the levels
results$File = factor(results$File,
                      levels = c('liver.arff',
                                 'shape.arff',
                                 'glass.arff',
                                 'vowel.arff',
                                 'breast.arff',
                                 'pendigits.arff',
                                 'magic.arff',
                                 'adult.arff',
                                 'letrec.arff',
                                 'communities.arff',
                                 'bank.arff',
                                 'insurance.arff',
                                 'internet_usage.arff',
                                 'musk.arff',
                                 'diabetes.arff',
                                 'diabetic_data.arff'
                                 ),
                      labels = c('Liver',
                                 'ShapeRecog',
                                 'Glass',
                                 'Vowels',
                                 'BreastCancer',
                                 'PenDigits',
                                 'MagicTelescope',
                                 'AdultCensus',
                                 'LetterRecog',
                                 'Crime',
                                 'BankMarleting',
                                 'Insurance',
                                 'InternetUsage',
                                 'MuskMolecules',
                                 'diabetes.arff',
                                 'Diabetic Data'
                                 ))

results$Algorithm = factor(results$Algorithm,
                     levels = c(
                                "clustine_feature_select",
                                "clustine_deduplicate",
                                "clustine_just_compute",
                                "Clustine Embedded",
                                "random_cols",
                                "MI_rank",
                                "wrap_kNN"
                                ),
                     labels = c(
                                 "Clustine",
                                 "Clustine Dedup.",
                                 "clustine_just_compute",
                                 "Clustine Embedded",
                                 "Random",
                                 "MutualInfo",
                                 "Wrap kNN"
                        ))

#########################
# Plots accuracy scores #
#########################
to_plot = results %>%
            filter(!Algorithm %in% c('clustine_just_compute'))

acc_plot = ggplot(data = to_plot,
                  mapping = aes(x = File, y = F1, fill=Algorithm)) +
            geom_bar(stat = 'identity', position = "dodge", width = .75) +
            scale_x_discrete('Dataset') +
            scale_y_continuous('Predict. Score (F1)', lim = c(0,1))

acc_plot = acc_plot +
            theme_few() +
            scale_fill_few() +
   theme(axis.text.x = element_text(angle = 15, hjust = 1))


print(acc_plot)
ggsave('F1_Scores.pdf', acc_plot, width = 10, height = 2)

#######################
# Plots deduplication #
#######################

to_plot = results %>%
   filter(!Algorithm %in% c('clustine_just_compute', 'Random')) %>%
   mutate(Redundancy = pmax(Redundancy, 0.01))

red_plot = ggplot(data = to_plot,
                  mapping = aes(x = File, y = Redundancy, fill=Algorithm)) +
   geom_bar(stat = 'identity', position = "dodge", width = .75) +
   scale_x_discrete('Dataset') +
   scale_y_continuous('Redundancy', lim = c(0,1))

red_plot = red_plot +
   theme_few() +
   scale_fill_few() +
   theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(red_plot)
ggsave('Red_Scores.pdf', red_plot, width = 10, height = 2)

#################
# Plots Timings #
#################

# Special case of Clustine stats
to_plot1 = results %>%
   filter(!Algorithm %in% c('clustine_just_compute'))%>%
   ungroup

to_plot2 = results %>%
   filter(Algorithm %in% c('Clustine Dedup.', 'clustine_just_compute')) %>%
   select(Algorithm, File, Runtime) %>%
   spread(Algorithm, Runtime) %>%
   mutate(ClustineEmbedded = pmax(`Clustine Dedup.` - clustine_just_compute, 0.01)) %>%
   mutate(File=File, #ncol=rep(NA,length(ncol)),
          Runtime=ClustineEmbedded,
          Redundancy=rep(NA,length(ncol)),
          F1=rep(NA,length(ncol)),
          Algorithm = rep('Clustine Embedded',length(ncol))) %>%
   select(-`Clustine Dedup.`, -clustine_just_compute, -ClustineEmbedded) %>%
   ungroup

to_plot = rbind(to_plot1, to_plot2) %>%
            filter(!Algorithm %in% c('Random', 'Clustine Dedup.'))

to_plot = to_plot %>%
   mutate(Runtime = ifelse(Runtime < 0.01, 0.01, Runtime)) %>%
   mutate(Exceed = ifelse(Runtime > 1.0, 'X', '')) %>%
   mutate(Runtime = ifelse(Runtime > 1.0, 1.0, Runtime))

time_plot = ggplot(data = to_plot,
                  mapping = aes(x = File, y = Runtime, fill=Algorithm, label = Exceed)) +
   geom_bar(stat = 'identity', position = "dodge", width = .75) +
   geom_text(aes(y=0.98), color="grey30", position = position_dodge(width=.75), size=4) +
   scale_x_discrete('Dataset') +
   scale_y_continuous('Exec. Time (s)', lim = c(0,1.0))

time_plot = time_plot +
   theme_few() +
   theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
   scale_fill_few()

print(time_plot)
ggsave('Timings.pdf', time_plot, width = 10, height = 2)
