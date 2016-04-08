source('Clustine.R', chdir = TRUE)
source('Baselines.R', chdir = TRUE)
library(foreign)



# Gets files and sorts them by size
files_location <- "../Data"
file_list <- list.files(path = files_location, pattern = "*.arff$")
sizes <- file.info(paste0(files_location, "/", file_list))$size
file_list <- file_list[order(sizes)]

OUT_FILE <- "ExperimentsResults2.log"
file.rename(OUT_FILE, paste0("OLD_", OUT_FILE))
cat('Algorithm', 'File', 'ncol', 'Runtime', 'F1 Score\n',
    sep = '\t', file = OUT_FILE, append = T)


###############
# EXPERIMENTS #
###############
evaluate_strategy <- function(col_selector, file, clu_assign, D, file_name = ''){

   cat('Evaluating:', as.character(match.call()[2]), '\n')

   # Runs strategy
   cat('Chosing columns....')
   TIME <- proc.time()["elapsed"]
   cols_to_evaluate = col_selector(file, clu_assign, D)
   TIMEDIFF <- proc.time()["elapsed"] - TIME
   print(cols_to_evaluate)

   # Evaluates strategy
   cat("Evaluating classification quality....")
   F1_score = kNN_strength(set = file[,cols_to_evaluate], target = clu_assign)
   print(F1_score)

   # Logging the result
   cat(as.character(match.call()[2]), file_name, D, TIMEDIFF, paste0(F1_score, '\n'),
         sep = '\t', file = OUT_FILE, append = T)

   cat("\n")
}

for (arff_file in file_list){
   cat("\n**** Doing file", arff_file, "\n")

   tryCatch({

      # Reads, shuffles
      file  = read.arff(paste0(files_location, "/", arff_file))
      file  = file[sample(1:nrow(file), nrow(file), replace=FALSE),]
      row.names(file) = 1:nrow(file)

      # Removes categorical data
      col_types = sapply(file, function(c) class(c))
      num_data  = (col_types == "numeric")
      file      = file[,num_data]

      # Cluster analysis
      clu_assign = kmeans(file, centers=4, nstart = 5)$cluster

      for (trial in 1:3){

         # Clustine Strategy
         evaluate_strategy(clustine_feature_select, file, clu_assign, 3, arff_file)

         # Clustine Just compute
         evaluate_strategy(clustine_just_compute, file, clu_assign, 3, arff_file)

         # Wrapper strategy
         evaluate_strategy(wrap_kNN, file, clu_assign, 3, arff_file)

         # MI Ranking strategy
         evaluate_strategy(MI_rank, file, clu_assign, 3, arff_file)

         # Random strategy a few times
         evaluate_strategy(random_cols, file, clu_assign, 3, arff_file)
         evaluate_strategy(random_cols, file, clu_assign, 3, arff_file)

      }

      cat("Done\n")
   },

   error = function(e){
      cat("ERROR!\n")
      print(e)
   })
}