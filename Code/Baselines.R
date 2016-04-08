library(class)

##########################################
# Baselines feature selection algorithms #
##########################################
# RANDOM COLUMNS #
random_cols <- function(data, objective, d){
   s = sample(ncol(data), d, replace = F)
   names(data)[s]
}

# WRAP kNN #
wrap_kNN <- function(data, objective, d){

   sel_columns = c()

   for (i in 1:d){

      cand_columns <- setdiff(names(data), sel_columns)

      scores <- sapply(cand_columns, function(col){
         cand_set <- c(sel_columns, col)
         kNN_strength(data[,cand_set,drop=F], objective)
      })

      best_col <- cand_columns[which.max(scores)]
      sel_columns <- c(sel_columns, best_col)
   }

   sel_columns

}

# MUTUAL INFORMATION RANKING #
entropy <- function(s1){
   freq <- table(s1)[]/length(s1)
   freq <- freq[freq != 0]
   sum(sapply(freq, function(x){x * log2(x)})) * -1
}

joint_entropy <- function(s1, s2){
   freq <- c(table(s1,s2)/length(s1))
   freq <- freq[freq != 0]
   sums <- freq * log2(freq)
   sum(sums) * -1
}

mutual_information <- function(s1, s2){
   entropy(s1) + entropy(s2) - joint_entropy(s1,s2)
}

MI_rank <- function(data, objective, d){

   # Preprocesses data
   cols <- lapply(data, cut, breaks = 8)
   obj  <- as.factor(objective)

   MIs <- sapply(cols, mutual_information, obj)
   ranking <- sort(MIs, decreasing = T)

   names(ranking)[1:d]

}

list2array <- function(L)
   array(unlist(L), dim = c(nrow(L[[1]]), ncol(L[[1]]), length(L)))

# CLUSTINE #
clustine_feature_select <- function(data, objective, d){

   # Cluster names
   clu_names = unique(objective)

   # Cluster counts
   clu_count = table(objective)

   # Cluster means
   clu_mean =  aggregate(data, by = list(Class=objective), mean, na.rm = T)
   clu_mean =  clu_mean[order(clu_mean$Class), ]
   clu_mean = t(clu_mean[,colnames(clu_mean) != 'Class'])

   # Cluster variances
   clu_sigma_agg =  aggregate(data, by = list(Class=objective), var, na.rm = T)
   clu_sigma_agg =  clu_sigma_agg[order(clu_sigma_agg$Class), ]

   sigma_clusters = clu_sigma_agg[[1]]
   clu_sigma_agg =  clu_sigma_agg[,-1]
   sigma_per_clu = lapply(sigma_clusters, function(clu){
      M = diag(clu_sigma_agg[clu,])
      rownames(M) = colnames(clu_sigma_agg)
      colnames(M) = colnames(clu_sigma_agg)
      M
   })
   clu_sigma = list2array(sigma_per_clu)


    # Gets descriptive columns and comment
    charac_col = list()

    for (class in clu_names){

      out = list()

      # Gets means and variances of columns in selection
      sel_means = clu_mean[,class]
      sel_vars  = diag(clu_sigma[,,class])
      sel_count = clu_count[class]

      # Gets moments of the remainder
      out_class = setdiff(clu_names, class)
      out_moments = aggregate_moments(
         counts = clu_count[out_class],
         means  = clu_mean[,out_class],
         covm   = clu_sigma[,,out_class]
      )
      out_count = out_moments$count
      out_means = out_moments$means
      out_vars  = out_moments$vars

      # Cohen d to rank
      pooled_vars = (sel_count * sel_vars + out_count * out_vars) /
         (sel_count + out_count)
      pooled_sds  = sqrt(pooled_vars)
      cohen_ds    = abs((sel_means - out_means) / pooled_sds)

      # Output
      charac_col[[class]] = names(sort(cohen_ds, decreasing = T))[1:d]
    }

    charac_col = unlist(charac_col)
    table_charac_col = sort(table(charac_col), decreasing = T)
    out = names(table_charac_col)[1:3]

   cat("Done\n")
   return(out)


}


clustine_just_compute <- function(data, objective, d){

   # Cluster names
   clu_names = unique(objective)

   # Cluster counts
   clu_count = table(objective)

   # Cluster means
   clu_mean =  aggregate(data, by = list(Class=objective), mean, na.rm = T)
   clu_mean =  clu_mean[order(clu_mean$Class), ]
   clu_mean = t(clu_mean[,colnames(clu_mean) != 'Class'])

   # Cluster variances
   clu_sigma_agg =  aggregate(data, by = list(Class=objective), var, na.rm = T)
   clu_sigma_agg =  clu_sigma_agg[order(clu_sigma_agg$Class), ]

   sigma_clusters = clu_sigma_agg[[1]]
   clu_sigma_agg =  clu_sigma_agg[,-1]
   sigma_per_clu = lapply(sigma_clusters, function(clu){
      M = diag(clu_sigma_agg[clu,])
      rownames(M) = colnames(clu_sigma_agg)
      colnames(M) = colnames(clu_sigma_agg)
      M
   })
   clu_sigma = list2array(sigma_per_clu)

   cat(as.character(clu_names), as.character(clu_count),
       as.character(clu_mean), as.character(sigma_per_clu), file = '/dev/null')

   return(names(data)[1:d])


}



###########################################
# Cross-Validation score with Naive Bayes #
###########################################
score <- function(pred, truth, F1=TRUE){
   cl  <- sort(unique(truth))
   cll <- as.character(cl)
   truth <- factor(truth, levels = cl, labels = cll)
   pred  <- factor(pred,  levels = cl, labels = cll)
   tab <- table(pred, truth)

   accuracy <- sum(diag(tab)) / sum(tab)
   if (!F1) return(accuracy)

   class_pred_count <- apply(tab, 1, sum)
   class_true_count <- apply(tab, 2, sum)
   class_pred_hits  <- diag(tab)
   precision <- ifelse(class_pred_count > 0,
                       class_pred_hits / class_pred_count,
                       1)
   recall    <- ifelse(class_true_count > 0,
                       class_pred_hits / class_true_count,
                       1)
   F1 <- 2 * precision * recall / (precision + recall)

   if (length(class_true_count) < 3){
      mino_class<- which.min(class_true_count)
      avg_F1 <- F1[mino_class]
   } else {
      avg_F1 <- sum(F1 * class_true_count, na.rm = TRUE) / sum(class_true_count)
   }

   return(avg_F1)
}



kNN_strength <- function(set, target, folds=5, F1=TRUE){


   # Deals with NAs
   set <- na.omit(set)
   target <- target[attr(set, 'row.names')]

   # If necessary, truncates
   if (nrow(set) > 5000){
      set <- set[1:5000,, drop=F]
      target <- target[1:5000]
   }

   # Computes witdth of the folds
   bin_width <- floor(nrow(set) / folds)
   folds <- lapply(1:folds, function(i){
      c((i - 1) * bin_width, i * bin_width - 1)
   })


   scores <- c()
   if (length(folds[[1]]) == 0) browser()

   for (fold in folds){
      train <- set[-fold[1]:-fold[2],, drop=FALSE]
      train_target <- target[-fold[1]:-fold[2]]
      test  <- set[fold[1]:fold[2],, drop=FALSE]
      test_taget <- target[fold[1]:fold[2]]

      knn_predict <- c()
      try({

         knn_predict <- knn(train,
                            test,
                            as.factor(train_target),
                            k = 5,
                            use.all = FALSE)
      }, silent = TRUE)

      if (length(knn_predict) > 1) {
         F1_score =  score(knn_predict, test_taget, F1)
         scores <- c(scores, F1_score)
      }
   }

   out <- if(length(scores) > 1){
      mean(scores, na.rm = TRUE)
   } else {
      NA
   }

   return(out)
}