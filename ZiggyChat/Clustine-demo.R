#library(tidyr)
library(ggplot2)
library(GGally)
library(ggthemes)
library(mclust)
library(cluster)
library(foreign)


PLOT_INDEX <<- 1

#################
# Preprocessing #
#################
preprocess <- function(file_data){

   vectors = lapply(file_data, function(col){
      if (is.numeric(col) & sum(is.na(col)) <= 0 * length(col)) col
      else NULL
   })

   data.frame(do.call(cbind, vectors))
}

###################
# Clustering Step #
###################
computeModel <- function(data, max_clusters = 4, sample_init_size = 200){
   cat("Clustering a data set with", ncol(data), "columns and",
       nrow(data), "rows.\n")

  set.seed(55555)

  # Removes columns with only one distinct value
  one_val <- sapply(data, function(col) length(na.omit(unique(col))) < 2)
  data <- data[,!one_val, drop=F]
  if (ncol(data) <= 1){
    warning("Data set did not survive cleaning, sorry!")
    return(NULL)
  }

  if (nrow(data) <= 5){
    warning("Not enough tuples - can't work!")
    return(NULL)
  }

   if (nrow(data) > sample_init_size)
      sample_init = sample(nrow(data), size=sample_init_size,  replace = F)
   else
      sample_init = 1:nrow(data)

   model = Mclust(data, G=2:max_clusters, modelNames = c('EEI', 'VVI', 'EVI'),
                  initialization=list(subset=sample_init))

   print(model)

   return(model)
}

compute_col_clusters <- function(data, max_cols = 3, black_list = c()){
   cat('Clustering columns.\n')

   cor_matrix = 1 - cor(data)
   cor_matrix = cor_matrix[!rownames(cor_matrix) %in% black_list,
                           !colnames(cor_matrix) %in% black_list]

   clu_sols = lapply(2:max_cols, function(k)  pam(as.dist(cor_matrix), k))
   sils = sapply(clu_sols, function(clu) clu$silinfo$avg.width)
   col_clusters = clu_sols[[which.max(sils)]]

   return(col_clusters)
}

###############
# Description #
###############
aggregate_moments <- function(counts, means, covm){

   # Trivial case
   if (length(dim(covm)) < 3){
      out = list(
         means = means,
         vars = diag(covm),
         counts = counts
      )
      return(out)
   }

   # Prepares structures
   means = lapply(1:dim(means)[2], function(k) means[,k])
   vars  = lapply(1:dim(covm)[3],  function(k) diag(covm[,,k]))
   if (length(vars) != length(means))
      warning('SUSPICIOUS! Different number of classes for means and variance matrices')

   # Performs the acual aggregation
   base_n     = counts[1]
   base_means = means[[1]]
   base_vars  = vars[[1]]

   for (k in 2:length(means)){
      new_n     = counts[k]
      new_means = means[[k]]
      new_vars  = vars[[k]]

      base_vars_1 = ( base_n * base_vars + new_n * new_vars ) / (base_n + new_n)
      base_vars_2 = (base_n * new_n) / (base_n + new_n)^2 * (base_means - new_means)^2
      base_vars   =  base_vars_1 + base_vars_2

      base_means = ( base_n * base_means + new_n * new_means ) / (base_n + new_n)

      base_n = base_n + new_n
   }

   list(count = base_n, means = base_means, vars = base_vars)

}


clusters_stats <- function(model, sig = 0.01){

   cat("Cluster testing phase\n")

   # Gets descriptive columns and comment
   cluster_tests = list()

   for (class in unique(model$classification)){

      out = list()

      # Gets means and variances of columns in selection
      sel_means = model$parameters$mean[,class]
      sel_vars  = diag(model$parameters$variance$sigma[,,class])
      sel_count = sum(model$z[,class])
      out[['means']] = sel_means
      out[['vars']]  = sel_vars
      out[['count']] = sel_count

      # Gets moments of the remainder
      out_class = setdiff(unique(model$classification), class)
      out_moments = aggregate_moments(
         counts = apply(model$z[,out_class,drop=F], 2, sum),
         means  = model$parameters$mean[,out_class],
         covm   = model$parameters$variance$sigma[,,out_class]
      )
      out_count = out_moments$count
      out_means = out_moments$means
      out_vars  = out_moments$vars

      # Wald Test to prune out
      diff_vars = sqrt(sel_vars / sel_count + out_vars / out_count)
      Ws = (sel_means - out_means) / diff_vars
      Walds = -abs(Ws) < qnorm(sig/2)
      out[['walds_val']]  = Ws
      out[['walds_test']] = Walds

      # Cohen d to rank
      pooled_vars = (sel_count * sel_vars + out_count * out_vars) /
                           (sel_count + out_count)
      pooled_sds  = sqrt(pooled_vars)
      cohen_ds    = (sel_means - out_means) / pooled_sds
      out[['cohen_ds']] = cohen_ds

      # Output
      cluster_tests[[class]] = out
   }

   cat("Done\n")
   return(cluster_tests)

}

interpret_d <- function(d){
   if (d >= 0) 'high'
   else        'low'
}

describe_clusters <- function(cluster_tests, black_list, max_cols=2){

      # Selects the columns to keep
      all_magnitudes <- lapply(cluster_tests, function(cluster_data){

        # Filters out blacklisted columns
        cand_cols = names(cluster_data$cohen_ds)
        cand_cols = cand_cols[!cand_cols %in% black_list]

        # Filters out uncharacteristic columns
        test_pass_cols = names(which(cluster_data$walds_test))
        cand_cols = cand_cols[cand_cols %in% test_pass_cols]

        # Extracts and absolute values Cohen's d
        abs(cluster_data$cohen_ds[cand_cols])

      })
      all_magnitudes <- unlist(all_magnitudes)

      col_names <- unique(names(all_magnitudes))
      avg_magnitudes <- sapply(col_names, function(col){
        col_magn <- all_magnitudes[names(all_magnitudes) == col]
        if (length(col_magn) != length(cluster_tests)) return(NA)
        mean(as.numeric(col_magn), na.rm = T)
      })

      avg_magnitudes <- na.omit(avg_magnitudes)
      char_scores    <- sort(avg_magnitudes, decreasing = T)[1:max_cols]
      char_columns   <- names(char_scores)

      if (length(char_columns) < max_cols){
        warning('Not enough characteristic columns!')
        return(list())
      }

      # Creates the descriptions
      cluster_descriptions = lapply(cluster_tests, function(cluster_data){

         # Produces a magnitude description
         cand_ds = cluster_data$cohen_ds[char_columns]
         magnitudes  = sapply(cand_ds, interpret_d)
         names(magnitudes) = char_columns

         return(magnitudes)
      })


      # Resolves conflicts
      for (clu1 in 1:(length(cluster_tests)-1)){
         for (clu2 in (clu1+1):length(cluster_tests)){

            desc1 = cluster_descriptions[[clu1]]
            desc2 = cluster_descriptions[[clu2]]

            # Case of conflict!
             if (all(desc1[sort(names(desc1))] == desc2[sort(names(desc2))])){
               for (col in names(desc1)){

                  if (abs(cluster_tests[[clu1]]$cohen_ds[col]) >
                        abs(cluster_tests[[clu2]]$cohen_ds[col])){

                     cluster_descriptions[[clu1]][col] = paste0(
                        'very ', cluster_descriptions[[clu1]][col]
                     )

                  } else {

                     cluster_descriptions[[clu2]][col] = paste0(
                        'very ', cluster_descriptions[[clu2]][col]
                     )

                  }
               }
            }

         }
      }

      return(cluster_descriptions)
}



wrap_cluster_description_html <- function(cluster_descriptions){

  out = '<ol>'

  for (k in 1:length(cluster_descriptions)){

    out = paste0(out, '<li>')
    clu_desc = cluster_descriptions[[k]]

    cols       = names(clu_desc)
    magnitudes = clu_desc

    for (i in 1:length(cols)){
      out = paste0(out, paste(magnitudes[i], cols[i]))
      if (i < length(cols)) out = paste0(out, ', ')
    }

    out = paste0(out, '</li>\n')
  }

  out = paste0(out, '</ol>')

}

wrap_cluster_description <- function(cluster_descriptions){

   out = ''

   for (k in 1:length(cluster_descriptions)){

      out = paste0(out, 'Cluster ', k, ': ')
      clu_desc = cluster_descriptions[[k]]

      cols       = names(clu_desc)
      magnitudes = clu_desc

      for (i in 1:length(cols)){
         out = paste0(out, paste(magnitudes[i], cols[i]))
         if (i < length(cols)) out = paste0(out, ', ')
      }

      out = paste0(out, '\n')
   }

   out

}

list_wrap_description <- function(cluster_descriptions){
   sapply(1:length(cluster_descriptions), function(i){

      clu_desc = cluster_descriptions[[i]]
      out = paste0('Cluster ',i,':\n')

      cols       = names(clu_desc)
      magnitudes = clu_desc
      for (i in 1:length(cols)){

         out = paste0(out, paste(magnitudes[i], cols[i]))
         if (i < length(cols))
            out = paste0(out, ',\n')
      }

      out

   })
}



###################
# Wrapping it up! #
###################
extract_char_cols <- function(cluster_descriptions){
   # Gets all columns used for description
   characteristic_col = unlist(lapply(cluster_descriptions, names))
   characteristic_col = unique(characteristic_col)
}

plot_explanation <- function(data, cluster_descriptions, model){

   cat('Plotting....\n')

   # Prepares columns
   characteristic_col = extract_char_cols(cluster_descriptions)
   if (length(characteristic_col) > 2) characteristic_col = characteristic_col[1:2]
   to_plot = data[,characteristic_col]
   to_plot = as.data.frame(lapply(data, jitter, factor = .15, amount=0))

   # Prepares labels
   labels <- list_wrap_description(cluster_descriptions)
   to_plot$labels = labels[model$classification]


   # Actual plot
   out_plot = ggplot(to_plot,
                     aes_string(characteristic_col[1],
                                characteristic_col[2],
                                color = 'labels',
                                fill = 'labels',
                                shape = 'labels')) +
      geom_point() +
      stat_density2d(aes(fill = labels, alpha=..level..),
                     geom="polygon",
                     n=20) +
      scale_color_discrete('Cluster labels') +
      scale_fill_discrete('Cluster labels') +
      scale_shape('Cluster labels') +
      scale_alpha(guide=FALSE) +
      theme_bw() +
      guides(fill = guide_legend(override.aes = list(alpha = 0.2))) +
      theme(legend.position="bottom",
            legend.direction="vertical",
            legend.key.height	= unit(1, "cm"),
            legend.title = element_blank())


   return(out_plot)

}