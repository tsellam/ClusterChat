source("Clustine-demo.R", chdir = TRUE)


extract_numbers <- function(str){
  matches <- gregexpr(pattern = '[0-9]+', str)
  as.numeric(unlist(regmatches(str, matches)))
}

##################################
# Static variables and constants #
##################################
MAX_COLS <- 2
SAMPLE_SIZE <- 10

cat("Reads data set.....\n")
file_data <- read.arff("../Data/communities.arff")
names(file_data)[names(file_data) == 'PctPopUnderPov']   <- 'PopUnderPovertyLine'
names(file_data)[names(file_data) == 'PctPersDenseHous'] <- 'PctDenseHousing'
names(file_data)[names(file_data) == 'FemalePctDiv']   <-   'PctFemaleDivorve'

cat("Preprocessing.....\n")
data <- preprocess(file_data)

cat("Initializes data structures.....\n")
active     <- rep(T, nrow(data))
clusters   <- rep(1, nrow(data))
black_list <- character(0)
cluster_infos        <- list()
cluster_descriptions <- list()
initialized          <- FALSE

##############
# Clustering #
##############
clustine_react <- function(input){

  cat("Parsing input", input, "\n")

  out_text <- ""
  plot  <- NULL
  table <- NULL
  reset <- FALSE

  # Parses the input and decides what to to
  go_start <- grepl("go|start|begin|work|yes|sure", input, ignore.case = TRUE)
  go_zoom <- grepl("zoom", input, ignore.case = TRUE)
  go_zoom_in <- grepl("[0-9]+", input, ignore.case = TRUE)
  go_alternative <- grepl("alternative", input, ignore.case = TRUE)
  go_plot <- grepl("see|show", input, ignore.case = TRUE)
  go_plot_plot <- grepl("graph|chart|visualization|plot", input, ignore.case = TRUE)
  go_plot_table <- grepl("table|sample", input, ignore.case = TRUE)
  go_exit <- grepl("done|finish|exit|leave|quit", input, ignore.case = TRUE)
  go_reset <- grepl("reset", input, ignore.case = TRUE)

  go_sum <- go_start + (go_zoom | go_zoom_in) + go_alternative +
            (go_plot | go_plot_plot | go_plot_table) + go_exit + go_reset

  if (go_sum != 1){
    out_text <- "I did not understand your input, could you please rephrase?"

  } else if (go_start & !initialized){
    initialized <<- T
    out_text <- zoom_into_cluster(1)

  } else  if (go_zoom & !go_zoom_in){
    out_text  <- "In which cluster would you like to zoom?"

  } else if (go_zoom_in){
      # Extracts cluster number
      cluster_num <- extract_numbers(input)
      cat("Attempting to zoom in:", cluster_num, "\n")

      if (!all(is.finite(cluster_num)))
        return(list(
          text  = "I did not understand the cluster numbers, please rephrase.",
          plot  = NULL,
          table = NULL
        ))

      # Zooms in
      out_text <- zoom_into_cluster(cluster_num)

  } else if (go_alternative){
    cat("Alternative request detected\n")
    out_text <- get_alternative_description()

  } else if (go_plot | go_plot_plot | go_plot_table){

    if (go_plot_plot) {
      plot <- generate_plot()
      out_text <- "What do you want to do now?"
    } else if (go_plot_table){
      table <- generate_table()
      out_text <- "What do you want to do now?"
    } else {
      out_text <- "Do you want to see a plot or a sample of the table?"
    }

  } else if (go_reset) {
    reset()
    reset <- TRUE

  } else if (go_exit){
    cat("Exit request detected\n")
    stopApp()

  } else {
    out_text <- "I did not understand your input, sorry!"
  }

  return(list(
    text  = out_text,
    plot  = plot,
    table = table,
    reset = reset
  ))

}


generate_table <- function(){
  to_print = file_data[active,, drop=F]

  sample_to_print <- if (nrow(to_print) > SAMPLE_SIZE)
    to_print[sample(1:nrow(to_print), SAMPLE_SIZE, replace = F),]
  else
    to_print

  return(sample_to_print)
}

generate_plot <-function(){

   # Prepares columns
  characteristic_col = extract_char_cols(cluster_descriptions)
  if (length(characteristic_col) > 2) characteristic_col = characteristic_col[1:2]

  # Prepares data
  to_plot = data[active,characteristic_col, drop=F]

  # Prepares labels
  labels <- list_wrap_description(cluster_descriptions)
  to_plot$labels = labels[clusters[active]]

  # Generates chart
  p <- ggplot(to_plot, aes_string(x=characteristic_col[1],
                                y=characteristic_col[2],
                                color = 'labels',
                                fill = 'labels',
                                shape = 'labels')) +
      geom_point() +
      scale_color_discrete('Cluster labels') +
      scale_fill_discrete('Cluster labels') +
      scale_shape('Cluster labels') +
      theme(legend.key.height	= unit(2, "cm"),
            text = element_text(size=14),
            legend.text = element_text(size=13)
            )

  return(p)
}


zoom_into_cluster <- function(clu_num){
  if (!all(clu_num %in% clusters))
    return("Cluster not found<br/>")

  # Selects tuples to zoom in
  active <<- active & (clusters %in% clu_num)
  if (sum(active) < 10)
    return("Can't zoom anymore, the data is too small!<br/>")

  data_select <- data[active,,drop=F]

  # Retsets black list
  black_list <<- c()

  # Clusters tuples
  model <- computeModel(data_select)
  clusters[active] <<- model$classification
  clusters[!active] <<- NA

  # Testing and analysis
  cluster_infos <<- clusters_stats(model)

  # Generates descriptions
  cluster_descriptions <<- describe_clusters(cluster_infos, black_list)
  text_description <- wrap_cluster_description_html(cluster_descriptions)

  # Pretty output:
  out <- paste0('I crunched ',sum(active),' tuples and ')
  out <- paste0(out, 'I found ', length(unique(na.omit(clusters))),' clusters:<br/>')
  out <- paste0(out, text_description)
  out <- paste0(out, 'Do you want to zoom, see the data or get an alternative description?<br/>')

  return(out)
}

get_alternative_description <- function(){
  if (length(cluster_infos)   == 0 |
      length(cluster_descriptions) == 0)
    return("I am an inconsistent state - maybe restart me?")

  # Updates black list
  cur_col_names <- unique(unlist(sapply(cluster_descriptions, names)))
  cat("Added", paste0(cur_col_names), " to black list\n")
  black_list <<- unique(c(black_list, cur_col_names))

  # Reclusters columns
  data_select <- data[active,,drop=F]

  # Generates descriptions
  new_description <- describe_clusters(cluster_infos, black_list)
  if (length(new_description) > 0) cluster_descriptions <<- new_description
  else return("Sorry, I am out of inspiration!")
  text_description <- wrap_cluster_description_html(cluster_descriptions)

  # Pretty output:
  out <- "Ok! Here is an alternative description:"
  out <- paste0(out, text_description)
  out <- paste0(out, 'Do you want to zoom, see the data or get an alternative description?<br/>')

  return(out)
}

reset <- function(){
  active     <<- rep(T, nrow(data))
  clusters   <<- rep(1, nrow(data))
  black_list <<- character(0)
  cluster_infos        <<- list()
  cluster_descriptions <<- list()
  initialized          <<- FALSE
}