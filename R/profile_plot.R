
#' Function to produce a profile plot for a LDA model object
#' @param data The dataframe to pass in
#' @param model the most fit model produced by the function find_best_fit
#' @param num_var The number of the variables put into the LCA modeling
#' @param f the formula for the LDA modeling
#' @return The ggplot objects
#' @export
#'
#' @examples
#' # Define a formula for the LDA modeling
#' f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)
#' profile_plot(data, num_var, f) # This will yield the plot
#'
#'
#'
#'
#'
#'
profile_plot = function(data, model = NULL, num_var, f){
  f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)
  min_bic <- 1000000
  for(i in 1:7){
    lc <- poLCA(f, data, nclass=i, maxiter=3000,
                tol=1e-5, na.rm=FALSE,
                nrep=10, verbose=TRUE, calc.se=TRUE)
    if(lc$bic < min_bic){
      min_bic <- lc$bic
      LCA_best_model <- lc
    }
  }

  # TO DO add check on if model has been provided

  probs = LCA_best_model$probs
  n_class = length(LCA_best_model$P)

  profile_tb = data.frame(
    tax = replicate(n_class, NA),
    religion = replicate(n_class, NA),
    free_election = replicate(n_class, NA),
    state_aid = replicate(n_class, NA),
    civil_rights = replicate(n_class, NA),
    women = replicate(n_class, NA))

  for (i in 1:num_var) {
    if (length(probs[[i]][1,]) < 10) {
      probs[[i]] = cbind(probs[[i]], matrix(0, nrow = nrow(probs[[i]]), ncol = 10 - ncol(probs[[i]])))
    }
    profile_tb[, i] = probs[[i]] %*% 1:10
  }

  rownames(profile_tb) = paste(rep("class", n_class), seq(1, n_class, 1), sep = "_")
  profile_tb = rownames_to_column(profile_tb)
  colnames(profile_tb)[1] = "class"
  profile_long = reshape::melt(profile_tb, id.vars = "class")

  p = ggplot(profile_long, aes(x = variable, y = value, group = class, color = class)) +
    geom_point(size = 2.25, aes(shape = class))+
    geom_line(size = 1.00) +
    labs(x = NULL, y = "Mean value of the response") +
    theme_bw(base_size = 12)+
    ggtitle(paste(paste("class", 1:length(LCA_best_model$P), sep = "_"),
                  round(LCA_best_model$P, 3), collapse = ", "))+
    theme(plot.margin=unit(c(1.5,1.5,1.5,1.2),"cm"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))+
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  print(p)
  return(p)

  # library(plotly)
  #
  # plotly_p = ggplotly(p, tooltip = "all") %>%
  #   layout(legend = list(orientation = "h", y = 1.2))
  # print(plotly_p)
  # return(plotly_p)
}





#' Find the most fit model by grid search of number of models
#' @param data The dataframe to pass in
#' @param criterion The criterion to choose the best model, default to BIC
#' @param form the formula for the LDA modeling
#' @return The model object of the most fit model according to the criterion
#' @export
#'
#' @examples
#' # Define a formula for the LDA modeling
#' f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)
#' find_best_fit(data, form = f, criterion = 'bic') # This will yield the plot
#'
#'
find_best_fit =function(data, form, criterion = 'bic'){
  #f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)
  min_criterion <- 1000000
  for(i in 1:7){
    lc <- poLCA(form, data, nclass=i, maxiter=3000,
                tol=1e-5, na.rm=FALSE,
                nrep = 10, verbose =TRUE, calc.se=TRUE)
    if(lc[[criterion]] < min_criterion){
      min_criterion <- lc[[criterion]]
      LCA_best_model <- lc
    }
  }

  return(LCA_best_model)
}



#' Visualization of each latent class in a stacked bar plot fashion
#' @param model This parameter expects to be the fitted latent class model object
#' @return The stacked bar plot object; the plot will be printed
#' @export
#' @example
#' # Find out the best model according to the BIC criterion
#' best_model = find_best_fit(data, form, "bic")
#' # Pass the model object into the stacked_bar_plot function
#' stacked_bar_plot(best_model) # The function will print the plot and return the plot object
#'
stacked_bar_plot = function(model) {
  level_multinomial = length(model$probs[[1]][1, ])
  if(!is.null(level_multinomial) & level_multinomial > 9){
    manual_Palette = colorRampPalette(RColorBrewer::brewer.pal(9, "Greys"))(level_multinomial)
  } else {
    manual_Palette = colorRampPalette(RColorBrewer::brewer.pal(level_multinomial, "Greys"))
  }
  g = reshape2::melt(model$probs, level=2) %>%
    ggplot(aes(x = L2, y = value, fill = Var2))+
    geom_bar(stat = "identity", position = "stack")+
    facet_grid(Var1 ~ .) +
    scale_fill_manual(values = manual_Palette)+
    theme_bw() +
    labs(x = "Items",y="Prob. of the multinomial model", fill ="Levels")+
    labs(title = "Stacked barchart of Latent Class model")+
    theme( axis.text.y=element_blank(),
           axis.ticks.y=element_blank(),
           panel.grid.major.y=element_blank())+
    guides(fill = guide_legend(reverse=TRUE))+
    coord_flip()

  print(g)
  return(g)
}

