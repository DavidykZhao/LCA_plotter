

#' Function to produce a profile plot for a LDA model object
#' @param data The dataframe to containing the item variables
#' @param model the most fit model produced by the function find_best_fit. It is optional,
#' if not provided, the functio will automatically find the most fit by the criterion of BIC.
#' @param num_var The number of the item variables put into the LCA modeling
#' @param form the formula for the LDA modeling
#' @param maximum_num_class If you input the data to let the function find out the most fit by the
#' criterion of BIC, what is the maximum number of classes you want it in the search space; Optional, default to 7.
#' If you input a model object in the model parameter, this parameter will be nullified. If you want to change the model
#' selecting criterion other than BIC, pleas use find_best_fit function where you could customize the criterion.
#' @param ... The wrapper arguments for the poLCA fitting function in the poLCA package. All the arguments
#' passed within here will be passed into the poLCA function.
#' @return The ggplot objects; you could customize the plot on top of the returned ggplot object
#' @export
#'
#' @examples
#' # Define a formula for the LDA modeling
#' f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)
#' p = profile_plot(data, num_var, f) # This will yield the plot
#' # If you want to further customize the plot, you could add more adjustments on top of
#' the plot object. For example:
#' p + theme(plot.margin=unit(c(1.5,1.5,1.5,1.2),"cm")) +
#'   labs(title = "The title name that you want")
#'
#'
#'
#'
#'
#'
profile_plot = function(data, num_var, form, model = NULL, maximum_num_class = 7, ...){
  require(tidyverse)
  require(poLCA)

  if(is.null(model)){
    #f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)
    min_bic <- 1000000
    for(i in 1:maximum_num_class){
      lc <- poLCA::poLCA(form, data, nclass=i, ...)
      if(lc$bic < min_bic){
        min_bic <- lc$bic
        LCA_best_model <- lc
      }
    }
  } else {
    LCA_best_model = model
  }

  # Compatibility check
  if(num_var != length(best_model$probs)) {
    stop("Please make sure you provide the correct number of item variables at num_var.")
  }
  # Extract the item variable name to construct a df for further plotting
  item_var_name = names(best_model$probs)

  probs = LCA_best_model$probs
  n_class = length(LCA_best_model$P)

  profile_tb = as.data.frame(matrix(NA, nrow =n_class, ncol = length(item_var_name)))
  colnames(profile_tb) = item_var_name
  # profile_tb = data.frame(
  #   tax = replicate(n_class, NA),
  #   religion = replicate(n_class, NA),
  #   free_election = replicate(n_class, NA),
  #   state_aid = replicate(n_class, NA),
  #   civil_rights = replicate(n_class, NA),
  #   women = replicate(n_class, NA))

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

}







#' Find the most fit model by grid search of number of models
#' @param data The dataframe to pass in
#' @param criterion The criterion to choose the best model, default to BIC.
#' Possible options include "aic","Gsq" and "llike"
#' @param form the formula for the LDA modeling
#' @param maximum_num_class If you input the data to let the function find out the most fit by the
#' criterion of BIC, what is the maximum number of classes you want it in the search space; Optional, default to 7.
#' If you input a model object in the model parameter, this parameter will be nullified.
#' @param ... The wrapper arguments for the poLCA fitting function in the poLCA package. All the arguments
#' passed within here will be passed into the poLCA function.
#' @return The model object of the most fit model according to the criterion
#' @export
#'
#' @examples
#' # Define a formula for the LDA modeling
#' f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)
#' # This will yield the best model according to the criterion. The model object could be
#' # passed into further ploting functions
#' best_model = find_best_fit(data, form = f, criterion = 'bic')
#'
#'
find_best_fit =function(data, form, criterion = 'bic', maximum_num_class = 7, ...){
  #f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)
  require(tidyverse)
  require(poLCA)
  min_criterion <- 1000000
  for(i in 1:maximum_num_class){
    lc <- poLCA(form, data, nclass=i, ...)
    if(lc[[criterion]] < min_criterion){
      min_criterion <- lc[[criterion]]
      LCA_best_model <- lc
    }
  }

  return(LCA_best_model)
}




#' Visualization of each latent class in a stacked bar plot fashion (facet by classes)
#' @param model This parameter expects to be the fitted latent class model object
#' @return The stacked bar plot object; the plot will be printed
#' @export
#' @example
#' # Find out the best model according to the BIC criterion
#' best_model = find_best_fit(data, form, "bic")
#' # Pass the model object into the stacked_bar_plot function
#' stacked_bar_plot(best_model) # The function will print the plot and return the plot object
#'
stacked_bar_by_class = function(model) {
  require(tidyverse)
  require(poLCA)
  level_multinomial = length(model$probs[[1]][1, ])
  if(!is.null(level_multinomial) & level_multinomial > 9){
    manual_Palette = colorRampPalette(RColorBrewer::brewer.pal(9, "Greys"))(level_multinomial)
  } else {
    manual_Palette = colorRampPalette(RColorBrewer::brewer.pal(level_multinomial, "Greys"))
  }
  g = reshape2::melt(model$probs) %>%
    ggplot(aes(x = L1, y = value, fill = Var2))+
    geom_bar(stat = "identity", position = "stack")+
    facet_grid(Var1 ~ .) +
    scale_fill_manual(values = manual_Palette)+
    theme_bw() +
    labs(x = "Items",y="Prob. of the multinomial model", fill ="Levels")+
    labs(title = "Stacked barchart of Latent Class model")+
    theme(axis.ticks.y=element_blank(),
           panel.grid.major.y=element_blank())+
    guides(fill = guide_legend(reverse=TRUE))+
    coord_flip()

  print(g)
  return(g)
}


#' Visualization of each latent class in a stacked bar plot fashion (facet by item variables)
#' @param model This parameter expects to be the fitted latent class model object
#' @return The stacked bar plot object; the plot object will be printed and returned
#' @export
#' @example
#' # Find out the best model according to the BIC criterion
#' best_model = find_best_fit(data, form, "bic")
#' # Pass the model object into the stacked_bar_plot function
#' # The function will print the plot and return the plot object
#' p = stacked_bar_plot(best_model)
#' # If you want to further customize the plot, you could add more adjustments on top of
#' the plot object. For example:
#' p + theme(plot.margin=unit(c(1.5,1.5,1.5,1.2),"cm")) +
#'   labs(title = "The title name that you want")
stacked_bar_by_item = function(model) {
  require(tidyverse)
  require(poLCA)
  require(RColorBrewer)

  level_multinomial = length(model$probs[[1]][1, ])
  if(!is.null(level_multinomial) & level_multinomial > 9){
    manual_Palette = colorRampPalette(brewer.pal(9, "Greys"))(level_multinomial)
  } else {
    manual_Palette = colorRampPalette(brewer.pal(level_multinomial, "Greys"))
  }
  g = reshape2::melt(model$probs) %>%
    ggplot(aes(x = Var1, y = value, fill = Var2))+
    geom_bar(stat = "identity", position = "stack")+
    facet_grid(L1 ~ .) +
    scale_fill_manual(values = manual_Palette)+
    theme_bw() +
    labs(x = "Items",y="Prob. of the multinomial model", fill ="Levels")+
    labs(title = "Stacked barchart of Latent Class model")+
    theme(axis.ticks.y=element_blank(),
          panel.grid.major.y=element_blank())+
    guides(fill = guide_legend(reverse=TRUE))+
    coord_flip()

  print(g)
  return(g)
}





