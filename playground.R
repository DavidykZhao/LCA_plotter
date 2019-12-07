install.packages('poLCA')
library(poLCA)
library(tidyverse)
dta = read.csv("materials/nonzero_dataset.csv")
US = dta %>%
  filter(country == 'United States')
f = with(US, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)



profile_plot = function(data, num_var){
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

profile_plot(US)


best_model = find_best_fit(US, f)


stacked_bar_plot = function(model) {
  reshape2::melt(model, level=2) %>%
    ggplot(lcmodel,aes(x = L1, y = value, fill = Var2))+
    geom_bar(stat = "identity", position = "stack")+
    facet_grid(Var1 ~ .) +
    scale_fill_brewer(type="seq", palette="Greys") +theme_bw() +
    labs(x = "",y="", fill ="")+
    theme( axis.text.y=element_blank(),
           axis.ticks.y=element_blank(),
           panel.grid.major.y=element_blank())+
    guides(fill = guide_legend(reverse=TRUE))
}



