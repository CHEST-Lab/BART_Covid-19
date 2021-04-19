pd_plotGGPLOT <- function (bart_machine, j, levs = c(0.05, seq(from = 0.1, to = 0.9, 
                                              by = 0.1), 0.95), lower_ci = 0.025, upper_ci = 0.975, prop_data = 1) 
{
  #check_serialization(bart_machine)
  if (class(j) == "integer") {
    j = as.numeric(j)
  }
  if (class(j) == "numeric" && (j < 1 || j > bart_machine$p)) {
    stop(paste("You must set j to a number between 1 and p =", 
               bart_machine$p))
  }
  else if (class(j) == "character" && !(j %in% bart_machine$training_data_features)) {
    stop("j must be the name of one of the training features (see \"<bart_model>$training_data_features\")")
  }
  else if (!(class(j) == "numeric" || class(j) == "character")) {
    stop("j must be a column number or column name")
  }
  x_j = bart_machine$model_matrix_training_data[, j]
  if (length(unique(na.omit(x_j))) <= 1) {
    warning("There must be more than one unique value in this training feature. PD plot not generated.")
    return()
  }
  x_j_quants = unique(quantile(x_j, levs, na.rm = TRUE))
  if (length(unique(x_j_quants)) <= 1) {
    warning("There must be more than one unique value among the quantiles selected. PD plot not generated.")
    return()
  }
  n_pd_plot = round(bart_machine$n * prop_data)
  bart_predictions_by_quantile = array(NA, c(length(x_j_quants), 
                                             n_pd_plot, bart_machine$num_iterations_after_burn_in))
  for (q in 1:length(x_j_quants)) {
    indices = sample(1:bart_machine$n, n_pd_plot)
    test_data = bart_machine$X[indices, ]
    test_data[, j] = rep(x_j_quants[q], n_pd_plot)
    bart_predictions_by_quantile[q, , ] = bart_machine_get_posterior(bart_machine, 
                                                                     test_data)$y_hat_posterior_samples
    cat(".")
  }
  cat("\n")
  if (bart_machine$pred_type == "classification") {
    bart_predictions_by_quantile = qnorm(bart_predictions_by_quantile)
  }
  bart_avg_predictions_by_quantile_by_gibbs = array(NA, c(length(x_j_quants), 
                                                          bart_machine$num_iterations_after_burn_in))
  for (q in 1:length(x_j_quants)) {
    for (g in 1:bart_machine$num_iterations_after_burn_in) {
      bart_avg_predictions_by_quantile_by_gibbs[q, g] = mean(bart_predictions_by_quantile[q, 
                                                                                          , g])
    }
  }
  bart_avg_predictions_by_quantile = apply(bart_avg_predictions_by_quantile_by_gibbs, 
                                           1, mean)
  bart_avg_predictions_lower = apply(bart_avg_predictions_by_quantile_by_gibbs, 
                                     1, quantile, probs = lower_ci)
  bart_avg_predictions_upper = apply(bart_avg_predictions_by_quantile_by_gibbs, 
                                     1, quantile, probs = upper_ci)
  var_name = ifelse(class(j) == "character", j, bart_machine$training_data_features[j])
  ylab_name = ifelse(bart_machine$pred_type == "classification", 
                     "Partial Effect (Probits)", "Partial Effect")
  
  # GGPLOT
  gg_output <- ggplot() +
    geom_polygon(aes(c(x_j_quants, 
                       rev(x_j_quants)), 
                     c(bart_avg_predictions_upper, 
                       rev(bart_avg_predictions_lower))),
                 fill = "gray87") + 
    geom_line(aes(x_j_quants, bart_avg_predictions_lower), col = "black", linetype = "dashed") +
    #geom_point(aes(x_j_quants, bart_avg_predictions_lower), col = "black", shape = 21) +
    geom_line(aes(x_j_quants, bart_avg_predictions_upper), col = "black", linetype = "dashed") +
    #geom_point(aes(x_j_quants, bart_avg_predictions_upper), col = "black", shape = 21) +
    geom_line(aes(x_j_quants, bart_avg_predictions_by_quantile), lwd = 0.5) + 
    geom_point(aes(x_j_quants, bart_avg_predictions_by_quantile), size = 0.8, shape = 21, fill = "black") +
    coord_cartesian(ylim = c(min(bart_avg_predictions_lower, 
                                 bart_avg_predictions_upper), 
                             max(bart_avg_predictions_lower, 
                                 bart_avg_predictions_upper))) +
    ggthemes::theme_few() +
    scale_y_continuous(position = "right") +
    labs(x = paste(var_name, "plotted at specified quantiles"),
         y = ylab_name)
  
  return(gg_output)
}