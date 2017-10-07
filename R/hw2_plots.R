# convert a unix-timestamp back to a date
unix_to_date <- function(x) {
  as_datetime(as.POSIXct(x, origin="1970-01-01"))
}

#' identity function
identity <- function(x) {
  x
}

#' Mean Squared Error
mse <- function(y_true, y_pred) {
  mean((y_true - y_pred)^2)
}

#' Root Mean Squared Error
rmse <- function(y_true, y_pred) {
  sqrt(mse(y_true, y_pred))
}

#' Look-up appropriate metric function.
which_metric <- function(metric_str) {
  if (metric_str == 'MSE') {
    return(mse)
  }
  else if (metric_str == 'RMSE') {
    return(rmse)
  } else {
    stop('Unrecognized metric name.')
  }
}

#' Plot a performance over time
#'
#' This is a useful diagnostic plot for monitoring the predictions of
#' a model over time. Plots the predictions of a list of models as
#' a function of data as well as the actual values.
#'
#' @param data data.frame with co-variates and the predictor
#' @param date_var string name of the date variable
#' @param ... A list of models where predictions are stored
#' @param convert.timestamps Boolean indicating whether unix times stamps
#'   need to be converted back to dates.
#' @param y_transform function used to transform the predictions if
#'   fit on a different scale.
#' @param metric string indicating which metric to use when comparing models.
#' @return a ggplot2 graph
performance_curve <- function(data, target_var, date_var, ...,
                              y_transform = identity,
                              convert.timestamps = TRUE,
                              metric = 'RMSE') {
  # add predictions of the models stored in ...
  perf <- data %>%
    gather_predictions(...) %>%
    mutate(pred = y_transform(pred))

  # calculate the metric per model
  model_metric <- function(data, model_level) {
    perf_model <- filter(perf, model == model_level)
    metric_func <- which_metric(metric)
    metric_func(perf_model[[target_var]], perf_model$pred)
  }

  # determine the label for the models (model_name (metric_name: value))
  model_factor = c()
  model_levels <- unique(perf$model)
  for (i in 1:length(model_levels)) {
    model_level <- model_levels[i]
    rmse <- round(model_metric(perf, model_level), 2)
    model_label <- paste(model_level, " (", metric, " = ", rmse, ")")
    model_factor[model_level] = model_label
  }

  # add actual predictions as well
  perf <- perf %>%
    mutate(model = plyr::revalue(model, model_factor))

  # add the actual values (hack is to add it to the model indicator)
  data <- data %>%
    mutate(model = rep("actual", n())) %>%
    mutate_(pred = target_var)

  # bind model preds and actual together for ggplot
  perf <- bind_rows(perf, data)

  # this is another hack to always color the actual predictions black
  model_names <-sort(setdiff(unique(perf$model), "actual"))
  values <- hue_pal()(length(model_names))
  names(values) <- model_names

  # hack this to display test/train error (rmse)
  values = c(values, c(actual = "black"))

  # unix-timestamp to datetime
  if (convert.timestamps) {
    perf[[date_var]] <- unix_to_date(perf[[date_var]])
  }

  # time to actual get to plotting
  perf_plot <- ggplot(data = perf, aes_(x = as.name(date_var))) +
    geom_line(aes(y = pred, color = model)) +
    scale_x_datetime(date_labels = '%Y-%m', date_breaks = '3 month') +
    scale_color_manual(values=values)

  perf_plot
}

#' Plots of training/testing curves over time.
#'
#' See `performance_curve` for more details.
train_test_curves <- function(train, test, target_var, date_var, ...,
                              y_transform = identity,
                              convert.timestamps = TRUE,
                              metric = 'RMSE') {

  # training curve
  train_plot <- performance_curve(train, target_var, date_var, ...,
                                  y_transform = y_transform,
                                  convert.timestamps = convert.timestamps,
                                  metric = metric)

  # testing curve
  test_plot <- performance_curve(test, target_var, date_var, ...,
                                 y_transform = y_transform,
                                 convert.timestamps = convert.timestamps,
                                 metric = metric)

  # stack the two plots vertically
  cowplot::plot_grid(test_plot, train_plot, ncol = 1, align = 'V')
}

#' This is a helper function that generates a dataframe with one
#' column being the variable names and the other column being the
#' number of times that variable was selected for a subset.
subset_stats <- function(subset_fit) {
  stats <- as.tibble(summary(subset_fit)$which) %>%
    gather(key = variable_name, value = selected) %>%
    filter(variable_name != '(Intercept)') %>%
    mutate(selected = as.numeric(selected)) %>%
    group_by(variable_name) %>%
    summarize(n_selected = sum(selected))

  stats
}

#' A waffleplot of best subsets
#'
#' The best subset selection algorithm attempts to find the best combination
#' of variables for a requested subset size, i.e. the best three variable
#' combination for a linear model using three preditors. This plot visualizes
#' the chosen subsets for each number of variables as a stacked waffleplot.
#' Each column of the plot indicates a subset, and each row indicates a
#' variable. To determine the variables included in a subset, one can read off
#' the shaded columns for that row.
#'
#' @param subset_fit the output from a call to leaps::regsubsets
#' @returns a ggplot waffleplot of the subsets.
subset_waffleplot <- function(subset_fit) {
  # This is essentially a transposed version of what we want
  # to visualize. Each row corresponds to a subset and
  # each column is a variable. An each is a boolean indicating
  # whether the variable is in the subset or not.
  selection_data <- as.tibble(summary(subset_fit)$which)

  # the visualization is easier to read if we order the variables
  # by how many times they are included in a subset.
  stats <- subset_stats(subset_fit)
  selection_order <- stats$variable_name[order(stats$n_selected)]

  # We are using geom_tile for the visualization. So we need a integer
  # grid of size (n_rows x n_cols) for the entries and a factor indicating
  # whether to shade the squares.
  n_rows <- nrow(selection_data)
  n_cols <- ncol(selection_data)
  grid_data <- selection_data %>%
    gather(key = variable_name, value = included) %>%
    mutate(step_number = rep(1:n_rows, n_cols)) %>%
    # we factor out the intercept since it is always included
    filter(variable_name != '(Intercept)') %>%
    mutate(variable_name = factor(variable_name, levels = selection_order))

  ggplot(grid_data, aes(x = step_number, y = variable_name, fill = included)) +
    geom_tile(color = 'black', size = 0.5) +
    xlab('Number of Variables') +
    theme(panel.border = element_rect(size = 2),
      plot.title = element_text(size = rel(1.2)),
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.text.y = element_text(size=7),
      legend.position = "right")
}