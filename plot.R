
rvs$data_by_condition |>
    filter(signal == input$selected_signal_plot) |>
    filter(condition %in% input$selected_condition_plot) |>
    drop_na(condition) -> plot_data

rvs$data_by_condition |>
    filter(signal == input$selected_signal_plot) |>
    filter(condition %in% input$selected_condition_plot) |>
    drop_na(condition) |>
    group_by(condition, Time) |>
    summarise(sd = sd(value), mean = mean(value)) -> summarized_plot_data

plot_metrics_comparison <- function(df, condition,  metric, size = 5,  color = "#e8871a") {
    df |>
        ggplot() +
        geom_point(aes(y = condition, x = metric), size = size, color = color) +
        theme_minimal() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }

plot_metric_visualization <- function(df, x, metrics_to_visualize, group, wrap = NULL) {
    df |>
        ggplot(aes(x = {{x}}, group = {{group}})) -> p


    for (metric in metrics_to_visualize) {


        if (metric %in% plot_points_metrics) {
            p <- p + geom_point(aes(y = .data[[metric]]), alpha = .4, color = "#7570b3")
            }

        if (metric %in% plot_hlines_metrics) {
            p <- p + geom_hline(aes(yintercept = .data[[metric]]), linetype = "dashed", color = "#1B9E77")
            }

        if (metric %in% plot_vlines_metrics) {
            p <- p + geom_vline(aes(xintercept = .data[[metric]]), linetype = "dashed", color = "#1B9E77")
            }

        # if (metric %in% plot_slopes_metrics) {
        #     p <- p + geom_line(aes(y = linear(x, metric, intercept)), linetype = "dashed", color = "#1B9E77")
        #     }

        if (metric %in% plot_lines_metrics) {
             p <- p + geom_line(aes(y = .data[[metric]]), color = "black")
             }

        }

    # p <- p +
    #     scale_x_continuous(expand = c(0, 0)) +
    #     scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    #     theme_minimal()

    if (!is.null(wrap)) {
        p <- p + facet_wrap(~well)
    }

    return(p)
}

plot_metric_comparison <- function(df, x, metrics_to_visualize, color, fill, group) {

    df |>
        ggplot(aes(x = x, color = color, fill = fill)) -> p

    for (metric in metrics_to_visualize) {

        if (metric %in% plot_points_metrics) {
            p <- p + geom_point(aes(y = metric, group = group), size = 2, alpha = .4)
            }

        if (metric %in% plot_ribbon_metrics) {
            p + geom_ribbon(data = summarized_plot_data, aes(ymin = mean - sd, ymax = mean + sd), alpha = .2)
        }
        if (metric %in% plot_errorbars_metrics) {
            p <- p + geom_errorbar(data = summarized_plot_data, aes(ymin = mean - sd, ymax = mean + sd), alpha = .4)
        }

        if (metric %in% plot_lines_metrics) {
            linetype <- switch(metric,
                               "fit" = "solid",
                               "mean" = "dashed")
            p <- p + geom_line(data = summarized_plot_data, aes(y = metric), size = 1, linetype = linetype)
        }
    }

    p <- p +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
        theme_minimal()

    return(p)
}




