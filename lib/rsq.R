table_rsq <- function(list_model) {

        df_list <- purrr::map(list_model, function(x) {
                rstan::summary(x)$summary |>
                as.data.frame() |>
                tibble::rownames_to_column(var = "parameters") |>
                dplyr::filter(stringr::str_detect(parameters, "Rsq"))
        })

        df <- do.call(rbind, df_list)

        rsq_table <- knitr::kable(df)

  return(rsq_table)
}

plot_rsq <- function(list_model) {

        df_list <- purrr::map(list_model, function(x) {
          as.data.frame(x) |>
          dplyr::select(dplyr::matches("Rsq"))
        })

        df <- do.call(cbind, df_list)

        rsq_plot <- bayesplot::mcmc_areas(df, area_method = "equal height") +
                    ggplot2::xlim(0, 1)

  return(rsq_plot)
}