#' Plots noised attribute
#' @param data list of data.frames with noise
#' @param variable chr attribute name to plot
#' @param n int number of points to plot
#' @importFrom dplyr %>%
#' @return plot of noise
plot_noise <- function(data, variable, n = 100) {
  data <- purrr::imap(data["noises"], ~ {
    purrr::imap(.x, ~ {
      .x %>%
        dplyr::select(variable) %>%
        setNames(paste0(as.numeric(.y) * 100, "%"))
    })
  }) %>%
    purrr::flatten() %>%
    dplyr::bind_cols() %>%
    dplyr::slice_head(n = n)

  plot <- data %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Szum") %>%
    dplyr::group_by(Szum) %>%
    dplyr::mutate(order = 1:dplyr::n()) %>%
    ggplot2::ggplot(ggplot2::aes(x = order, y = value, color = Szum)) +
    ggplot2::geom_line(ggplot2::aes()) +
    ggplot2::ggtitle(paste0("Szum dla atrybutu ", variable)) +
    ggplot2::xlab("") +
    ggplot2::ylab("Wartosc atrybutu") +
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Generates html report
#' @param data list of 4; preprocessed, noises, train, test
#' @param models data.frame; output models
#' @param to_plot chr; a vector of attributes names' to plot
#' @param output_dir chr; path to save the report
#' @export
generate_report <- function(data, models, to_plot = c("aa_000", "ba_003", "cn_005"), output_dir = getwd()) {
  cat("Generating report.\n")

  suppressMessages(suppressWarnings(rmarkdown::render(system.file("report.Rmd", package = "scania"),
    output_file = "report",
    output_format = "html_document",
    output_dir = output_dir,
    quiet = TRUE
  )))

  cat("\nRaport has been generated without errors.\nYou can find it in: \"", output_dir, "\"\n", sep = "")
}
