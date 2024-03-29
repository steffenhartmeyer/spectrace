#' Plot clustered spectrace data
#'
#' @param lightData Data frame containing the (calibrated) light data. Data needs
#'    to be in wide format (see \code{\link{spectrace_to_wide}}). Must contain a
#'    `cluster_id` column.
#' @param lightData.PCA (Optional) data frame containing the principal components
#'    after PCA of the lightData. Must contain a `cluster_id` column. Defaults to NULL.
#' @param classification (Optional) data frame with classification per cluster.
#'    Must contain a `cluster_id` column, a `spectrum_id`column with the name of the
#'    predicted spectrum, and the spectral data of each predicted spectrum in the same
#'    resolution as `lightData`.
#' @param sil.scores (Optional) data frame with silhouette scores per cluster.
#'    Must contain a `cluster_id` and `sil_score` column. Defaults to NULL.
#' @param samplesize (Optional) Integer, indicating the size of the random sample of spectra
#'    plotted per cluster. Defaults to NULL.
#'
#' @return
#' @export
#'
#' @examples
spectrace_plot_clusters <- function(lightData,
                                    lightData.PCA = NULL,
                                    classification = NULL,
                                    sil.scores = NULL,
                                    samplesize = NULL) {
  if (is.null(samplesize)) {
    samplesize <- Inf
  }

  plot1 <- lightData %>%
    dplyr::mutate(index = 1:nrow(.)) %>%
    dplyr::group_by(cluster_id) %>%
    dplyr::mutate(N = dplyr::n()) %>%
    dplyr::slice_sample(n = samplesize) %>%
    dplyr::ungroup() %>%
    spectrace_normalize_spectra(method = "peak") %>%
    spectrace_to_long() %>%
    dplyr::mutate(cluster_id = as.character(cluster_id)) %>%
    ggplot2::ggplot(ggplot2::aes(x = wl, y = val)) +
    ggplot2::geom_text(
      ggplot2::aes(x = 380, y = 1.02, label = sprintf("N = %s", N)),
      data = function(x) dplyr::slice(dplyr::group_by(x, cluster_id), 1),
      hjust = 0, vjust = 0, size = 2.5,
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = index),
      linewidth = 0.02, color = "grey70"
    ) +
    ggplot2::stat_summary(
      ggplot2::aes(group = 1, color = cluster_id),
      fun = "median", geom = "line", linewidth = 0.6
    ) +
    lemon::facet_rep_wrap(~ sprintf("Cluster %s", cluster_id)) +
    ggplot2::scale_x_continuous(breaks = c(450, 550, 650, 750)) +
    ggplot2::labs(x = "Wavelength (nm)", y = "Relative Power (-)") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "none",
      panel.spacing.x = ggplot2::unit(-1.5, "lines"),
      panel.spacing.y = ggplot2::unit(-1, "lines"),
    )

  if (!is.null(sil.scores)) {
    plot1 <- plot1 +
      ggplot2::geom_text(
        ggplot2::aes(x = 380, y = 1.12, label = sprintf("Sil-score = %s", round(sil_score, 4))),
        data = sil.scores, hjust = 0, vjust = 0, size = 2.5
      )
  }

  if(!is.null(classification)) {
    classification.spectra = classification %>% spectrace_to_long()
    plot1 <- plot1 +
      ggplot2::geom_line(
        ggplot2::aes(group = 1, color = as.character(cluster_id)),
        data = classification.spectra, linetype = 2, linewidth = 0.6,
      ) +
      ggplot2::geom_text(
        ggplot2::aes(x = 380, y = 1.22, label = spectrum_id),
        data = classification, hjust = 0, vjust = 0, size = 2.5,
        fontface='bold'
      )
  }

  plot1 <- plot1 +
    ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1.32))

  if (!is.null(lightData.PCA)) {
    plot.pc1 <- lightData.PCA %>%
      dplyr::mutate(cluster_id = as.character(cluster_id)) %>%
      dplyr::group_by(cluster_id) %>%
      # dplyr::summarise(dplyr::across(dplyr::starts_with("PC"), list(mean, )) %>%
      tidyr::pivot_longer(dplyr::starts_with("PC"), names_to = "PC", values_to = "val") %>%
      ggplot2::ggplot(ggplot2::aes(x = PC, y = val, group = cluster_id)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey70") +
      ggplot2::stat_summary(
        ggplot2::aes(color = cluster_id),
        geom = "line", fun = "mean", linewidth = 0.6
      ) +
      ggplot2::stat_summary(
        ggplot2::aes(fill = cluster_id),
        geom = "ribbon", alpha = 0.1,
        fun.min = ~ mean(.x) - sd(.x), fun.max = ~ mean(.x) + sd(.x),
      ) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = "none",
        legend.margin = ggplot2::margin(-10),
      )

    plot.pc2 <- lightData.PCA %>%
      dplyr::mutate(cluster_id = as.character(cluster_id)) %>%
      dplyr::group_by(cluster_id) %>%
      {
        ggplot2::ggplot(data = ., ggplot2::aes(x = PC1, y = PC2, color = cluster_id, group = cluster_id)) +
          ggplot2::geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey70") +
          ggplot2::geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4, color = "grey70") +
          ggplot2::geom_point(shape = 1, size = 0.01) +
          ggplot2::stat_ellipse(type = "norm", color = "black") +
          ggplot2::scale_x_continuous(limits = c(-max(abs(c(.$PC1, .$PC2))), max(abs(c(.$PC1, .$PC2))))) +
          ggplot2::scale_y_continuous(limits = c(-max(abs(c(.$PC1, .$PC2))), max(abs(c(.$PC1, .$PC2))))) +
          ggplot2::coord_equal() +
          ggplot2::theme_classic() +
          ggplot2::theme(
            legend.title = ggplot2::element_blank(),
            legend.position = "none",
            legend.margin = ggplot2::margin(-10),
          )
      }

    plot <- patchwork::wrap_plots(plot1, plot.pc1, plot.pc2,
                                  design = c(
                                    patchwork::area(1, 1, 2, 2),
                                    patchwork::area(3, 1),
                                    patchwork::area(3, 2)
                                  )
    )
  } else {
    plot <- plot1
  }

  print(plot)
  return(plot)
}
