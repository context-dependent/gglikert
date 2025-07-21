#' @importFrom ggplot2 ggplot aes geom_col geom_text facet_wrap scale_fill_manual scale_x_continuous scale_y_discrete labs theme
#' @export
plot_likert_table <- function(
  tab, 
  fill_hi = "darkorange",
  fill_lo = "purple",
  fill_neutral = "grey",
  font_family = "inter"
) {
  tab |>
    ggplot(aes(x = p, y = subgroup_label, fill = response_category)) +
    geom_col(position = "stack") +
    geom_text(
      aes(label = scales::percent(p, accuracy = 1)),
      family = font_family,
      colour = "white",
      hjust = "right",
      nudge_x = -0.01,
      data = \(d) {
        d |>
          dplyr::filter(
            as.integer(response_category) == 3L,
            p > 0.03
          )
      }
    ) +
    geom_text(
      aes(x = 1 - p, label = scales::percent(p, accuracy = 1)),
      family = font_family,
      colour = "white",
      hjust = "left",
      nudge_x = 0.01,
      data = \(d) {
        d |>
          dplyr::filter(
            as.integer(response_category) == 1L,
            p >= 0.05
          )
      }
    ) +
    facet_wrap(~item_label, ncol = 1) +
    scale_fill_manual(
      values = c(fill_hi, fill_neutral, fill_lo),
      guide = guide_legend(reverse = TRUE)
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    bptheme::theme_blueprint(
      grid = "xX",
      base_family = font_family,
      base_size = 10,
      strip_text_size = 10,
      plot_background_color = "white",
      axis_col = "black"
    ) +
    scale_y_discrete() +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme(
      axis.text.x = element_blank()
    )
}

#' @export
ragg_dev <- function(...) {
  ragg::agg_png(
    ...,
    res = 300,
    units = "in"
  )
}

#' @export
save_plot <- function(plt, filename, width = 7, height = 4.4, ...) {
  showtext::showtext_opts(dpi = 300)
  showtext::showtext_auto(enable = TRUE)

  ggsave(
    filename,
    plot = plt,
    device = ragg_dev,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    ...
  )
}
