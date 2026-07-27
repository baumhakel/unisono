#' Add Angular Geometry for Polar Plots
#'
#' @description
#' Calculates angular bounds for lag vectors in a 2D spatial field, properly
#' handling the boundary seam (2-pi wrapping) to draw continuous wedge polygons.
#'
#' @param data A data frame containing lag vectors and group information.
#'
#' @return A data frame expanded with \code{xmin}, \code{xmax}, and \code{drawlab}
#'   columns defining the geometric bounds for polar rendering.
#'
#' @keywords internal
#' @noRd
add_angular_geometry <- function(data) {
  data |>
    dplyr::mutate(angle = atan2(.data$lags[, 2], .data$lags[, 1]) %% (2 * pi)) |>
    dplyr::arrange(.data$set, .data$angle) |>
    dplyr::group_by(.data$set) |>
    dplyr::mutate(
      next_angle = dplyr::lead(.data$angle, default = dplyr::first(.data$angle) + 2 * pi),
      prev_angle = dplyr::lag(.data$angle, default = dplyr::last(.data$angle) - 2 * pi),
      a_min = (11 * .data$angle + 9 * .data$prev_angle) / 20,
      a_max = (11 * .data$angle + 9 * .data$next_angle) / 20
    ) |>
    dplyr::ungroup() |>
    # Generate discrete boundary geometries, breaking polygons at the 2-pi seam
    dplyr::mutate(
      geom = purrr::map2(.data$a_min, .data$a_max, function(mi, ma) {
        if (mi < 0) {
          data.frame(xmin = c(mi + 2 * pi, 0), xmax = c(2 * pi, ma), drawlab = c(TRUE, FALSE))
        } else if (ma > 2 * pi) {
          data.frame(xmin = c(mi, 0), xmax = c(2 * pi, ma - 2 * pi), drawlab = c(FALSE, TRUE))
        } else {
          data.frame(xmin = mi, xmax = ma, drawlab = TRUE)
        }
      })
    ) |>
    tidyr::unnest("geom")
}

#' Add Indexed Geometry for Linear Plots
#'
#' @description
#' Assigns sequential positional coordinates and fixed-width rectangular boundaries
#' to rank-ordered lags for standard Cartesian rendering.
#'
#' @param data A data frame containing lag vector length information.
#' @param width Numeric scalar. The width of the geometric bounding box. Default is 0.6.
#'
#' @return A data frame expanded with \code{xpos}, \code{xmin}, \code{xmax}, and
#'   \code{drawlab} columns.
#'
#' @keywords internal
#' @noRd
add_indexed_geometry <- function(data, width = 0.6) {
  data |>
    dplyr::group_by(.data$set) |>
    dplyr::arrange(length, .by_group = TRUE) |>
    # Compute sequential position and symmetric boundaries
    dplyr::mutate(
      xpos = dplyr::row_number(),
      xmin = .data$xpos - width / 2,
      xmax = .data$xpos + width / 2,
      drawlab = TRUE
    ) |>
    dplyr::ungroup()
}

#' Compute Summary Information for Plot Facets
#'
#' @description
#' Extracts group-level summary statistics to generate facet labels and expected
#' proportion reference lines.
#'
#' @param data A data frame with plot geometries and group assignments.
#'
#' @return A summarized data frame containing facet labels and expected null values.
#'
#' @keywords internal
#' @noRd
compute_facet_info <- function(data) {
  info <- data |>
    dplyr::group_by(.data$set) |>
    dplyr::summarise(expected = 1 / sum(.data$drawlab), len = dplyr::first(.data$length), .groups = "drop")

  info$label <- paste0(info$set, ": length ", round(info$len, 2))
  info
}

#' Generate Color and Fill Scales
#'
#' @description
#' Provides default Viridis scales or applies user-specified color mapping.
#'
#' @param cols Optional vector of colors. If \code{NULL}, defaults to the "mako" palette.
#'
#' @return A list containing \code{ggplot2} fill and color scale objects.
#'
#' @keywords internal
#' @noRd
get_fill_col_scales <- function(cols) {
  if (is.null(cols)) {
    list(
      fill = ggplot2::scale_fill_viridis_d(option = "mako", end = 0.7, guide = "none"),
      col  = ggplot2::scale_color_viridis_d(option = "mako", end = 0.7, guide = "none")
    )
  } else {
    list(
      fill = ggplot2::scale_fill_manual(values = cols, guide = "none"),
      col  = ggplot2::scale_color_manual(values = cols, guide = "none")
    )
  }
}

#' Construct Strip Text Theme Element
#'
#' @description
#' Toggles the display of facet strip headers.
#'
#' @param setnames Logical. Whether to render facet set names.
#'
#' @return A \code{ggplot2} theme element.
#'
#' @keywords internal
#' @noRd
strip_style <- function(setnames) {
  if (setnames) ggplot2::element_text(face = "bold") else ggplot2::element_blank()
}

#' Append Lollipop Base Layers to Plot
#'
#' @description
#' Adds the primary geometric layers (confidence interval rectangles, reference lines,
#' and points) to the underlying plot object. Designed to adapt flexibly to either
#' linear or angular coordinate systems via string mapping.
#'
#' @param p A \code{ggplot2} object.
#' @param info A data frame containing facet summary information.
#' @param x Character. Name of the primary positional axis variable.
#' @param xmin Character. Name of the minimum bounding variable.
#' @param xmax Character. Name of the maximum bounding variable.
#'
#' @return A modified \code{ggplot2} object.
#'
#' @keywords internal
#' @noRd
add_lollipop_layers <- function(p, info, x, xmin, xmax) {
  p +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data[[xmin]], xmax = .data[[xmax]],
                   ymin = .data$ci_lower, ymax = .data$ci_upper, fill = .data$set),
      alpha = 0.3
    ) +
    ggplot2::geom_hline(data = info, ggplot2::aes(yintercept = .data$expected),
                        color = "firebrick", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = .data[[x]], xend = .data[[x]], y = 0, yend = .data$prop),
                          color = "grey30", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(x = .data[[x]], y = .data$prop, color = .data$set), size = 3) +
    ggplot2::geom_point(ggplot2::aes(x = .data[[x]], y = .data$prop),
                        size = 3, stroke = 1, shape = 21, color = "black") +
    ggrepel::geom_text_repel(data = info, ggplot2::aes(x = pi/12, y = .data$expected, label = paste0("1/", round(1/.data$expected))), color = "firebrick", vjust = -1, hjust = 0)
}

#' Append Non-Overlapping Labels to Plot
#'
#' @description
#' Attaches descriptive text labels to specific coordinates, utilizing a repulsive
#' algorithm to prevent occlusion.
#'
#' @param p A \code{ggplot2} object.
#' @param plot_data A data frame containing plot geometries.
#' @param x Character. Name of the primary positional axis variable.
#'
#' @return A modified \code{ggplot2} object.
#'
#' @keywords internal
#' @noRd
add_repel_labels <- function(p, plot_data, x) {
  p + ggrepel::geom_text_repel(
    data = dplyr::filter(plot_data, .data$drawlab),
    ggplot2::aes(x = .data[[x]], y = .data$prop, label = .data$label),
    size = 3, color = "black", box.padding = 0.5, show.legend = FALSE
  )
}

#' Generate Rose Plot for Isotropy Results
#'
#' @description
#' Constructs a polar coordinate plot ("rose plot") to visualize directional dependencies
#' in 2-dimensional spatial lags.
#'
#' @param uni.res An object of class \code{unifIsoResult}.
#' @param alpha Numeric. Significance level for confidence bounds.
#' @param drawlabs Logical. If \code{TRUE}, adds text labels to the plot.
#' @param cols Optional color palette.
#' @param setnames Logical. If \code{TRUE}, prints facet labels.
#'
#' @return A \code{ggplot} object.
#'
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' # Placeholder for rose plot usage
#' }
# rename to rose(..)
unifIsoResult_roseplot <- function(uni.res, alpha, drawlabs = TRUE, cols = NULL, setnames = TRUE) {
  plot_data <- compute_ci_table(uni.res, alpha) |> add_angular_geometry()
  info <- compute_facet_info(plot_data)
  sc <- get_fill_col_scales(cols)

  p <- ggplot2::ggplot(plot_data) |> add_lollipop_layers(info, x = "angle", xmin = "xmin", xmax = "xmax")
  if (drawlabs) p <- add_repel_labels(p, plot_data, x = "angle")

  p + sc$fill + sc$col +
    ggplot2::scale_x_continuous(limits = c(0, 2 * pi), expand = c(0, 0),
                                breaks = seq(0, 2 * pi - 0.001, by = pi / 4)) +
    ggplot2::scale_y_continuous(breaks = seq(0, max(plot_data$prop) + 0.1, by = 0.05), expand = c(0, 0)) +
    ggplot2::coord_polar(start = -pi / 2, direction = -1) +
    ggplot2::facet_wrap(~set, scales = "free_y", labeller = ggplot2::as_labeller(stats::setNames(info$label, info$set))) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey80"),
      panel.grid.major.y = ggplot2::element_line(color = "grey80"),
      strip.text = strip_style(setnames)
    ) +
    ggplot2::labs(x = NULL, y = NULL)
}

#' Generate Lollipop Plot for Isotropy Results
#'
#' @description
#' Constructs a standard Cartesian lollipop plot to visualize dependencies across
#' lags when spatial dimensions exceed 2 or when angular rendering is disabled.
#'
#' @param uni.res An object of class \code{unifIsoResult}.
#' @param alpha Numeric. Significance level for confidence bounds.
#' @param drawlabs Logical. If \code{TRUE}, adds text labels to the plot.
#' @param cols Optional color palette.
#' @param setnames Logical. If \code{TRUE}, prints facet labels.
#' @param width Numeric. Width of the confidence bounds rects.
#'
#' @return A \code{ggplot} object.
#'
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' # Placeholder for lollipop plot usage
#' }
# rename to lollipop(..)
unifIsoResult_lollipop <- function(uni.res, alpha, drawlabs = TRUE, cols = NULL, setnames = TRUE, width = 0.6) {
  plot_data <- compute_ci_table(uni.res, alpha) |> add_indexed_geometry(width = width)
  info <- compute_facet_info(plot_data)
  sc <- get_fill_col_scales(cols)

  p <- ggplot2::ggplot(plot_data) |> add_lollipop_layers(info, x = "xpos", xmin = "xmin", xmax = "xmax")
  if (drawlabs) p <- add_repel_labels(p, plot_data, x = "xpos")

  p + sc$fill + sc$col +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::facet_wrap(~set, scales = "free", labeller = ggplot2::as_labeller(stats::setNames(info$label, info$set))) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey80"),
      strip.text = strip_style(setnames)
    ) +
    ggplot2::labs(x = NULL, y = NULL)
}
