
#' Get metadata of phyloseq object
#'
#' `meta()` gets the sample data associated with a phyloseq and converts it to 
#' a tibble.
#'
#' @param ps phyloseq - phyloseq object from which sample data will be taken.
#'
#' @return tibble - tibble containing sample data of phyloseq
meta <- function(ps) {
  require(tidyverse)

  if (class(ps) != "phyloseq") stop("ps is not an object of class 'phyloseq'")
  else ps %>% sample_data %>% as_tibble
}

#' Plot Alpha Diversity boxplots based on a measure.
#' 
#' Generates a publication quality boxplot for alpha diversity based on a
#' phyloseq object.
#' 
#' @param data A tibble containing the sample data of a phyloseq object
#' containing a column with measure alpha diversity.
#' @param group A string containing the group by which the x-axis is plotted.
#' @param measure A type of alpha diversity.
#' - measures = c("Observed", Chao1", "Shannon", "Fisher")
#' @param palette list - a list of hex codes representing color palettes for each
#' boxplot to be generated.
#'
#' @return A ggplot2 object, which can be passed to ggarrange.
plot_alpha_diet <- function(data, group, measure, palette) {
  require(tidyverse)
  require(ggpubr)

  range <- range(data[[measure]])
  ggplot(
    data = data,
    aes(
      x = as.factor(data[[group]]),
      y = data[[measure]],
      fill = as.factor(data[[group]])
      )
    ) +
    geom_point(position = position_jitter(width = 0.1)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank()
    ) +
    stat_compare_means(
      method = "wilcox.test",
      paired = TRUE,
      label = "p.format",
      label.y = range[1] - diff(range) * .1,
      label.x = 1.5
    ) +
    scale_fill_manual(values = palette) +
    labs(title = str_glue("{measure} Alpha Diversity"),
         x = str_glue("{group}"),
         y = str_glue("{measure} Alpha Diversity")
    )
  }

#' Make Publication Quality Alpha Diversity Plots
#'
#' `aggregate_plot_alpha_diet()` takes the output of `plot_alpha_diet()` and agreggates
#' the plots from mulitple different measures into 1 publication quality figure.
#' Why two separate functions? It doesn't work otherwise.
#'
#' @param data tibble or data.frame - dataframe containing phyloseq data
#' @param group string - string containing the column in meta(ps) to group by.
#' @param measures vector - list of measures, as columns found in meta(ps) to plot.
#' @param palette vector - list of colors for grouping variables.
#' @param dim vector - number of columns x number of rows for plots.
#'
#' @return ggarranged ggplot of alpha diversity.
#'
#' @examples
#'    group <- "Diagnosis"
#'    measures <- c("Observed", "Chao1", "Shannon", "Fisher")
#'    palette <- get_palette(c("#E69F00", "#56B4E9", "#009E73", "#F0E442"), 4)
#'    
#'    aggregate_plot_alpha(group, measures, palette) %>%
#'      plot %T>%
#'      ggsave(
#'        filename = str_glue("{directory}/output/figures/Alpha Diversity by {group}.tiff"),
#'        plot = .,
#'        width = 8,
#'        height = 8,
#'        bg = 'white'
#'      )
aggregate_plot_alpha_diet <- function(data, group, measures, palette, name, dim = c(2, 2)) {
  if (missing(name)) name = group
  
  ggarrange(
    plotlist = measures %>% map(
      plot_alpha_diet,
      data = data,
      group = group,
      palette = palette
    ),
    ncol = dim[1],
    nrow = dim[2]
  ) %>%
    annotate_figure(
      top = text_grob(
        str_glue("Alpha Diversity Metrics by {name}"),
        face = "bold",
        size = 14
      )
    )
}

#' Plot Alpha Diversity boxplots based on a measure.
#' 
#' Generates a publication quality boxplot for alpha diversity based on a
#' phyloseq object.
#' 
#' @param data A tibble containing the sample data of a phyloseq object
#' containing a column with measure alpha diversity.
#' @param group A string containing the group by which the x-axis is plotted.
#' @param measure A type of alpha diversity.
#' - measures = c("Observed", Chao1", "Shannon", "Fisher")
#' @param palette list - a list of hex codes representing color palettes for each
#' boxplot to be generated.
#'
#' @return A ggplot2 object, which can be passed to ggarrange.
plot_alpha_social <- function(data, group, measure, palette) {
  require(tidyverse)
  require(ggpubr)
  require(rstatix)

  range <- range(data[[measure]])
  ggplot(
    data = data,
    aes(
      x = as.factor(data[[group]]),
      y = data[[measure]],
      fill = as.factor(data[[group]])
      ),
    environment = environment()
    ) +
    geom_point(position = position_jitter(width = 0.1)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank()
    ) +
    stat_friedman_test(
      aes(wid = Name),
      parse = TRUE,
      label = "Friedman~Test:~Chi^2~({df})~==~{statistic},~p~==~{rstatix::p_format(p, accuracy = 0.0001, digits = 3, leading.zero = FALSE)}",
      label.y = range[1] - diff(range) * .1,
      label.x = 1.5,
    ) +
    scale_fill_manual(values = palette) +
    labs(title = str_glue("{measure} Alpha Diversity"),
         x = str_glue("{group}"),
         y = str_glue("{measure} Alpha Diversity")
    )
  }

#' Make Publication Quality Alpha Diversity Plots
#'
#' `aggregate_plot_alpha_social()` takes the output of `plot_alpha_social()` and agreggates
#' the plots from mulitple different measures into 1 publication quality figure.
#' Why two separate functions? It doesn't work otherwise.
#'
#' @param data tibble or data.frame - dataframe containing phyloseq data
#' @param group string - string containing the column in meta(ps) to group by.
#' @param measures vector - list of measures, as columns found in meta(ps) to plot.
#' @param palette vector - list of colors for grouping variables.
#' @param dim vector - number of columns x number of rows for plots.
#'
#' @return ggarranged ggplot of alpha diversity.
#'
#' @examples
#'    group <- "Diagnosis"
#'    measures <- c("Observed", "Chao1", "Shannon", "Fisher")
#'    palette <- get_palette(c("#E69F00", "#56B4E9", "#009E73", "#F0E442"), 4)
#'    
#'    aggregate_plot_alpha(group, measures, palette) %>%
#'      plot %T>%
#'      ggsave(
#'        filename = str_glue("{directory}/output/figures/Alpha Diversity by {group}.tiff"),
#'        plot = .,
#'        width = 8,
#'        height = 8,
#'        bg = 'white'
#'      )
aggregate_plot_alpha_social <- function(data, group, measures, palette, name, dim = c(2, 2)) {
  if (missing(name)) name = group
  
  ggarrange(
    plotlist = measures %>% map(
      plot_alpha_social,
      data = data,
      group = group,
      palette = palette
    ),
    ncol = dim[1],
    nrow = dim[2]
  ) %>%
    annotate_figure(
      top = text_grob(
        str_glue("Alpha Diversity Metrics by {name}"),
        face = "bold",
        size = 14
      )
    )
}

#' Plot Relative Abundance across a grouping variable
#'
#' @param name: String or List - Name of the sample (under sample var "Name")
#' @param palette List - Color palette to use for graph. Number of colors must
#' equal number of taxa.
#'
#' @return ggplot object - graph of relative abundance.
plot_bar_graph <- function(group, palette) {
  ggplot(
    data = filter(melt, Name %in% group), 
    aes(x = as.factor(Timepoint), y = Abundance, fill = Family)
  ) + 
    geom_bar(aes(), stat = "identity", position = "stack") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank()
    ) +
    labs(title = str_glue("{group}")) +
    scale_fill_manual(
      limits = unique(melt$Family),
      values = c(palette, "#000000")
    )
}

plot_bar_graph_bacteria <- function(melt, palette) {
  ggplot(
    data = melt, 
    aes(x = Timepoint, y = Abundance, fill = Family)
  ) +
    geom_bar(stat = "identity", position = "fill", ) +
    scale_y_continuous(labels = scales::number_format(scale = 1)) +
    scale_fill_manual(
      limits = unique(melt$Family),
      values = palette,
    ) + 
    theme_minimal() +
    theme(axis.title = element_blank())
}