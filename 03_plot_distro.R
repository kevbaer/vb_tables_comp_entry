plot_distro_full <- function(full) {
  # this splits up the encoding into a player and position portion
  a <- str_split(full, "99")[[1]]

  player <- a[1]
  pos <- a[2]

  # get the position distribution
  pos_distribution <- total |>
    filter(position == pos)

  pos_distribution |>
    ggplot() +
    aes(x = PPA_Total) +
    stat_halfeye(
      aes(
        fill = after_stat(
          x < (dat |> filter(player_name == player) |> pull(PPA_Total)) # grab the value we want
        ),
      ),
      slab_linewidth = 1,
      slab_color = "black",
      slab_linetype = "solid",
      interval_size_range = c(.5, 2.5)
    ) +
    theme_void(base_size = 16, base_family = "Barlow") + #basically barren
    theme(
      legend.position = "none", # no legend
      axis.text.y = element_blank(), # no axes
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank() # no grids
    ) +
    scale_fill_manual(values = c("TRUE" = "#940034", "FALSE" = "#cdfbff")) + #fill colors
    ggview::canvas(width = 14, height = 6) #set sizing
}
