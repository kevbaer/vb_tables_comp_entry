make_gt <- function(
  ..df,
  remove_middle_headers = FALSE,
  title = "Volleyball Analysis - Table Competition Entry 2025"
) {
  ..df <- ..df |>
    mutate(
      row_id = str_glue(
        "{dplyr::row_number()}."
      ),
      .before = 1,
      .by = position
    ) |>
    mutate(
      #this encodes the player_name and position to be turned into the distribution plot
      DIST = str_c(player_name, "99", position)
    )
  # We mutated some new columns

  # We have to know which positions we are dealing with to ensure
  # different begin, end, middle headers/footers etc. since we'll combine
  # at end
  list_of_pos <- ..df |> distinct(position) |> arrange(position) |> pull()

  begin <- list_of_pos[1]
  end <- list_of_pos[length(list_of_pos)]
  middle <- base::setdiff(list_of_pos, c(begin, end))

  # Here we make a function which creates the table we want for the current pos
  # At the end, we'll run this for each of the positions in list_of_pos
  current_pos <- function(.df, curr_pos) {
    curr_pos <- curr_pos$position
    .df <- .df |>
      select(-conference) |>
      relocate(
        row_id,
        player_name,
        team,
        Team_Logo,
        contains("Serve"),
        contains("Reception"),
        contains("Attack"),
        contains("Block"),
        contains("Set"),
        contains("Dig")
      ) |>
      gt(groupname_col = "position") |>
      gtExtras::gt_theme_espn() |>
      gtExtras::gt_color_rows(
        c(
          PPA_Block,
          PPA_Serve,
          PPA_Reception,
          PPA_Attack,
          PPA_Dig,
          PPA_Set,
          PPA_Total
        ),
        palette = "ggsci::blue_material",
        na.color = "#e8e8e8"
      ) |>
      tab_spanner(
        label = "Serve",
        columns = c(n_Serve, PPA_Serve, PPA_Serve_percentile)
      ) |>
      tab_spanner(
        label = "Receive",
        columns = c(n_Reception, PPA_Reception, PPA_Reception_percentile)
      ) |>
      tab_spanner(
        label = "Attack",
        columns = c(n_Attack, PPA_Attack, PPA_Attack_percentile)
      ) |>
      tab_spanner(
        label = "Block",
        columns = c(n_Block, PPA_Block, PPA_Block_percentile)
      ) |>
      tab_spanner(
        label = "Dig",
        columns = c(n_Dig, PPA_Dig, PPA_Dig_percentile)
      ) |>
      tab_spanner(
        label = "Set",
        columns = c(n_Set, PPA_Set, PPA_Set_percentile)
      ) |>
      cols_align(align = "center") |>
      cols_label(
        player_name = "Player",
        team = "Team",
        PPA_Serve = "PPA",
        PPA_Reception = "PPA",
        PPA_Set = "PPA",
        PPA_Attack = "PPA",
        PPA_Dig = "PPA",
        PPA_Block = "PPA",
        PPA_Total = "Weighted PPA",
        n_Serve = "Serves",
        n_Reception = "Receives",
        n_Attack = "Attacks",
        n_Set = "Sets",
        n_Dig = "Digs",
        n_Block = "Blocks",
        ends_with("percentile") ~ "%"
      ) |>
      gtExtras::gt_hulk_col_numeric(
        #this provides the Green Purple
        PPA_Dig_percentile,
        domain = c(0, 100),
        trim = TRUE
      ) |>
      gtExtras::gt_hulk_col_numeric(
        PPA_Total_percentile,
        domain = c(0, 100)
      ) |>
      gtExtras::gt_add_divider(
        c(
          PPA_Serve_percentile,
          PPA_Reception_percentile,
          PPA_Attack_percentile,
          PPA_Block_percentile,
          PPA_Dig_percentile,
          PPA_Set_percentile,
          Team_Logo
        ),
        weight = "5px",
        color = "black"
      ) |>
      tab_row_group(
        label = str_glue("{curr_pos |> str_replace('_',' ')}"),
        rows = everything()
      ) |>
      tab_style(
        style = list(
          cell_fill("black"),
          cell_text(color = "white", weight = "bold")
        ),
        locations = cells_row_groups()
      ) |>
      sub_missing()

    if (curr_pos %in% begin) {
      # this adds the header to the top entry
      .df <- .df |>
        tab_header(
          title = str_glue("{title}"),
          subtitle = str_glue(
            "2025 NCAA MVB Season | Made by Kevin Baer"
          )
        )
    }

    if (curr_pos != "libero") {
      .df <- .df |>
        gtExtras::gt_hulk_col_numeric(
          PPA_Serve_percentile,
          domain = c(0, 100),
          trim = TRUE
        )
    }

    if (curr_pos %in% c("libero", "outside")) {
      .df <- .df |>
        gtExtras::gt_hulk_col_numeric(
          PPA_Reception_percentile,
          domain = c(0, 100),
          trim = TRUE
        )
    }
    if (!curr_pos %in% c("libero", "service_sub")) {
      .df <- .df |>
        gtExtras::gt_hulk_col_numeric(
          PPA_Attack_percentile,
          domain = c(0, 100),
          trim = TRUE
        ) |>
        gtExtras::gt_hulk_col_numeric(
          PPA_Block_percentile,
          domain = c(0, 100),
          trim = TRUE
        )
    }

    if (curr_pos %in% c("libero", "setter")) {
      .df <- .df |>
        gtExtras::gt_hulk_col_numeric(
          PPA_Set_percentile,
          domain = c(0, 100),
          trim = TRUE
        )
    }

    if (curr_pos %in% c(middle, end) & !(curr_pos %in% begin)) {
      # this removes the header
      .df <- .df |>
        rm_stubhead() |>
        tab_options(table.border.top.width = 0, row_group.border.top.width = 0)
      if (remove_middle_headers) {
        .df <- .df |> tab_options(column_labels.hidden = TRUE)
      }
    }

    if (curr_pos %in% c(begin, middle)) {
      .df <- .df |> tab_options(table.border.bottom.width = 0)
    }

    if (curr_pos %in% end) {
      #this adds the footer
      .df <- .df |>
        tab_footnote(
          locations = cells_column_labels(c(n_Serve)),
          footnote = "Minimum of 25 rep in each weighted category for inclusion"
        ) |>
        tab_footnote(
          locations = cells_column_labels(c(PPA_Serve)),
          footnote = "Point Percentage Added: a stat that measures impact on winning a point per touch"
        ) |>
        tab_footnote(
          locations = cells_column_labels(c(PPA_Total)),
          footnote = "Weighted by Position: e.g. Outsides are 19% Serve, 25% Receive, 36% Attack, 12% Block, 10% Dig"
        )
    }
    .df |>
      gtExtras::gt_merge_stack(
        # this makes the team name appear below player name
        player_name,
        team,
        small_cap = FALSE,
        palette = c("black", "grey20"),
        font_size = c("16px", "11px")
      ) |>
      gtExtras::gt_img_rows(Team_Logo) |> # this makes the logos images
      cols_label(
        Team_Logo = "Team",
        row_id = "#"
      ) |>
      cols_width(
        row_id ~ 35,
        player_name ~ 180,
        Team_Logo ~ 80,
        PPA_Serve ~ 60,
        PPA_Reception ~ 60,
        starts_with("n_") ~ 70,
        PPA_Attack ~ 60,
        PPA_Block ~ 60,
        PPA_Set ~ 60,
        PPA_Dig ~ 60,
        PPA_Total ~ 90,
        ends_with("percentile") ~ 40
      ) |>
      text_transform(
        # this transforms the distribution encoding into the actual plot
        locations = cells_body(DIST),
        fn = function(x) {
          bar_fx <- function(y) {
            # this function is modified from Tom Mock's gtExtras code
            plot_out <- plot_distro_full(y)

            out_name <- file.path(tempfile(
              pattern = "file",
              tmpdir = tempdir(),
              fileext = ".svg"
            ))

            ggview::save_ggplot(
              file = out_name,
              plot = plot_out,
              dpi = 72,
              width = 100,
              height = 30,
              units = "px"
            )

            img_plot <- readLines(out_name) %>%
              paste0(collapse = "") %>%
              gt::html()

            on.exit(file.remove(out_name), add = TRUE)

            img_plot
          }
          tab_built <- mapply(bar_fx, x)
          tab_built
        }
      ) |>
      tab_style(cell_fill("white"), cells_body("DIST")) |>
      cols_label(
        DIST = "Distribution"
      ) |>
      tab_style(
        cell_fill(color = "gold1"),
        cells_column_labels(c(PPA_Total, PPA_Total_percentile, DIST))
      ) |>
      tab_style(
        style = cell_borders(
          sides = c("right"),
          weight = px(5),
          color = "black"
        ),
        locations = cells_column_spanners(
          c("Serve", "Receive", "Attack", "Block", "Set", "Dig")
        )
      ) |>
      gtExtras::gt_add_divider(
        DIST,
        weight = "5px",
        color = "black"
      ) |>
      fmt_number(
        # this formats the numbers in PPA_ columns
        columns = starts_with("PPA_"),
        decimals = 3,
        use_seps = FALSE
      ) |>
      fmt_number(
        # this formats the numbers in percentile columns
        columns = ends_with("percentile"),
        decimals = 0,
        use_seps = FALSE
      )
  }

  suppressWarnings(group_map(
    # this maps through each position and runs current_pos
    ..df |> group_by(position),
    current_pos
  ))
}
