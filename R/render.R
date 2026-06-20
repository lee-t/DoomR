utils::globalVariables(c(".data", "x1", "y1", "x2", "y2", "dx", "dy", "x", "y", "group", "shade"))

#' Render Doom Map
#'
#' Parses a Doom WAD file and renders maps in 3D using BSP traversal, ray casting,
#' and polygon projection.
#'
#' @param wad_path Path to the DOOM WAD file. Default is "DOOM.WAD".
#' @param map_name Name of the map to render. Default is "E1M1".
#' @param player A list containing player position and angle (x, y, z, angle).
#' @param fov Field of view in radians. Default is `pi / 2`.
#' @param n_rays Number of rays to cast for ray rendering.
#' @param screen_width Rendered screen width.
#' @param screen_height Rendered screen height.
#' @param far_clip Far clipping distance in map units. Default is 2400.
#' @param verbose Logical; if TRUE, prints progress.
#' @return A list of plots and parsed data.
#' @export
doom_render <- function(wad_path = "DOOM.WAD",
                        map_name = "E1M1",
                        player = list(
                          x = 1024,
                          y = -3264,
                          z = 41,
                          angle = atan2(-96, -96)
                        ),
                        fov = pi / 2,
                        n_rays = 640,
                        screen_width = 640,
                        screen_height = 480,
                        far_clip = 2400,
                        verbose = FALSE) {

  if (!file.exists(wad_path)) {
    stop("Could not find DOOM.WAD at: ", wad_path)
  }

  # Epsilon threshold for filtering near-zero render distances
  min_render_distance <- 1e-6

  wad <- read_lump_directory(wad_path)
  lump_table <- wad$lumps

  map_idx <- which(lump_table$name == map_name)
  if (length(map_idx) != 1) {
    stop("Could not find map lump ", map_name)
  }

  map_data <- lump_table[(map_idx + 1):(map_idx + 10), ]
  names_by_lump <- stats::setNames(seq_len(nrow(map_data)), map_data$name)

  linedefs <- read_linedefs(wad_path, map_data[names_by_lump["LINEDEFS"], ])
  sidedefs <- read_sidedefs(wad_path, map_data[names_by_lump["SIDEDEFS"], ])
  vertices <- read_vertexes(wad_path, map_data[names_by_lump["VERTEXES"], ])
  segs <- read_segs(wad_path, map_data[names_by_lump["SEGS"], ])
  ssectors <- read_ssectors(wad_path, map_data[names_by_lump["SSECTORS"], ])
  nodes <- read_nodes(wad_path, map_data[names_by_lump["NODES"], ])
  sectors <- read_sectors(wad_path, map_data[names_by_lump["SECTORS"], ])

  if (verbose) {
    cat(
      "Loaded",
      nrow(vertices), "vertices,",
      nrow(linedefs), "linedefs,",
      nrow(segs), "segs,",
      nrow(ssectors), "subsectors,",
      nrow(nodes), "nodes, and",
      nrow(sectors), "sectors.\n"
    )
  }

  no_sidedef <- 65535
  get_sector_for_sidedef <- function(side_index) {
    if (is.na(side_index) || side_index == no_sidedef) {
      return(NA_integer_)
    }
    sidedefs$sector[side_index + 1]
  }

  front_sidedef <- integer(nrow(segs))
  back_sidedef <- integer(nrow(segs))
  front_sector <- integer(nrow(segs))
  back_sector <- integer(nrow(segs))

  for (i in seq_len(nrow(segs))) {
    linedef <- linedefs[segs$linedef[i] + 1, ]

    if (segs$direction[i] == 0) {
      front_sidedef[i] <- linedef$right_sidedef
      back_sidedef[i] <- linedef$left_sidedef
    } else {
      front_sidedef[i] <- linedef$left_sidedef
      back_sidedef[i] <- linedef$right_sidedef
    }

    front_sector[i] <- get_sector_for_sidedef(front_sidedef[i])
    back_sector[i] <- get_sector_for_sidedef(back_sidedef[i])
  }

  seg_info <- cbind(
    segs,
    front_sidedef = front_sidedef,
    back_sidedef = back_sidedef,
    front_sector = front_sector,
    back_sector = back_sector
  )

  seg_info$front_floor <- sectors$floor_height[seg_info$front_sector + 1]
  seg_info$front_ceiling <- sectors$ceiling_height[seg_info$front_sector + 1]
  seg_info$back_floor <- ifelse(
    is.na(seg_info$back_sector),
    NA,
    sectors$floor_height[seg_info$back_sector + 1]
  )
  seg_info$back_ceiling <- ifelse(
    is.na(seg_info$back_sector),
    NA,
    sectors$ceiling_height[seg_info$back_sector + 1]
  )

  walls <- data.frame(
    x1 = vertices$x[seg_info$v1 + 1],
    y1 = vertices$y[seg_info$v1 + 1],
    x2 = vertices$x[seg_info$v2 + 1],
    y2 = vertices$y[seg_info$v2 + 1],
    linedef = seg_info$linedef,
    direction = seg_info$direction,
    front_sector = seg_info$front_sector,
    back_sector = seg_info$back_sector,
    front_floor = seg_info$front_floor,
    front_ceiling = seg_info$front_ceiling,
    back_floor = seg_info$back_floor,
    back_ceiling = seg_info$back_ceiling
  )

  if (verbose) {
    cat("Constructed", nrow(walls), "wall segments with sector heights.\n")
  }

  map_plot <- ggplot2::ggplot(walls) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$x1, y = .data$y1, xend = .data$x2, yend = .data$y2)) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()

  angles <- seq(
    player$angle - fov / 2,
    player$angle + fov / 2,
    length.out = n_rays
  )

  rays <- data.frame(
    dx = cos(angles),
    dy = sin(angles)
  )

  ray_plot <- ggplot2::ggplot(rays) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = player$x,
        y = player$y,
        xend = player$x + 50 * .data$dx,
        yend = player$y + 50 * .data$dy
      ),
      color = "blue"
    ) +
    ggplot2::annotate("point", x = player$x, y = player$y, color = "red", size = 3) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal()

  half_screen <- screen_height / 2
  projection_scale <- (screen_width / 2) / tan(fov / 2)

  root_node <- nrow(nodes) - 1
  player_subsector <- find_player_subsector(root_node, player$x, player$y, nodes)
  player_sector <- get_subsector_sector(player_subsector, seg_info, ssectors)

  if (verbose) {
    cat("Player starts in subsector", player_subsector, "sector", player_sector, "\n")
  }

  slice_rows <- list()
  plane_rows <- list()
  column_width <- screen_width / n_rays

  for (i in seq_len(n_rays)) {
    ray_dx <- rays$dx[i]
    ray_dy <- rays$dy[i]
    ray_hits <- data.frame(
      wall_index = integer(0),
      dist = numeric(0),
      corrected_distance = numeric(0)
    )

    for (j in seq_len(nrow(walls))) {
      wall <- walls[j, ]
      res <- intersect_ray_segment(
        px = player$x,
        py = player$y,
        dx = ray_dx,
        dy = ray_dy,
        x1 = wall$x1,
        y1 = wall$y1,
        x2 = wall$x2,
        y2 = wall$y2
      )

      if (!is.null(res)) {
        corrected_distance <- res$dist * cos(angles[i] - player$angle)

        if (corrected_distance > min_render_distance) {
          ray_hits <- rbind(ray_hits, data.frame(
            wall_index = j,
            dist = res$dist,
            corrected_distance = corrected_distance
          ))
        }
      }
    }

    if (nrow(ray_hits) > 0) {
      ray_hits <- ray_hits[order(ray_hits$corrected_distance), ]
      x_p <- ((i - 0.5) / n_rays) * screen_width
      visible_windows <- data.frame(y_top = 0, y_bottom = screen_height)
      current_sector <- player_sector
      interval_start <- 1

      for (hit_idx in seq_len(nrow(ray_hits))) {
        if (nrow(visible_windows) == 0) {
          break
        }

        hit <- ray_hits[hit_idx, ]
        wall <- walls[hit$wall_index, ]
        spans <- list()
        interval_end <- hit$corrected_distance

        plane_rows[[length(plane_rows) + 1]] <- make_plane_span(
          x_p,
          column_width,
          interval_start,
          interval_end,
          current_sector,
          "floor",
          sectors, player$z, projection_scale, half_screen, screen_height
        )
        plane_rows[[length(plane_rows) + 1]] <- make_plane_span(
          x_p,
          column_width,
          interval_start,
          interval_end,
          current_sector,
          "ceiling",
          sectors, player$z, projection_scale, half_screen, screen_height
        )

        if (is.na(wall$back_sector)) {
          spans[[length(spans) + 1]] <- make_wall_span(
            x_p,
            hit$corrected_distance,
            wall$front_floor,
            wall$front_ceiling,
            wall$linedef,
            wall$front_sector,
            wall$back_sector,
            "solid",
            player$z, projection_scale, half_screen, screen_height
          )
        } else {
          next_sector <- if (!is.na(current_sector) && current_sector == wall$front_sector) {
            wall$back_sector
          } else if (!is.na(current_sector) && current_sector == wall$back_sector) {
            wall$front_sector
          } else {
            wall$back_sector
          }

          if (!isTRUE(all.equal(wall$front_floor, wall$back_floor))) {
            spans[[length(spans) + 1]] <- make_wall_span(
              x_p,
              hit$corrected_distance,
              min(wall$front_floor, wall$back_floor),
              max(wall$front_floor, wall$back_floor),
              wall$linedef,
              wall$front_sector,
              wall$back_sector,
              "lower",
              player$z, projection_scale, half_screen, screen_height
            )
          }

          if (!isTRUE(all.equal(wall$front_ceiling, wall$back_ceiling))) {
            spans[[length(spans) + 1]] <- make_wall_span(
              x_p,
              hit$corrected_distance,
              min(wall$front_ceiling, wall$back_ceiling),
              max(wall$front_ceiling, wall$back_ceiling),
              wall$linedef,
              wall$front_sector,
              wall$back_sector,
              "upper",
              player$z, projection_scale, half_screen, screen_height
            )
          }

          current_sector <- next_sector
          interval_start <- interval_end
        }

        for (span in spans) {
          if (is.null(span)) {
            next
          }

          clipped_spans <- clip_span_to_windows(span$y_top, span$y_bottom, visible_windows)
          if (nrow(clipped_spans) == 0) {
            next
          }

          for (clip_idx in seq_len(nrow(clipped_spans))) {
            clipped_span <- span
            clipped_span$y_top <- clipped_spans$y_top[clip_idx]
            clipped_span$y_bottom <- clipped_spans$y_bottom[clip_idx]
            slice_rows[[length(slice_rows) + 1]] <- clipped_span
          }

          visible_windows <- subtract_span_from_windows(
            visible_windows,
            span$y_top,
            span$y_bottom
          )
        }

        if (is.na(wall$back_sector)) {
          break
        }
      }

      if (nrow(visible_windows) > 0 && !is.na(current_sector)) {
        plane_rows[[length(plane_rows) + 1]] <- make_plane_span(
          x_p,
          column_width,
          interval_start,
          far_clip,
          current_sector,
          "floor",
          sectors, player$z, projection_scale, half_screen, screen_height
        )
        plane_rows[[length(plane_rows) + 1]] <- make_plane_span(
          x_p,
          column_width,
          interval_start,
          far_clip,
          current_sector,
          "ceiling",
          sectors, player$z, projection_scale, half_screen, screen_height
        )
      }
    }
  }

  slice_data <- do.call(rbind, slice_rows)
  plane_data <- do.call(rbind, plane_rows)

  polygon_rows <- list()
  group_id <- 1

  for (i in seq_len(nrow(walls))) {
    wall <- walls[i, ]
    wall_parts <- list()

    if (is.na(wall$back_sector)) {
      wall_parts[[length(wall_parts) + 1]] <- list(
        z_low = wall$front_floor,
        z_high = wall$front_ceiling,
        part = "solid"
      )
    } else {
      if (!isTRUE(all.equal(wall$front_floor, wall$back_floor))) {
        wall_parts[[length(wall_parts) + 1]] <- list(
          z_low = min(wall$front_floor, wall$back_floor),
          z_high = max(wall$front_floor, wall$back_floor),
          part = "lower"
        )
      }

      if (!isTRUE(all.equal(wall$front_ceiling, wall$back_ceiling))) {
        wall_parts[[length(wall_parts) + 1]] <- list(
          z_low = min(wall$front_ceiling, wall$back_ceiling),
          z_high = max(wall$front_ceiling, wall$back_ceiling),
          part = "upper"
        )
      }
    }

    for (wall_part in wall_parts) {
      polygon <- make_wall_polygon(
        wall,
        wall_part$z_low,
        wall_part$z_high,
        wall_part$part,
        group_id,
        player,
        projection_scale,
        screen_width,
        screen_height,
        half_screen
      )

      if (!is.null(polygon)) {
        polygon_rows[[length(polygon_rows) + 1]] <- polygon
        group_id <- group_id + 1
      }
    }
  }

  wall_polygons <- do.call(rbind, polygon_rows)
  wall_polygons <- wall_polygons[order(wall_polygons$avg_dist, decreasing = TRUE), ]

  render_plot <- ggplot2::ggplot(wall_polygons) +
    ggplot2::geom_polygon(
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$group,
        fill = .data$shade
      ),
      colour = "#111111",
      linewidth = 0.08
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(
      ratio = 1,
      xlim = c(0, screen_width),
      ylim = c(screen_height, 0),
      expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "black", colour = NA),
      plot.background = ggplot2::element_rect(fill = "black", colour = NA),
      legend.position = "none"
    )

  if (verbose) {
    cat("Traversing BSP from root node", root_node, "\n")
  }
  bsp_draw_order <- traverse_bsp(
    root_node,
    player$x,
    player$y,
    walls,
    seg_info,
    nodes,
    ssectors,
    verbose = verbose
  )
  if (verbose) {
    cat("BSP traversal visited", length(bsp_draw_order), "subsectors.\n")
  }

  list(
    map_plot = map_plot,
    ray_plot = ray_plot,
    render_plot = render_plot,
    player_sector = player_sector,
    player_subsector = player_subsector,
    bsp_draw_order = bsp_draw_order,
    slice_data = slice_data,
    plane_data = plane_data,
    wall_polygons = wall_polygons
  )
}
