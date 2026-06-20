# Darkening multipliers for lower/upper wall sections.
# Different rendering approaches use slightly different multipliers
# to optimize the visual representation.
SHADE_RAY_SPAN_LOWER <- 0.72
SHADE_RAY_SPAN_UPPER <- 0.86
SHADE_POLYGON_LOWER  <- 0.74
SHADE_POLYGON_UPPER  <- 0.88

#' Ray-segment intersection
#'
#' @keywords internal
intersect_ray_segment <- function(px, py, dx, dy, x1, y1, x2, y2) {
  sx <- x2 - x1
  sy <- y2 - y1
  denominator <- dx * sy - dy * sx

  if (abs(denominator) < 1e-9) {
    return(NULL)
  }

  t <- ((x1 - px) * sy - (y1 - py) * sx) / denominator
  u <- (dx * (y1 - py) - dy * (x1 - px)) / denominator

  if (t >= 0 && u >= 0 && u <= 1) {
    intersection_x <- px + t * dx
    intersection_y <- py + t * dy

    return(list(
      x = intersection_x,
      y = intersection_y,
      dist = sqrt((intersection_x - px)^2 + (intersection_y - py)^2)
    ))
  }

  NULL
}

#' Project world Z to screen Y
#'
#' @keywords internal
project_z <- function(world_z, corrected_distance, player_z, projection_scale, half_screen) {
  half_screen - ((world_z - player_z) * projection_scale / corrected_distance)
}

#' Check if node/subsector index refers to a subsector
#'
#' @keywords internal
is_subsector <- function(index) {
  bitwAnd(index, 0x8000) != 0
}

#' Get clean subsector index
#'
#' @keywords internal
get_subsector_index <- function(index) {
  bitwAnd(index, 0x7FFF)
}

#' Check if point is on front side of BSP node
#'
#' @keywords internal
is_point_on_front_side <- function(px, py, node) {
  ((px - node$x[1]) * node$dy[1] - (py - node$y[1]) * node$dx[1]) <= 0
}

#' Find subsector of player location in BSP
#'
#' @keywords internal
find_player_subsector <- function(node_index, px, py, nodes) {
  node_index <- as.integer(node_index[1])

  if (is_subsector(node_index)) {
    return(get_subsector_index(node_index))
  }

  node <- nodes[node_index + 1, ]
  if (is_point_on_front_side(px, py, node)) {
    find_player_subsector(node$left_child, px, py, nodes)
  } else {
    find_player_subsector(node$right_child, px, py, nodes)
  }
}

#' Get sector index of a subsector
#'
#' @keywords internal
get_subsector_sector <- function(ssector_index, seg_info, ssectors) {
  entry <- ssectors[ssector_index + 1, ]
  first_seg <- entry$first_seg_index + 1
  seg_info$front_sector[first_seg]
}

#' Create wall rendering span
#'
#' @keywords internal
make_wall_span <- function(x, corrected_distance, z_low, z_high,
                           linedef, front_sector, back_sector, part,
                           player_z, projection_scale, half_screen, screen_height) {
  y_top <- project_z(z_high, corrected_distance, player_z, projection_scale, half_screen)
  y_bottom <- project_z(z_low, corrected_distance, player_z, projection_scale, half_screen)

  top <- max(min(min(y_top, y_bottom), screen_height), 0)
  bottom <- max(min(max(y_top, y_bottom), screen_height), 0)

  shade <- max(45, min(235, 245 - corrected_distance / 5))
  if (part == "lower") {
    shade <- shade * SHADE_RAY_SPAN_LOWER
  } else if (part == "upper") {
    shade <- shade * SHADE_RAY_SPAN_UPPER
  }
  }

  data.frame(
    x = x,
    y_top = top,
    y_bottom = bottom,
    corrected_distance = corrected_distance,
    linedef = linedef,
    front_sector = front_sector,
    back_sector = back_sector,
    part = part,
    shade = grDevices::rgb(shade, shade, shade, maxColorValue = 255),
    stringsAsFactors = FALSE
  )
}

#' Create plane span (floor or ceiling)
#'
#' @keywords internal
make_plane_span <- function(x, x_width, near_distance, far_distance, sector_index, plane,
                            sectors, player_z, projection_scale, half_screen, screen_height) {
  if (is.na(sector_index) || far_distance <= near_distance) {
    return(NULL)
  }

  sector <- sectors[sector_index + 1, ]
  world_z <- if (plane == "floor") sector$floor_height else sector$ceiling_height
  y_near <- project_z(world_z, near_distance, player_z, projection_scale, half_screen)
  y_far <- project_z(world_z, far_distance, player_z, projection_scale, half_screen)

  y_min <- max(min(min(y_near, y_far), screen_height), 0)
  y_max <- max(min(max(y_near, y_far), screen_height), 0)
  base <- if (plane == "floor") 42 else 26

  if (abs(y_near - y_far) < 0.5 || y_max - y_min < 0.5) {
    return(NULL)
  }

  shade <- max(20, min(120, base + 120 / (1 + far_distance / 260)))

  data.frame(
    xmin = x - x_width / 2,
    xmax = x + x_width / 2,
    ymin = y_min,
    ymax = y_max,
    sector = sector_index,
    plane = plane,
    shade = grDevices::rgb(shade, shade, shade, maxColorValue = 255),
    stringsAsFactors = FALSE
  )
}

#' Clip span to current visible windows
#'
#' @keywords internal
clip_span_to_windows <- function(y_top, y_bottom, windows) {
  clipped <- data.frame(y_top = numeric(0), y_bottom = numeric(0))

  if (nrow(windows) == 0 || y_bottom - y_top < 0.5) {
    return(clipped)
  }

  for (i in seq_len(nrow(windows))) {
    top <- max(y_top, windows$y_top[i])
    bottom <- min(y_bottom, windows$y_bottom[i])

    if (bottom - top >= 0.5) {
      clipped <- rbind(clipped, data.frame(y_top = top, y_bottom = bottom))
    }
  }

  clipped
}

#' Subtract rendered span from visible windows
#'
#' @keywords internal
subtract_span_from_windows <- function(windows, y_top, y_bottom) {
  remaining <- data.frame(y_top = numeric(0), y_bottom = numeric(0))

  if (nrow(windows) == 0 || y_bottom - y_top < 0.5) {
    return(windows)
  }

  for (i in seq_len(nrow(windows))) {
    win_top <- windows$y_top[i]
    win_bottom <- windows$y_bottom[i]

    if (y_bottom <= win_top || y_top >= win_bottom) {
      remaining <- rbind(remaining, windows[i, ])
    } else {
      if (y_top - win_top >= 0.5) {
        remaining <- rbind(remaining, data.frame(y_top = win_top, y_bottom = y_top))
      }
      if (win_bottom - y_bottom >= 0.5) {
        remaining <- rbind(remaining, data.frame(y_top = y_bottom, y_bottom = win_bottom))
      }
    }
  }

  remaining
}

#' Camera transformation
#'
#' @keywords internal
camera_transform <- function(x, y, player) {
  dx <- x - player$x
  dy <- y - player$y

  list(
    forward = dx * cos(player$angle) + dy * sin(player$angle),
    side = -dx * sin(player$angle) + dy * cos(player$angle)
  )
}

#' Clip wall segment to near plane
#'
#' @keywords internal
clip_to_near_plane <- function(p1, p2, near_clip) {
  if (p1$forward >= near_clip && p2$forward >= near_clip) {
    return(list(p1 = p1, p2 = p2))
  }

  if (p1$forward < near_clip && p2$forward < near_clip) {
    return(NULL)
  }

  t <- (near_clip - p1$forward) / (p2$forward - p1$forward)
  clipped <- list(
    forward = near_clip,
    side = p1$side + t * (p2$side - p1$side)
  )

  if (p1$forward < near_clip) {
    list(p1 = clipped, p2 = p2)
  } else {
    list(p1 = p1, p2 = clipped)
  }
}

#' Project camera point to screen X
#'
#' @keywords internal
screen_x_from_camera <- function(point, screen_width, projection_scale) {
  screen_width / 2 + point$side * projection_scale / point$forward
}

#' Project world Z to screen Y from camera space
#'
#' @keywords internal
screen_y_from_camera <- function(world_z, point, player_z, projection_scale, half_screen) {
  half_screen - ((world_z - player_z) * projection_scale / point$forward)
}

#' Make 3D wall polygon
#'
#' @keywords internal
make_wall_polygon <- function(wall, z_low, z_high, part, group_id,
                              player, projection_scale, screen_width, screen_height, half_screen, near_clip = 8) {
  p1 <- camera_transform(wall$x1, wall$y1, player)
  p2 <- camera_transform(wall$x2, wall$y2, player)
  clipped <- clip_to_near_plane(p1, p2, near_clip = near_clip)

  if (is.null(clipped)) {
    return(NULL)
  }

  p1 <- clipped$p1
  p2 <- clipped$p2
  sx1 <- screen_x_from_camera(p1, screen_width, projection_scale)
  sx2 <- screen_x_from_camera(p2, screen_width, projection_scale)

  if (max(sx1, sx2) < 0 || min(sx1, sx2) > screen_width) {
    return(NULL)
  }

  y1_top <- screen_y_from_camera(z_high, p1, player$z, projection_scale, half_screen)
  y1_bottom <- screen_y_from_camera(z_low, p1, player$z, projection_scale, half_screen)
  y2_top <- screen_y_from_camera(z_high, p2, player$z, projection_scale, half_screen)
  y2_bottom <- screen_y_from_camera(z_low, p2, player$z, projection_scale, half_screen)

  if (max(y1_top, y1_bottom, y2_top, y2_bottom) < 0 ||
      min(y1_top, y1_bottom, y2_top, y2_bottom) > screen_height) {
    return(NULL)
  }

  avg_dist <- (p1$forward + p2$forward) / 2
  shade <- max(40, min(230, 245 - avg_dist / 5))
  if (part == "lower") {
    shade <- shade * SHADE_POLYGON_LOWER
  } else if (part == "upper") {
    shade <- shade * SHADE_POLYGON_UPPER
  }
  }

  data.frame(
    group = group_id,
    x = c(sx1, sx2, sx2, sx1),
    y = c(y1_top, y2_top, y2_bottom, y1_bottom),
    avg_dist = avg_dist,
    part = part,
    shade = grDevices::rgb(shade, shade, shade, maxColorValue = 255),
    stringsAsFactors = FALSE
  )
}

#' Draw subsector segs (debug helper)
#'
#' @keywords internal
draw_ssector <- function(ssector_index, walls, seg_info, ssectors, verbose = FALSE) {
  entry <- ssectors[ssector_index + 1, ]
  num_segs <- entry$num_segs
  start_idx <- entry$first_seg_index

  for (i in 0:(num_segs - 1)) {
    seg_index <- start_idx + i
    wall <- walls[seg_index + 1, ]
    seg <- seg_info[seg_index + 1, ]

    if (verbose) {
      cat(sprintf(
        "Drawing SEG %d (linedef %d, front sector %d): (%d,%d) -> (%d,%d)\n",
        seg_index,
        seg$linedef,
        seg$front_sector,
        wall$x1,
        wall$y1,
        wall$x2,
        wall$y2
      ))
    }
  }
}

#' Traverse BSP and return drawing order of subsectors
#'
#' @keywords internal
traverse_bsp <- function(node_index, px, py, walls, seg_info, nodes, ssectors,
                          verbose = FALSE) {
  node_index <- as.integer(node_index[1])

  if (is_subsector(node_index)) {
    ssector_index <- get_subsector_index(node_index)

    if (ssector_index >= 0 && ssector_index < nrow(ssectors)) {
      if (verbose) {
        cat(sprintf("Reached SSECTOR %d\n", ssector_index))
      }
      draw_ssector(ssector_index, walls, seg_info, ssectors, verbose)
      return(ssector_index)
    } else {
      warning(sprintf("Invalid SSECTOR index: %d\n", ssector_index))
    }
    return(integer(0))
  }

  if (node_index < 0 || node_index >= nrow(nodes)) {
    warning(sprintf("Invalid NODE index: %d\n", node_index))
    return(integer(0))
  }

  node <- nodes[node_index + 1, ]

  if (is_point_on_front_side(px, py, node)) {
    draw_order <- c(
      traverse_bsp(node$left_child, px, py, walls, seg_info, nodes, ssectors, verbose),
      traverse_bsp(node$right_child, px, py, walls, seg_info, nodes, ssectors, verbose)
    )
  } else {
    draw_order <- c(
      traverse_bsp(node$right_child, px, py, walls, seg_info, nodes, ssectors, verbose),
      traverse_bsp(node$left_child, px, py, walls, seg_info, nodes, ssectors, verbose)
    )
  }

  draw_order
}
