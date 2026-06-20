#' Read an 8-byte name from WAD file connection
#'
#' @param con A file connection
#' @return A character string
#' @keywords internal
read_wad_name <- function(con) {
  bytes <- readBin(con, what = raw(), n = 8)
  rawToChar(bytes[bytes != as.raw(0)], multiple = FALSE)
}

#' Read WAD Lump Directory
#'
#' @param wad_path Path to the DOOM WAD file.
#' @return A list containing `magic` and a data frame `lumps`.
#' @export
read_lump_directory <- function(wad_path) {
  con <- file(wad_path, "rb")
  on.exit(close(con))

  magic <- readChar(con, nchars = 4, useBytes = TRUE)
  num_lumps <- readBin(con, integer(), size = 4, endian = "little")
  dir_offset <- readBin(con, integer(), size = 4, endian = "little")

  seek(con, where = dir_offset, origin = "start")

  lump_table <- data.frame(
    offset = integer(num_lumps),
    size = integer(num_lumps),
    name = character(num_lumps),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(num_lumps)) {
    lump_table$offset[i] <- readBin(con, integer(), size = 4, endian = "little")
    lump_table$size[i] <- readBin(con, integer(), size = 4, endian = "little")
    lump_table$name[i] <- read_wad_name(con)
  }

  list(magic = magic, lumps = lump_table)
}

#' Read Vertexes Lump
#'
#' @param wad_path Path to the DOOM WAD file.
#' @param lump_info Data frame containing lump offset and size.
#' @return A data frame of vertexes with x and y coordinates.
#' @export
read_vertexes <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_vertices <- lump_info$size %/% 4
  raw_ints <- readBin(
    con,
    what = integer(),
    n = n_vertices * 2,
    endian = "little",
    signed = TRUE,
    size = 2
  )

  verts <- matrix(raw_ints, ncol = 2, byrow = TRUE)
  colnames(verts) <- c("x", "y")
  as.data.frame(verts)
}

#' Read Linedefs Lump
#'
#' @param wad_path Path to the DOOM WAD file.
#' @param lump_info Data frame containing lump offset and size.
#' @return A data frame of linedefs.
#' @export
read_linedefs <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_linedefs <- lump_info$size %/% 14
  raw_ints <- readBin(
    con,
    what = integer(),
    n = n_linedefs * 7,
    size = 2,
    endian = "little",
    signed = FALSE
  )

  linedefs <- matrix(raw_ints, ncol = 7, byrow = TRUE)
  colnames(linedefs) <- c(
    "v1", "v2", "flags", "special_type", "sector_tag",
    "right_sidedef", "left_sidedef"
  )
  as.data.frame(linedefs)
}

#' Read Sidedefs Lump
#'
#' @param wad_path Path to the DOOM WAD file.
#' @param lump_info Data frame containing lump offset and size.
#' @return A data frame of sidedefs.
#' @export
read_sidedefs <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_sidedefs <- lump_info$size %/% 30
  sidedefs <- data.frame(
    x_offset = integer(n_sidedefs),
    y_offset = integer(n_sidedefs),
    upper_texture = character(n_sidedefs),
    lower_texture = character(n_sidedefs),
    middle_texture = character(n_sidedefs),
    sector = integer(n_sidedefs),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_sidedefs)) {
    sidedefs$x_offset[i] <- readBin(con, integer(), size = 2, signed = TRUE, endian = "little")
    sidedefs$y_offset[i] <- readBin(con, integer(), size = 2, signed = TRUE, endian = "little")
    sidedefs$upper_texture[i] <- read_wad_name(con)
    sidedefs$lower_texture[i] <- read_wad_name(con)
    sidedefs$middle_texture[i] <- read_wad_name(con)
    sidedefs$sector[i] <- readBin(con, integer(), size = 2, signed = FALSE, endian = "little")
  }

  sidedefs
}

#' Read Segs Lump
#'
#' @param wad_path Path to the DOOM WAD file.
#' @param lump_info Data frame containing lump offset and size.
#' @return A data frame of segs.
#' @export
read_segs <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_segs <- lump_info$size %/% 12
  raw_ints <- readBin(
    con,
    what = integer(),
    n = n_segs * 6,
    size = 2,
    endian = "little",
    signed = FALSE
  )

  segs <- matrix(raw_ints, ncol = 6, byrow = TRUE)
  colnames(segs) <- c("v1", "v2", "angle", "linedef", "direction", "offset")
  as.data.frame(segs)
}

#' Read Ssectors Lump
#'
#' @param wad_path Path to the DOOM WAD file.
#' @param lump_info Data frame containing lump offset and size.
#' @return A data frame of ssectors.
#' @export
read_ssectors <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_ssectors <- lump_info$size %/% 4
  raw_ints <- readBin(
    con,
    integer(),
    n = n_ssectors * 2,
    size = 2,
    signed = FALSE,
    endian = "little"
  )

  ssectors <- matrix(raw_ints, ncol = 2, byrow = TRUE)
  colnames(ssectors) <- c("num_segs", "first_seg_index")
  as.data.frame(ssectors)
}

#' Read Nodes Lump
#'
#' @param wad_path Path to the DOOM WAD file.
#' @param lump_info Data frame containing lump offset and size.
#' @return A data frame of nodes.
#' @export
read_nodes <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_nodes <- lump_info$size %/% 28
  nodes <- data.frame(
    x = integer(n_nodes),
    y = integer(n_nodes),
    dx = integer(n_nodes),
    dy = integer(n_nodes),
    bbox0_top = integer(n_nodes),
    bbox0_bottom = integer(n_nodes),
    bbox0_left = integer(n_nodes),
    bbox0_right = integer(n_nodes),
    bbox1_top = integer(n_nodes),
    bbox1_bottom = integer(n_nodes),
    bbox1_left = integer(n_nodes),
    bbox1_right = integer(n_nodes),
    right_child = integer(n_nodes),
    left_child = integer(n_nodes)
  )

  for (i in seq_len(n_nodes)) {
    nodes[i, 1:12] <- readBin(
      con,
      integer(),
      n = 12,
      size = 2,
      signed = TRUE,
      endian = "little"
    )
    nodes$right_child[i] <- readBin(con, integer(), size = 2, signed = FALSE, endian = "little")
    nodes$left_child[i] <- readBin(con, integer(), size = 2, signed = FALSE, endian = "little")
  }

  nodes
}

#' Read Sectors Lump
#'
#' @param wad_path Path to the DOOM WAD file.
#' @param lump_info Data frame containing lump offset and size.
#' @return A data frame of sectors.
#' @export
read_sectors <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_sectors <- lump_info$size %/% 26
  sectors <- data.frame(
    floor_height = integer(n_sectors),
    ceiling_height = integer(n_sectors),
    floor_texture = character(n_sectors),
    ceiling_texture = character(n_sectors),
    light_level = integer(n_sectors),
    special_type = integer(n_sectors),
    tag = integer(n_sectors),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_sectors)) {
    sectors$floor_height[i] <- readBin(con, integer(), size = 2, signed = TRUE, endian = "little")
    sectors$ceiling_height[i] <- readBin(con, integer(), size = 2, signed = TRUE, endian = "little")
    sectors$floor_texture[i] <- read_wad_name(con)
    sectors$ceiling_texture[i] <- read_wad_name(con)
    sectors$light_level[i] <- readBin(con, integer(), size = 2, signed = TRUE, endian = "little")
    sectors$special_type[i] <- readBin(con, integer(), size = 2, signed = FALSE, endian = "little")
    sectors$tag[i] <- readBin(con, integer(), size = 2, signed = FALSE, endian = "little")
  }

  sectors
}
