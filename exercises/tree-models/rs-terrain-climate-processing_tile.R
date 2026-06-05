# =============================================================================
# Download Landsat + 3DEP DEM + TerraClimate from Microsoft Planetary Computer
# Combined DSM covariate pipeline with tiled download support
# =============================================================================
# Packages: rstac, terra, sf, httr, climateR
# install.packages(c("rstac", "terra", "sf", "httr"))
# remotes::install_github("mikejohnson51/climateR")

library(rstac)
library(terra)
library(sf)
library(httr)
library(climateR)

# NULL-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# SETTINGS
# =============================================================================

# --- AOI (modify for your study area) ----------------------------------------
aoi_bbox <- c(-123.00, 44., -125.00, 46.00)  # xmin, ymin, xmax, ymax (WGS84)

# --- Tiling -----------------------------------------------------------------
# Tile size in degrees. AOIs larger than tile_size in either dimension
# are automatically split into tiles, processed independently, then mosaicked.
# Set to a large value (e.g., 999) to force single-pass mode.
#   0.25 deg (~25 km) - conservative, reliable, slower
#   0.5  deg (~50 km) - good default
#   1.0  deg (~100km) - faster but larger downloads, may timeout on 10m DEM
tile_size <- 0.5

# --- Landsat search parameters -----------------------------------------------
landsat_daterange <- "2023-07-01/2023-09-30"
landsat_cloud_max <- 20  # percent; relaxes to 50 if nothing found

# --- 3DEP resolution ----------------------------------------------------------
dem_gsd <- 30  # 10 = 1/3 arc-second (~10m), 30 = 1 arc-second (~30m)

# --- TerraClimate normals -----------------------------------------------------
tc_start_year <- 2013
tc_end_year   <- 2022
tc_vars <- c("tmin", "tmax", "ppt", "aet", "def", "soil", "pet", "vpd", "srad", "swe")

# --- Output -------------------------------------------------------------------
output_dir <- "dsm_covariates"
dir.create(output_dir, showWarnings = FALSE)
# tmp_dir <- file.path(output_dir, "tmp_downloads")
# dir.create(tmp_dir, showWarnings = FALSE)

# --- MPC STAC endpoint -------------------------------------------------------
mpc_stac <- "https://planetarycomputer.microsoft.com/api/stac/v1"
mpc_token_base <- "https://planetarycomputer.microsoft.com/api/sas/v1/token"


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# --- Get SAS token for a given storage account / container -------------------
get_mpc_token <- function(storage_account, container, max_attempts = 5) {
  url <- paste0(mpc_token_base, "/", storage_account, "/", container)
  
  for (attempt in 1:max_attempts) {
    resp <- httr::GET(url)
    status <- httr::status_code(resp)
    
    if (status == 200) {
      token <- httr::content(resp)$token
      cat("    SAS token acquired for", storage_account, "/", container, "\n")
      return(token)
    }
    
    if (status == 429 && attempt < max_attempts) {
      wait <- 10 * attempt  # 10s, 20s, 30s, 40s backoff
      cat("    Token rate limited (429), waiting", wait, "s (attempt",
          attempt, "/", max_attempts, ")...\n")
      Sys.sleep(wait)
    } else {
      stop("Failed to get SAS token for ", storage_account, "/", container,
           ". HTTP ", status)
    }
  }
}

# --- Build crop extent via sf (avoids terra PROJ database issue) -------------
get_crop_extent <- function(bbox, target_crs) {
  aoi_sf <- st_as_sfc(st_bbox(c(
    xmin = bbox[1], ymin = bbox[2],
    xmax = bbox[3], ymax = bbox[4]
  ), crs = st_crs(4326)))
  aoi_proj <- st_transform(aoi_sf, target_crs)
  ext(vect(aoi_proj))
}

# --- Split bbox into regular tiles -------------------------------------------
make_tiles <- function(bbox, tile_sz) {
  # Clamp tile_sz to AOI dimensions if AOI is smaller
  x_range <- bbox[3] - bbox[1]
  y_range <- bbox[4] - bbox[2]
  tile_x <- min(tile_sz, x_range)
  tile_y <- min(tile_sz, y_range)
  
  x_breaks <- seq(bbox[1], bbox[3], by = tile_x)
  y_breaks <- seq(bbox[2], bbox[4], by = tile_y)
  if (max(x_breaks) < bbox[3]) x_breaks <- c(x_breaks, bbox[3])
  if (max(y_breaks) < bbox[4]) y_breaks <- c(y_breaks, bbox[4])
  
  tiles <- expand.grid(
    xi = seq_along(x_breaks[-length(x_breaks)]),
    yi = seq_along(y_breaks[-length(y_breaks)])
  )
  tiles$id   <- paste0("tile_", seq_len(nrow(tiles)))
  tiles$xmin <- x_breaks[tiles$xi]
  tiles$xmax <- x_breaks[tiles$xi + 1]
  tiles$ymin <- y_breaks[tiles$yi]
  tiles$ymax <- y_breaks[tiles$yi + 1]
  tiles[, c("id", "xmin", "ymin", "xmax", "ymax")]
}

# --- Visualize tile layout ---------------------------------------------------
plot_tiles <- function(tiles, bbox) {
  aoi_poly <- st_as_sfc(st_bbox(c(
    xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]
  ), crs = 4326))
  tile_polys <- lapply(seq_len(nrow(tiles)), function(i) {
    st_as_sfc(st_bbox(c(
      xmin = tiles$xmin[i], ymin = tiles$ymin[i],
      xmax = tiles$xmax[i], ymax = tiles$ymax[i]
    ), crs = 4326))
  })
  tile_sf <- do.call(c, tile_polys) |> st_as_sf()
  tile_sf$id <- tiles$id
  plot(st_geometry(tile_sf), border = "blue", main = paste(nrow(tiles), "tiles"))
  plot(aoi_poly, border = "red", lwd = 2, add = TRUE)
  text(x = (tiles$xmin + tiles$xmax) / 2, y = (tiles$ymin + tiles$ymax) / 2,
       labels = tiles$id, cex = 0.6)
}

# --- Mosaic a list of SpatRasters --------------------------------------------
mosaic_rasters <- function(raster_list, name = "") {
  valid <- Filter(Negate(is.null), raster_list)
  if (length(valid) == 0) { cat("  No valid tiles for", name, "\n"); return(NULL) }
  if (length(valid) == 1) return(valid[[1]])
  cat("  Mosaicking", length(valid), "tiles for", name, "... ")
  result <- do.call(mosaic, valid)
  cat("done\n")
  result
}


# =============================================================================
# LANDSAT: Process a single tile
# =============================================================================
process_landsat_tile <- function(tile_bbox, tile_id,
                                 daterange    = landsat_daterange,
                                 cloud_max    = landsat_cloud_max,
                                 bands        = c("blue", "green", "red",
                                                  "nir08", "swir16", "swir22",
                                                  "lwir11")) {
  
  cat("\n  [Landsat] tile:", tile_id, "\n")
  
  # Query STAC
  items <- tryCatch({
    stac(mpc_stac) |>
      stac_search(collections = "landsat-c2-l2", bbox = tile_bbox,
                  datetime = daterange, limit = 20) |>
      post_request()
  }, error = function(e) { cat("    STAC query failed:", e$message, "\n"); NULL })
  
  if (is.null(items) || items_length(items) == 0) {
    cat("    No items found\n"); return(NULL)
  }
  
  # Filter to best scene
  props <- lapply(items$features, function(x) {
    data.frame(id = x$id, platform = x$properties$platform,
               cloud = x$properties$`eo:cloud_cover`, stringsAsFactors = FALSE)
  })
  props_df <- do.call(rbind, props)
  keep <- props_df$platform %in% c("landsat-8", "landsat-9") & props_df$cloud < cloud_max
  if (!any(keep)) keep <- props_df$platform %in% c("landsat-8", "landsat-9") & props_df$cloud < 50
  if (!any(keep)) { cat("    No suitable scene\n"); return(NULL) }
  
  best_item <- items$features[[which(props_df$id == props_df$id[keep][1])]]
  cat("    Scene:", props_df$id[keep][1],
      "cloud:", props_df$cloud[keep][1], "%\n")
  
  # Token + download with retry
  ls_token <- get_mpc_token("landsateuwest", "landsat-c2")
  tile_tmp <- file.path(tmp_dir, tile_id, "landsat")
  dir.create(tile_tmp, showWarnings = FALSE, recursive = TRUE)
  
  max_attempts <- 3
  for (band in bands) {
    base_url <- best_item$assets[[band]]$href
    if (is.null(base_url)) next
    dest <- file.path(tile_tmp, paste0(band, ".tif"))
    
    for (attempt in 1:max_attempts) {
      if (file.exists(dest) && file.size(dest) > 1000) break
      cat("    Downloading", band,
          if (attempt > 1) paste0("(attempt ", attempt, ")") else "", "... ")
      tryCatch({
        old_timeout <- getOption("timeout")
        options(timeout = 300)
        download.file(paste0(base_url, "?", ls_token), dest, mode = "wb", quiet = TRUE)
        options(timeout = old_timeout)
        cat("done\n")
      }, error = function(e) {
        options(timeout = old_timeout)
        if (file.exists(dest)) file.remove(dest)
        if (attempt < max_attempts) {
          cat("FAILED, retrying in 5s...\n")
          Sys.sleep(5)
        } else {
          cat("FAILED after", max_attempts, "attempts\n")
        }
      })
    }
  }
  
  # Crop downloaded bands
  test_file <- file.path(tile_tmp, paste0(bands[1], ".tif"))
  if (!file.exists(test_file)) {
    cat("    No bands downloaded successfully\n")
    return(NULL)
  }
  
  tile_crs <- crs(rast(test_file))
  tile_ext <- get_crop_extent(tile_bbox, tile_crs)
  
  band_rasters <- list()
  for (band in bands) {
    f <- file.path(tile_tmp, paste0(band, ".tif"))
    if (!file.exists(f) || file.size(f) < 1000) next
    r <- tryCatch({ crop(rast(f), tile_ext) }, error = function(e) NULL)
    if (!is.null(r)) { names(r) <- band; band_rasters[[band]] <- r }
  }
  
  if (length(band_rasters) == 0) {
    cat("    No bands cropped successfully\n")
    return(NULL)
  }
  ls_stack <- rast(band_rasters)
  
  # Check required bands for indices
  required <- c("blue", "red", "nir08", "swir16", "swir22")
  missing <- setdiff(required, names(ls_stack))
  
  if (length(missing) > 0) {
    cat("    WARNING: Missing bands for indices:", paste(missing, collapse = ", "), "\n")
    idx_stack <- NULL
  } else {
    scale_sr <- function(r) r * 0.0000275 - 0.2
    blue_sr  <- scale_sr(ls_stack[["blue"]])
    red_sr   <- scale_sr(ls_stack[["red"]])
    nir_sr   <- scale_sr(ls_stack[["nir08"]])
    swir1_sr <- scale_sr(ls_stack[["swir16"]])
    swir2_sr <- scale_sr(ls_stack[["swir22"]])
    
    ndvi       <- (nir_sr - red_sr) / (nir_sr + red_sr);        names(ndvi) <- "NDVI"
    ndmi       <- (nir_sr - swir1_sr) / (nir_sr + swir1_sr);    names(ndmi) <- "NDMI"
    savi       <- ((nir_sr - red_sr) / (nir_sr + red_sr + 0.5)) * 1.5; names(savi) <- "SAVI"
    bsi        <- ((swir1_sr + red_sr) - (nir_sr + blue_sr)) /
      ((swir1_sr + red_sr) + (nir_sr + blue_sr));    names(bsi) <- "BSI"
    clay_ratio <- swir1_sr / swir2_sr;                           names(clay_ratio) <- "Clay_Ratio"
    iron_ratio <- red_sr / blue_sr;                              names(iron_ratio) <- "Iron_Oxide_Ratio"
    idx_stack  <- c(ndvi, ndmi, savi, bsi, clay_ratio, iron_ratio)
  }
  
  # Write processed tiles to disk (keeps data persistent)
  tile_out_dir <- file.path(output_dir, "tiles")
  dir.create(tile_out_dir, showWarnings = FALSE, recursive = TRUE)
  
  ls_file <- file.path(tile_out_dir, paste0(tile_id, "_landsat.tif"))
  writeRaster(ls_stack, ls_file, overwrite = TRUE)
  
  idx_file <- NULL
  if (!is.null(idx_stack)) {
    idx_file <- file.path(tile_out_dir, paste0(tile_id, "_indices.tif"))
    writeRaster(idx_stack, idx_file, overwrite = TRUE)
  }
  
  # Clean up raw downloads (keep processed tiles)
  unlink(file.path(tmp_dir, tile_id, "landsat"), recursive = TRUE)
  
  cat("    Tile saved:", ls_file, "\n")
  list(
    landsat  = rast(ls_file),
    indices  = if (!is.null(idx_file)) rast(idx_file) else NULL,
    scene_id = props_df$id[keep][1]
  )
}


# =============================================================================
# 3DEP DEM: Process a single tile
# =============================================================================
process_dem_tile <- function(tile_bbox, tile_id, gsd = dem_gsd) {
  
  cat("\n  [DEM] tile:", tile_id, "\n")
  
  items <- tryCatch({
    stac(mpc_stac) |>
      stac_search(collections = "3dep-seamless", bbox = tile_bbox, limit = 50) |>
      post_request()
  }, error = function(e) { cat("    STAC query failed\n"); NULL })
  
  if (is.null(items) || items_length(items) == 0) {
    cat("    No items found\n"); return(NULL)
  }
  
  # Filter to target resolution
  props <- lapply(items$features, function(x) {
    data.frame(id = x$id, gsd = x$properties$gsd, stringsAsFactors = FALSE)
  })
  props_df <- do.call(rbind, props)
  keep <- props_df$gsd == gsd
  if (!any(keep)) keep <- which.min(abs(props_df$gsd - gsd))
  
  matching_ids <- unique(props_df$id[keep])
  cat("    DEM items:", length(matching_ids), "\n")
  
  dep_token <- get_mpc_token("ai4edataeuwest", "3dep")
  tile_tmp <- file.path(tmp_dir, tile_id, "dem")
  dir.create(tile_tmp, showWarnings = FALSE, recursive = TRUE)
  
  # Download with retry
  max_attempts <- 3
  for (i in seq_along(matching_ids)) {
    mid <- matching_ids[i]
    feat <- items$features[[which(props_df$id == mid)[1]]]
    base_url <- feat$assets[["data"]]$href
    dest <- file.path(tile_tmp, paste0("dem_", i, ".tif"))
    
    for (attempt in 1:max_attempts) {
      if (file.exists(dest) && file.size(dest) > 1000) break
      cat("    Downloading DEM part", i, "/", length(matching_ids),
          if (attempt > 1) paste0("(attempt ", attempt, ")") else "", "... ")
      tryCatch({
        old_timeout <- getOption("timeout")
        options(timeout = 600)
        download.file(paste0(base_url, "?", dep_token), dest, mode = "wb", quiet = TRUE)
        options(timeout = old_timeout)
        cat("done\n")
      }, error = function(e) {
        options(timeout = old_timeout)
        if (file.exists(dest)) file.remove(dest)
        if (attempt < max_attempts) {
          cat("FAILED, retrying in 5s...\n")
          Sys.sleep(5)
        } else {
          cat("FAILED after", max_attempts, "attempts\n")
        }
      })
    }
  }
  
  # Mosaic via VRT (avoids memory issues with multiple large tiles)
  part_files <- list.files(tile_tmp, pattern = "dem_.*\\.tif$", full.names = TRUE)
  if (length(part_files) == 0) {
    cat("    No DEM parts downloaded\n")
    return(NULL)
  }
  
  if (length(part_files) > 1) {
    vrt_file <- file.path(tile_tmp, "dem_mosaic.vrt")
    vrt(part_files, vrt_file)
    dem_raw <- rast(vrt_file)
  } else {
    dem_raw <- rast(part_files[1])
  }
  
  # Crop
  dem_crs <- crs(dem_raw)
  tile_ext <- get_crop_extent(tile_bbox, dem_crs)
  dem <- crop(dem_raw, tile_ext)
  names(dem) <- "elevation"
  
  # Terrain derivatives
  slope   <- terrain(dem, "slope", unit = "degrees")
  aspect  <- terrain(dem, "aspect", unit = "degrees")
  tpi     <- terrain(dem, "TPI")
  tri     <- terrain(dem, "TRI")
  rough   <- terrain(dem, "roughness")
  
  slope_rad     <- terrain(dem, "slope", unit = "radians")
  slope_clamped <- ifel(slope_rad < 0.001, 0.001, slope_rad)
  twi_approx    <- log(1 / tan(slope_clamped))
  names(twi_approx) <- "TWI_approx"
  
  curv <- tryCatch({
    r <- focal(dem, w = matrix(c(0,1,0, 1,-4,1, 0,1,0), 3, 3),
               fun = "sum", na.rm = TRUE)
    names(r) <- "curvature"
    r
  }, error = function(e) NULL)
  
  layers <- list(dem, slope, aspect, tpi, tri, rough, twi_approx)
  if (!is.null(curv)) layers <- c(layers, list(curv))
  terrain_stack <- rast(layers)
  
  # Write processed tile to disk (keeps data persistent)
  tile_out_dir <- file.path(output_dir, "tiles")
  dir.create(tile_out_dir, showWarnings = FALSE, recursive = TRUE)
  
  terr_file <- file.path(tile_out_dir, paste0(tile_id, "_terrain.tif"))
  writeRaster(terrain_stack, terr_file, overwrite = TRUE)
  
  # Clean up raw downloads only (keep processed tiles)
  unlink(file.path(tmp_dir, tile_id, "dem"), recursive = TRUE)
  
  cat("    Tile saved:", terr_file, "\n")
  rast(terr_file)
}


# =============================================================================
# PART 1: LANDSAT + 3DEP (with automatic tiling)
# =============================================================================
cat("\n##########################################################\n")
cat("# PART 1: Landsat + 3DEP (tiled if AOI is large)\n")
cat("##########################################################\n\n")

# --- Determine tiling -------------------------------------------------------
aoi_width  <- aoi_bbox[3] - aoi_bbox[1]
aoi_height <- aoi_bbox[4] - aoi_bbox[2]
use_tiling <- (aoi_width > tile_size) || (aoi_height > tile_size)

if (use_tiling) {
  tiles <- make_tiles(aoi_bbox, tile_size)
  cat("AOI split into", nrow(tiles), "tiles\n")
  plot_tiles(tiles, aoi_bbox)
} else {
  tiles <- data.frame(
    id = "tile_1", xmin = aoi_bbox[1], ymin = aoi_bbox[2],
    xmax = aoi_bbox[3], ymax = aoi_bbox[4], stringsAsFactors = FALSE
  )
  cat("AOI fits in single tile — no tiling needed\n")
}

# --- Process tiles -----------------------------------------------------------
ls_results  <- list()
idx_results <- list()
dem_results <- list()
ls_scene_ids <- character()

for (i in seq_len(nrow(tiles))) {
  t_bbox <- c(tiles$xmin[i], tiles$ymin[i], tiles$xmax[i], tiles$ymax[i])
  t_id   <- tiles$id[i]

  cat("\n=== Tile", i, "/", nrow(tiles), ":", t_id, "===\n")

  # Landsat
  ls_out <- tryCatch(
    process_landsat_tile(t_bbox, t_id),
    error = function(e) { cat("  Landsat failed:", e$message, "\n"); NULL }
  )
  if (!is.null(ls_out)) {
    ls_results[[t_id]]  <- ls_out$landsat
    idx_results[[t_id]] <- ls_out$indices
    ls_scene_ids <- c(ls_scene_ids, ls_out$scene_id)
  }

  # DEM + terrain
  dem_out <- tryCatch(
    process_dem_tile(t_bbox, t_id),
    error = function(e) { cat("  DEM failed:", e$message, "\n"); NULL }
  )
  if (!is.null(dem_out)) {
    dem_results[[t_id]] <- dem_out
  }

  # Pause between tiles to avoid rate limiting
  if (i < nrow(tiles)) Sys.sleep(5)
}

# --- Mosaic tiles ------------------------------------------------------------
cat("\n##########################################################\n")
cat("# Mosaicking tiles\n")
cat("##########################################################\n\n")

landsat_stack <- mosaic_rasters(ls_results, "Landsat bands")
indices_stack <- mosaic_rasters(idx_results, "spectral indices")
terrain_stack <- mosaic_rasters(dem_results, "terrain")

if (is.null(landsat_stack)) stop("No Landsat data processed successfully.")
if (is.null(terrain_stack)) stop("No terrain data processed successfully.")

# --- Save individual outputs -------------------------------------------------
writeRaster(landsat_stack, file.path(output_dir, "landsat_bands.tif"), overwrite = TRUE)
writeRaster(indices_stack, file.path(output_dir, "landsat_indices.tif"), overwrite = TRUE)
writeRaster(terrain_stack, file.path(output_dir, "terrain_covariates.tif"), overwrite = TRUE)


# =============================================================================
# PART 2: ALIGN GRIDS + COMBINED STACK
# =============================================================================
cat("\n##########################################################\n")
cat("# PART 2: Align grids + combined covariate stack\n")
cat("##########################################################\n\n")

# 3DEP is in geographic coords (arc-seconds); Landsat is in UTM (meters).
# Even at matching ~30m resolution, the grids need resampling to align
# CRS, extent, and cell registration.

cat("Resampling terrain to Landsat 30m grid...\n")
terrain_30m <- resample(terrain_stack, landsat_stack, method = "bilinear")
cat("  Resampled:", nrow(terrain_30m), "x", ncol(terrain_30m),
    "@ res", res(terrain_30m)[1], "\n")

cat("Building combined covariate stack...\n")
combined_stack <- c(landsat_stack, indices_stack, terrain_30m)

writeRaster(combined_stack,
            file.path(output_dir, "dsm_covariates_combined.tif"),
            overwrite = TRUE)

cat("  Combined:", nlyr(combined_stack), "layers\n")


# =============================================================================
# PART 3: TERRACLIMATE (no tiling needed — ~4km resolution)
# =============================================================================
cat("\n##########################################################\n")
cat("# PART 3: TerraClimate normals\n")
cat("##########################################################\n\n")

# Create AOI sf object
aoi_sf <- st_as_sfc(st_bbox(c(
  xmin = aoi_bbox[1], ymin = aoi_bbox[2],
  xmax = aoi_bbox[3], ymax = aoi_bbox[4]
), crs = st_crs(4326))) |> st_as_sf()

# --- Download year by year ---------------------------------------------------
tc_annual_list <- list()

for (yr in tc_start_year:tc_end_year) {
  cat("  Downloading", yr, "... ")
  tc_yr <- tryCatch({
    getTerraClim(
      AOI       = aoi_sf,
      varname   = tc_vars,
      startDate = paste0(yr, "-01-01"),
      endDate   = paste0(yr, "-12-31")
    )
  }, error = function(e) { cat("ERROR:", e$message, "\n"); NULL })

  if (!is.null(tc_yr)) {
    tc_annual_list[[as.character(yr)]] <- tc_yr
    cat("done\n")
  }
}

n_years <- length(tc_annual_list)
cat("\n  Successfully downloaded", n_years, "years\n")

if (n_years == 0) stop("No TerraClimate data downloaded.")

# --- Aggregate monthly -> annual per year ------------------------------------
cat("\nAggregating annual summaries...\n")

aggregate_year <- function(tc_yr) {
  list(
    tmin = mean(tc_yr$tmin, na.rm = TRUE),
    tmax = mean(tc_yr$tmax, na.rm = TRUE),
    ppt  = sum(tc_yr$ppt,   na.rm = TRUE),
    aet  = sum(tc_yr$aet,   na.rm = TRUE),
    def  = sum(tc_yr$def,   na.rm = TRUE),
    pet  = sum(tc_yr$pet,   na.rm = TRUE),
    soil = mean(tc_yr$soil, na.rm = TRUE),
    srad = mean(tc_yr$srad, na.rm = TRUE),
    vpd  = mean(tc_yr$vpd,  na.rm = TRUE),
    swe  = mean(tc_yr$swe,  na.rm = TRUE)
  )
}

annual_summaries <- list()
for (yr_name in names(tc_annual_list)) {
  cat("  ", yr_name, "... ")
  annual <- tryCatch(
    aggregate_year(tc_annual_list[[yr_name]]),
    error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(annual)) {
    annual_summaries[[yr_name]] <- annual
    cat("done\n")
  }
}

# --- Multi-year normals (mean across years) ----------------------------------
cat("\nComputing", n_years, "-year normals...\n")

var_names <- names(annual_summaries[[1]])
normals <- list()
for (v in var_names) {
  yearly_layers <- lapply(annual_summaries, function(x) x[[v]])
  yearly_stack  <- rast(yearly_layers)
  normal <- mean(yearly_stack, na.rm = TRUE)
  names(normal) <- v
  normals[[v]] <- normal
}
normal_stack <- rast(normals)

# --- Derived climate covariates ----------------------------------------------
cat("Deriving additional covariates...\n")

mat <- (normal_stack[["tmin"]] + normal_stack[["tmax"]]) / 2
names(mat) <- "MAT"

temp_range <- normal_stack[["tmax"]] - normal_stack[["tmin"]]
names(temp_range) <- "temp_range"

aridity <- normal_stack[["ppt"]] / normal_stack[["pet"]]
aridity <- ifel(is.finite(aridity), aridity, NA)
names(aridity) <- "aridity_index"

moisture_idx <- normal_stack[["aet"]] / normal_stack[["pet"]]
moisture_idx <- ifel(is.finite(moisture_idx), moisture_idx, NA)
names(moisture_idx) <- "moisture_index"

climate_covariates <- c(normal_stack, mat, temp_range, aridity, moisture_idx)

writeRaster(climate_covariates,
            file.path(output_dir, "terraclim_normals.tif"),
            overwrite = TRUE)

cat("  TerraClimate saved:", nlyr(climate_covariates), "layers\n")


# =============================================================================
# PART 4: FULL COVARIATE STACK (Landsat + terrain + climate)
# =============================================================================
cat("\n##########################################################\n")
cat("# PART 4: Full covariate stack\n")
cat("##########################################################\n\n")

# Project climate to Landsat CRS, then resample to 30m grid
cat("Projecting + resampling climate to Landsat grid...\n")
climate_projected <- project(climate_covariates, crs(landsat_stack))
climate_30m <- resample(climate_projected, landsat_stack, method = "bilinear")

full_stack <- c(combined_stack, climate_30m)

writeRaster(full_stack,
            file.path(output_dir, "full_covariate_stack.tif"),
            overwrite = TRUE)

cat("  Full stack:", nlyr(full_stack), "layers\n")


# =============================================================================
# SUMMARY
# =============================================================================
cat("\n==========================================================\n")
cat("  DSM COVARIATE DOWNLOAD COMPLETE\n")
cat("==========================================================\n")
cat("Output directory:", output_dir, "\n\n")

cat("Tiling:", nrow(tiles), "tile(s) @", tile_size, "deg\n\n")

cat("Files:\n")
cat("  landsat_bands.tif          (", nlyr(landsat_stack), "bands )\n")
cat("  landsat_indices.tif        (", nlyr(indices_stack), "indices )\n")
cat("  terrain_covariates.tif     (", nlyr(terrain_stack), "terrain layers )\n")
cat("  terraclim_normals.tif      (", nlyr(climate_covariates), "climate layers )\n")
cat("  dsm_covariates_combined.tif(", nlyr(combined_stack), "spectral+terrain @ 30m )\n")
cat("  full_covariate_stack.tif   (", nlyr(full_stack), "all layers @ 30m )\n")

cat("\nSpectral (", nlyr(landsat_stack), "):",
    paste(names(landsat_stack), collapse = ", "), "\n")
cat("Indices  (", nlyr(indices_stack), "):",
    paste(names(indices_stack), collapse = ", "), "\n")
cat("Terrain  (", nlyr(terrain_stack), "):",
    paste(names(terrain_stack), collapse = ", "), "\n")
cat("Climate  (", nlyr(climate_covariates), "):",
    paste(names(climate_covariates), collapse = ", "), "\n")

cat("\nLandsat scene(s):", paste(unique(ls_scene_ids), collapse = ", "), "\n")
cat("DEM:        3DEP @", dem_gsd, "m\n")
cat("Climate:    TerraClimate", tc_start_year, "-", tc_end_year,
    "(", n_years, "yr normals )\n")
cat("Output CRS:", crs(full_stack, describe = TRUE)$name, "\n")
cat("Output res:", res(full_stack)[1], "m\n")
cat("==========================================================\n")

# Clean up temp files (optional)
# unlink(tmp_dir, recursive = TRUE)


# =============================================================================
# NOTES
# =============================================================================
#
# --- Confirmed MPC SAS token endpoints (tested 2026-03-06) -----------------
#
# Token URL format:
#   https://planetarycomputer.microsoft.com/api/sas/v1/token/{account}/{container}
#
# Collection               Storage Account      Container            Status
# ----------------------   ------------------   ------------------   ------
# landsat-c2-l2            landsateuwest        landsat-c2           200 OK
# 3dep-seamless            ai4edataeuwest       3dep                 200 OK
# cop-dem-glo-30           elevationeuwest      copernicus-dem-stac  200 OK
# cop-dem-glo-90           elevationeuwest      copernicus-dem-stac  200 OK
# daymet-annual-na         daymeteuwest         daymet-zarr          200 OK
# daymet-monthly-na        daymeteuwest         daymet-zarr          200 OK
# daymet-daily-na          daymeteuwest         daymet-zarr          200 OK
# gridmet                  ai4edataeuwest       gridmet              200 OK
# terraclimate             cpdataeuwest         cpdata               200 OK *
# sentinel-2-l2a           sentinel2l2a01       sentinel2-l2         200 OK *
# naip                     naipeuwest           naip                 200 OK *
# esa-worldcover           ai4edataeuwest       esa-worldcover       200 OK *
#
# * May return 429 (rate limited) under rapid successive requests.
#   Add Sys.sleep(2) between calls or use a subscription key.
#
# --- Tiling guidance --------------------------------------------------------
#
# Tile size:
#   0.25 deg (~25 km) - conservative, reliable
#   0.5  deg (~50 km) - good default
#   1.0  deg (~100km) - faster, may timeout on 10m DEM
#
# Different tiles may use different Landsat scenes (path/row). The mosaic
# step merges them seamlessly. For single-scene consistency, filter by
# WRS path/row instead of using spatial tiling.
#
# For very large AOIs (>5 deg), consider writing tiles to disk and using
# terra::vrt() for virtual mosaics to manage memory:
#   files <- list.files(tmp_dir, "tile_.*\\.tif$", full.names = TRUE)
#   vrt(files, "mosaic.vrt")
#
# --- Better TWI with whitebox -----------------------------------------------
#
#   install.packages("whitebox")
#   whitebox::install_whitebox()
#   whitebox::wbt_breach_depressions_least_cost("dem.tif", "dem_filled.tif")
#   whitebox::wbt_d_inf_flow_accumulation("dem_filled.tif", "flowacc.tif")
#   whitebox::wbt_wetness_index("flowacc.tif", "slope.tif", "twi.tif")
#
# --- Climate variable guide -------------------------------------------------
#
# Variable         Aggregation   Soil relevance
# --------         -----------   ----------------------------------------
# tmin, tmax       mean          Weathering rates, organic matter turnover
# MAT              derived       Primary control on pedogenesis
# temp_range       derived       Freeze-thaw, continentality
# ppt (MAP)        sum           Leaching, soil moisture regime
# aet              sum           Actual water cycling through system
# def              sum           Water stress, moisture limitations
# pet              sum           Atmospheric demand for water
# soil             mean          Plant-available soil moisture
# srad             mean          Energy input, evapotranspiration driver
# vpd              mean          Atmospheric dryness, transpiration demand
# swe              mean          Snowpack, spring recharge
# aridity_index    derived       MAP/PET - soil moisture regime proxy
# moisture_index   derived       AET/PET - water use efficiency
#
# --- Adding gridMET daily metrics -------------------------------------------
#
#   gm <- getGridMET(AOI = aoi_sf, varname = c("tmmn", "tmmx", "pr"),
#                    startDate = "2022-01-01", endDate = "2022-12-31")
#   frost_free <- sum(gm$daily_minimum_temperature > 273.15, na.rm = TRUE)
#   names(frost_free) <- "frost_free_days"
#   tmean_k <- (gm$daily_minimum_temperature + gm$daily_maximum_temperature) / 2
#   gdd <- sum(ifel(tmean_k > 283.15, tmean_k - 283.15, 0), na.rm = TRUE)
#   names(gdd) <- "GDD_base10"
#
# --- PROJ/GDAL CRS workaround ----------------------------------------------
#
# This script uses sf::st_transform() instead of terra::project() on
# SpatVector objects to avoid the PROJ database mismatch issue.
# =============================================================================
