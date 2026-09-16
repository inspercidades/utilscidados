# mapbox_upload ----

#' Upload a Spatial Layer to Mapbox Studio (GeoPortal)
#'
#' Uploads an `sf` object (or an existing GeoJSON file) to Mapbox via
#' the Uploads API as a tileset, ready to use as a Mapbox Studio source.
#' Optionally simplifies the geometry first with
#' [rmapshaper::ms_simplify()].
#'
#' @param shp Either an `sf` object or a character path to an existing
#'   GeoJSON / shapefile / MBTiles file that Mapbox's Uploads API
#'   accepts.
#' @param username Mapbox username (the prefix of the full tileset ID).
#' @param tileset_id Character scalar with the short tileset slug (the
#'   part *after* the username, e.g. `"zoneamento_2024"`). Required.
#'   Will be sanitised (lower-cased, special characters replaced with
#'   `_`) before submission.
#' @param tileset_name Optional human-readable name shown in Mapbox
#'   Studio. Defaults to `tileset_id`.
#' @param access_token Mapbox secret token with `uploads:write` scope.
#'   Defaults to the `MAPBOX_SECRET_TOKEN` environment variable.
#' @param simplify Geometry simplification. `FALSE` (default) writes
#'   the layer as-is. `TRUE` calls [rmapshaper::ms_simplify()] with
#'   `keep = 0.05`. A numeric in `(0, 1)` is passed as `keep` to
#'   `ms_simplify()`.
#' @param keep_geojson Logical; when `shp` is an `sf` object, whether
#'   to leave the intermediate GeoJSON file on disk after upload.
#' @param multipart Logical; pass through to
#'   [mapboxapi::upload_tiles()] for large files.
#' @param wait Logical. If `TRUE`, blocks until Mapbox reports the
#'   upload as complete (or failed). If `FALSE` (default), returns
#'   immediately after submission.
#' @param poll_interval Seconds between status polls when `wait = TRUE`.
#'
#' @return Invisibly returns the response from
#'   [mapboxapi::upload_tiles()] (or, if `wait = TRUE`, the final
#'   status response from [mapboxapi::check_upload_status()]).
#'
#' @details
#' Requires the suggested packages `mapboxapi` (always) and
#' `rmapshaper` (only when `simplify` is not `FALSE`).
#'
#' The Uploads API caps individual uploads at roughly 300 MB
#' (uncompressed) or 260 MB GeoJSON. For larger layers, use
#' [mapboxapi::mts_create_tileset()] / [mapboxapi::mts_publish_tileset()]
#' directly.
#'
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#'
#' # Simplest path: write a tileset called "<your_user>.nc_counties"
#' mapbox_upload(
#'   nc,
#'   username   = "insper_cidados",
#'   tileset_id = "nc_counties"
#' )
#'
#' # Aggressive simplification for a heavy polygon layer, then wait
#' # for Mapbox to finish processing before returning.
#' mapbox_upload(
#'   nc,
#'   username     = "insper_cidados",
#'   tileset_id   = "nc_counties_simplified",
#'   simplify     = 0.1,
#'   wait         = TRUE
#' )
#' }
#'
#' @seealso [export_shapefile()] for writing a layer to disk in
#'   GeoJSON / GeoPackage formats before uploading.
#' @export
mapbox_upload <- function(
  shp,
  username,
  tileset_id,
  tileset_name = NULL,
  access_token = Sys.getenv("MAPBOX_SECRET_TOKEN"),
  simplify = FALSE,
  keep_geojson = FALSE,
  multipart = TRUE,
  wait = FALSE,
  poll_interval = 5
) {
  rlang::check_installed("mapboxapi", reason = "to upload to Mapbox Studio.")

  if (missing(username) || !is.character(username) || length(username) != 1) {
    cli::cli_abort("{.arg username} must be a single Mapbox username.")
  }
  if (
    missing(tileset_id) || !is.character(tileset_id) || length(tileset_id) != 1
  ) {
    cli::cli_abort(
      "{.arg tileset_id} must be a single character slug (the part after the username)."
    )
  }
  if (!nzchar(access_token)) {
    cli::cli_abort(
      "No Mapbox access token. Pass {.arg access_token} or set {.envvar MAPBOX_SECRET_TOKEN}."
    )
  }

  clean_id <- clean_file_name(tileset_id)
  if (clean_id != tileset_id) {
    cli::cli_inform(
      "Sanitised {.arg tileset_id}: {.val {tileset_id}} -> {.val {clean_id}}"
    )
  }
  if (is.null(tileset_name)) {
    tileset_name <- clean_id
  }

  # Optional simplification --------------------------------------------------
  if (!isFALSE(simplify) && inherits(shp, "sf")) {
    rlang::check_installed(
      "rmapshaper",
      reason = "to simplify geometry before upload."
    )
    keep <- if (isTRUE(simplify)) 0.05 else simplify
    if (!is.numeric(keep) || length(keep) != 1 || keep <= 0 || keep >= 1) {
      cli::cli_abort(
        "{.arg simplify} must be FALSE, TRUE, or a single numeric in (0, 1)."
      )
    }
    cli::cli_alert_info(
      "Simplifying geometry with {.code rmapshaper::ms_simplify(keep = {keep})}"
    )
    shp <- rmapshaper::ms_simplify(shp, keep = keep, keep_shapes = TRUE)
  }

  # Submit upload ------------------------------------------------------------
  cli::cli_alert_info(
    "Uploading {.val {clean_id}} to Mapbox as {.val {username}.{clean_id}}"
  )

  result <- mapboxapi::upload_tiles(
    input = shp,
    username = username,
    access_token = access_token,
    tileset_id = clean_id,
    tileset_name = tileset_name,
    keep_geojson = keep_geojson,
    multipart = multipart
  )

  upload_id <- result$id %||% result[["id"]]
  if (is.null(upload_id)) {
    cli::cli_alert_warning(
      "Upload submitted but no upload id returned; cannot poll status."
    )
    return(invisible(result))
  }

  cli::cli_alert_success(
    "Upload submitted (id: {.val {upload_id}})."
  )

  if (!isTRUE(wait)) {
    cli::cli_alert_info(
      "Returning before Mapbox finishes processing. Pass {.arg wait = TRUE} to block."
    )
    return(invisible(result))
  }

  # Blocking poll ------------------------------------------------------------
  cli::cli_progress_step(
    "Waiting for Mapbox to finish processing tileset",
    spinner = TRUE
  )
  repeat {
    status <- mapboxapi::check_upload_status(
      upload_id = upload_id,
      username = username,
      access_token = access_token
    )
    if (isTRUE(status$complete)) {
      cli::cli_progress_done()
      cli::cli_alert_success(
        "Tileset {.val {username}.{clean_id}} is live in Mapbox Studio."
      )
      return(invisible(status))
    }
    if (isTRUE(nzchar(status$error %||% ""))) {
      cli::cli_progress_done()
      cli::cli_abort("Mapbox reported an error: {status$error}")
    }
    Sys.sleep(poll_interval)
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x
