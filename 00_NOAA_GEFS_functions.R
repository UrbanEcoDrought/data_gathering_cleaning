# customizing the GEFS4cast functions to pull teh 0.25 degree data from GEFS
#' grib_extract
#'
#' @inheritParams gefs_grib_collection
#' @inheritDotParams gefs_view
#' @param bands numeric vector of bands to extract, see details
#' @param sites sf object of sites to extract
#' @return gdalcubes::image_collection()
#' @details See <https://www.nco.ncep.noaa.gov/pmb/products/gens/> for
#' details on GEFS data, including horizon, cycle, and band information.
#' This function is a simple wrapper around
#' [gdalcubes::extract_geom()].  Users can instead construct their own
#' gdalcubes image_collection with [gefs_grib_collection()] and cube view
#' (e.g. with [gefs_view()]), and then use [gdalcubes::raster_cube()] in
#' pipe-chain operations with any other gdalcubes functionality.
#'
#' @export
grib_extract <-function(ens,
                        date = Sys.Date(),
                        sites = neon_sites(),
                        bands = gefs_bands(),
                        horizon = gefs_horizon(),
                        cycle = "00",
                        
                        ...) {
  date <- lubridate::as_date(date)
  view <- gefs_view(date, ...)
  gefs_grib_collection(ens, date, horizon, cycle) |>
    gdalcubes::raster_cube(view) |>
    gdalcubes::select_bands(bands) |>
    gdalcubes::extract_geom(sites)
  
}

#' mapping of gefs_bands to variable names
#'
#' export
gefs_bands <- function() {
  bands = c("PRES"= "band57",
            "TMP" = "band63",
            "RH" = "band64",
            "UGRD" = "band67",
            "VGRD" = "band68",
            "APCP" = "band69",
            "DSWRF" = "band78",
            "DLWRF" = "band79")
}


#' gefs_grib_collection
#'
#' Generate grib URLs for a requested forecast timeseries (geospatial data cube)
#' as a [gdalcubes::image_collection()] object.
#' @param ens ensemble the ensemble member to process, e.g. 'gepavg'
#' can also be geavg (mean), gespr (esemble spread),
#'  gec00 (control) or gep01-gep30 (perturbed)
#' @param date start date
#' @param horizon list of horizon values, in hours (see [gefs_horizon()])
#' @param cycle forecast cycle, GEFS forecasts are produced four times a day,
#' at 00 (for 35 day horizon) and at 06 ,12, 18 hrs (at 16 day horizon)
#' Note that the control member, gec00, also has only a 16 day horizon
#' @param ... additional arguments got create_image_collection
#' @return gdalcubes::image_collection()
#' @details <https://www.nco.ncep.noaa.gov/pmb/products/gens/>
#'
#' @export
#'
gefs_grib_collection <- function(ens,
                                 date = Sys.Date(),
                                 horizon = gefs_horizon(),
                                 cycle = "00",
                                 ...) {
  date <- lubridate::as_date(date)
  date_time <- date + lubridate::hours(horizon)
  urls <- gefs_urls(ens, date, horizon, cycle)
  gribs <- paste0("/vsicurl/", urls)
  gdalcubes::create_image_collection(gribs, date_time = date_time, ...)
  
}

# https://www.nco.ncep.noaa.gov/pmb/products/gens/
gefs_urls <- function(ens,
                      date = Sys.Date(),
                      horizon = gefs_horizon(),
                      cycle = "00",
                      series = "atmos",
                      resolution = "0p25", # changed this from 0p50 to get the 0.25 degree data
                      base = "https://noaa-gefs-pds.s3.amazonaws.com") {
  date <- lubridate::as_date(date)
  date_time <- date + lubridate::hours(horizon)
  gribs <- paste0(
    base,
    "/gefs.",format(date, "%Y%m%d"),
    "/", cycle,
    "/",series,
    "/pgrb2sp25/",
    ens,
    ".t", cycle, "z.",
    "pgrb2sp25.0p25.", # Changed from 0p50
    "f", horizon)
  gribs
  print("Meow!!")
}

#' gefs_view
#'
#' A thin wrapper around gdalcubes::cube_view
#' with the default configuration for GEFS
#' @param t0 start time (Date or datetime object)
#' @param t1 end time. 35 days after start by default.
#' @param box bounding box giving (xmin, ymin, xmax, ymax) numeric vector
#' (or an sf_bbox object)
#' @param dx resolution, x-coordinate
#' @param dy resolution, y-coordinate
#' @param dt resolution, time coordinate, ISO8601 temporal-duration notation
#' @param crs Coordinate reference system (string)
#' @param ... additional options, see [gdalcubes::cube_view()]
#' @return a cube_view object
#' @examples
#' view <- gefs_view(as.Date("2022-12-01"))
#'
#' @export
gefs_view <- function (t0 = Sys.Date(),
                       t1 = as.Date(t0) + 35L - lubridate::seconds(1),
                       box = gefs_bbox(),
                       dx = 0.25, # changed from 0.5 to get the smaller scale data
                       dy = 0.25, # changed from 0.5 to get the smaller scale data
                       dt = "PT3H",
                       crs = "EPSG:4326",
                       ...
) {
  t0 <- lubridate::as_datetime(t0)
  t1 <- lubridate::as_datetime(t1)
  gdalcubes::cube_view(srs = crs,
                       extent = list(left = box[1], right = box[3],
                                     top = box[4], bottom = box[2],
                                     t0= format(t0, "%Y-%m-%dT%H:%M:%SZ"),
                                     t1 = format(t1, "%Y-%m-%dT%H:%M:%SZ")),
                       dx = dx, dy = dy, dt = dt, ...)
  
}

#' gefs_horizon
#' @return list of horizon values (for cycle 00, gepNN forecasts)
#' @export
gefs_horizon <- function() {
  c(stringr::str_pad(seq(3,240,by=3), 3, pad="0"),
    stringr::str_pad(seq(246,840,by=6), 3, pad="0"))
}


#' gefs ensemble list
#' @return Generates the strings for the 30 perturbed ensembles
#'
#' @export
gefs_ensemble <- function() {
  paste0("gep", stringr::str_pad(1:30, 2, pad="0"))
}

#' gefs bounding box
#' @export
gefs_bbox <- function(){
  ## alternately, extract from GRIB
  #date <- as.Date("2022-12-01")
  #grib <- paste0("/vsicurl/",
  #               "https://noaa-gefs-pds.s3.amazonaws.com/gefs.",
  #               format(date, "%Y%m%d"), "/00/atmos/pgrb2ap5/",
  #               "gep01", ".t00z.pgrb2a.0p50.f003")
  #box <- stars::read_stars(grib) |> sf::st_bbox()
  
  # WKT extracted from grib (not really necessary)
  wkt <- 'GEOGCRS["Coordinate System imported from GRIB file",
    DATUM["unnamed",
        ELLIPSOID["Sphere",6371229,0,
            LENGTHUNIT["metre",1,
                ID["EPSG",9001]]]],
    PRIMEM["Greenwich",0,
        ANGLEUNIT["degree",0.0174532925199433,
            ID["EPSG",9122]]],
    CS[ellipsoidal,2],
        AXIS["latitude",north,
            ORDER[1],
            ANGLEUNIT["degree",0.0174532925199433,
                ID["EPSG",9122]]],
        AXIS["longitude",east,
            ORDER[2],
            ANGLEUNIT["degree",0.0174532925199433,
                ID["EPSG",9122]]]]'
  bbox <- sf::st_bbox(c(xmin=-180.25, ymin=-90.25, xmax=179.75, ymax=90.25),
                      crs = wkt)
}


#' NEON sites as a simple features point geometry object
#' @return an sf object with coordinates for NEON sites
#' @export
neon_sites <- function() {
  sites <- readr::read_csv(paste0("https://github.com/eco4cast/",
                                  "neon4cast-noaa-download/",
                                  "raw/master/noaa_download_site_list.csv"))
  sf_sites <- sf::st_as_sf(sites,coords=c("longitude", "latitude"),
                           crs = 4326) |>
    tibble::rowid_to_column("FID")
}





# thin wrapper around gdalcubes::write_tif, probably not that useful
# Some workflows may prefer to create COG tifs in a remote cache / object store,
# allowing downstream users to work against the cloud-optimzed geotifs directly.
# When high bandwidth is available, this is especially efficient format for
# subsetting from full spatial data.
grib_to_tif <- function(ens,
                        date = Sys.Date(),
                        cube = gefs_grib_collection(ens, date),
                        view = gefs_view(date),
                        dir = NULL,
                        band = paste0("band", c(57, 63, 64, 67, 68, 69, 78, 79)),
                        creation_options=list(COMPRESS="zstd")) {
  
  if(is.null(dir)) {
    dir <- fs::dir_create(paste0("gefs.", format(date, "%Y%m%d")))
  }
  
  gdalcubes::raster_cube(cube, view) |>
    gdalcubes::select_bands(band) |>
    gdalcubes::write_tif(dir,
                         prefix=paste0(ens, ".t00z.pgrb2sp25.0p25.f"), # changed from 0p50 for the smaller scale data
                         COG=TRUE,
                         creation_options = creation_options)
}



