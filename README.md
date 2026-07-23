# utilscidados

<!-- badges: start -->
<!-- badges: end -->

Internal helpers for data work at Insper Cidades (ONMS and Cidados).

## Goals

The package exists to support two recurring workflows:

1.  **Standardise data exports for Insper's Dataverse.** Datasets
    published to Dataverse have to be shipped in several file formats
    with consistent naming conventions and a column-level data
    dictionary (metadata). `utilscidados` provides exporters
    (`export_table()`, `export_shapefile()`) that produce the full
    Dataverse-compliant set in one call, plus
    `build_documentation()` / `create_documentation_index()` to
    generate the documentation tables that accompany the data,
    `build_readme()` to scaffold the bilingual README that ships
    alongside a dataset, and `read_metadata()` / `update_metadata()` /
    `create_metadata_sheet()` / `validate_metadata()` to manage the
    Dataverse metadata workbook itself.

2.  **Simplify the shapefile -> Mapbox Studio (GeoPortal) pipeline.**
    GeoPortal ingests GeoJSON / GeoPackage, in WGS84, with predictable
    file names. `export_shapefile()` reprojects to EPSG:4326, sanitises
    the file name, and writes both formats so a layer is ready to
    upload to Mapbox Studio without any per-dataset wrangling.

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("portalcidados/utilscidados")
```

## Overview

| Function                       | Purpose                                                       |
| ------------------------------ | ------------------------------------------------------------- |
| `export_table()`               | Write a data frame to csv, xlsx, parquet, rds and/or feather  |
| `export_shapefile()`           | Write an `sf` object to geojson, gpkg, shp and/or geoparquet  |
| `export_tables_to_excel()`     | Write a named list of data frames to a multi-sheet xlsx file  |
| `mapbox_upload()`              | Upload an `sf` object to Mapbox Studio (GeoPortal) as a tileset |
| `build_documentation()`        | Build a column-level data dictionary for a dataset            |
| `create_documentation_index()` | Build an index mapping file names to table/sheet names        |
| `build_readme()`               | Generate a bilingual (EN/PT) README.txt skeleton              |
| `read_metadata()`              | Read field values from a Dataverse metadata workbook          |
| `update_metadata()`            | Write field values into a Dataverse metadata workbook         |
| `create_metadata_sheet()`      | Clone a template sheet for a new dataset in the workbook      |
| `validate_metadata()`          | Check a metadata sheet for missing fields or placeholders     |

Shared conventions across exporters:

* `file_name` is sanitised (lower-cased, special characters replaced
  with `_`) so the same input produces a filesystem-safe name on
  every platform.
* `out_dir` is created if it does not exist.
* Existing files are skipped with a warning unless `overwrite = TRUE`.
* `extension = "dataverse"` selects the Dataverse-compliant subset
  (csv + xlsx + parquet for tables; geojson + gpkg + shp + geoparquet
  for spatial).
* All progress and result messages use the cli package.

## Examples

### Dataverse export workflow

``` r
library(utilscidados)

# 1. Export the data in Dataverse-friendly formats
export_table(
  my_table,
  "indicadores municipais 2024",   # sanitised to "indicadores_municipais_2024"
  out_dir   = "dataverse-out",
  extension = "dataverse"
)

# 2. Build the column-level data dictionary
descs <- tibble::tibble(
  col_names   = c("cod_mun", "nome_mun", "pop_2022"),
  description = c("Codigo IBGE do municipio",
                  "Nome do municipio",
                  "Populacao residente no Censo 2022")
)
doc <- build_documentation(my_table, tbl_description = descs)

# 3. Bundle the index + per-table docs into a single workbook
index <- create_documentation_index(
  name_file  = "indicadores_municipais_2024.csv",
  name_table = "indicadores_municipais_2024"
)
export_tables_to_excel(
  tables    = list(indice = index, indicadores_municipais_2024 = doc),
  file_name = "dataverse-out/metadados.xlsx"
)
```

### GeoPortal (Mapbox Studio) upload workflow

``` r
camadas <- sf::st_read("source/camada_zoneamento.shp")

# Option A — write WGS84 files locally for manual upload / archiving
export_shapefile(
  camadas,
  "zoneamento_municipal",
  out_dir   = "mapbox-upload",
  extension = c("geojson", "gpkg"),
  overwrite = TRUE
)

# Option B — upload directly to Mapbox Studio in one call.
# Requires the suggested mapboxapi package and MAPBOX_SECRET_TOKEN in env.
mapbox_upload(
  camadas,
  username     = "insper_cidados",
  tileset_id   = "zoneamento_municipal",
  simplify     = 0.1,           # rmapshaper::ms_simplify(keep = 0.1)
  wait         = TRUE
)
```

## Scope

This is an internal package; it is not intended for CRAN. We try to
keep it CRAN-compliant nonetheless so that `R CMD check` passes
cleanly.
