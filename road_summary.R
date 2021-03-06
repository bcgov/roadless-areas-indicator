# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


library(readr) # read in data
library(sf) # spatial object
library(dplyr) # data munging
library(readr) # load data
library(ggplot2) # plotting, dev version from GitHub for geom_sf
library(forcats) # reorder factors
library(bcmaps) # bc_bound()
library(scales) # comma()
library(envreportutils) # theme_soe(), png_retina()
library(R.utils) # capitalize
library(foreach) # parallel processing tiles
library(doMC) # parallel processing tiles

# Create output folders (if necessary)
DataDir <- "data"
dir.create(DataDir, showWarnings = FALSE)

if (!exists("out")) dir.create("out", showWarnings = FALSE)
if (!exists("out/data")) dir.create("out/data", showWarnings = FALSE)
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)


# load DRA raw data -----------------------------------------------------

# Raw DRA road file from BC Data Catalogue:
# https://catalogue.data.gov.bc.ca/dataset/digital-road-atlas-dra-master-partially-attributed-roads/resource/a06a2e11-a0b1-41d4-b857-cb2770e34fb0
RdsZip <- 'dra.gdb.zip'
download.file("ftp://ftp.geobc.gov.bc.ca/sections/outgoing/bmgs/DRA_Public/dgtl_road_atlas.gdb.zip",
              destfile = file.path(DataDir, RdsZip))
unzip(file.path(DataDir, RdsZip), exdir = file.path(DataDir, "DRA"))

# List feature classes in the geodatabase
Rd_gdb <- list.files(file.path(DataDir, "DRA"), pattern = ".gdb", full.names = TRUE)[1]
fc_list <- st_layers(Rd_gdb)

# Read as sf and calculate road lengths
roads_sf <- read_sf(Rd_gdb, layer = "TRANSPORT_LINE") %>%
  mutate(rd_len = st_length(.))

# Write metadata from gdb to csv files
# (sf >= 0.6-1 supports reading non-spatial tables))
lapply(fc_list$name[grepl("CODE$", fc_list$name)], function(l) {
  metadata <- st_read(Rd_gdb, layer = l, stringsAsFactors = FALSE)
  write_csv(metadata, path = file.path("data", paste0(l, ".csv")))
})

# Determine the FC extent, projection, and attribute information
summary(roads_sf)

# Save as RDS for quicker access later
saveRDS(roads_sf, file = "tmp/DRA_roads_sf.rds")


# clip to bc boundary -----------------------------------------------------
# Note: this chunk takes a 3-4 of hours to run

bc <- bc_bound_hres()

# Make a 10x10 grid of tiles to chunk out processing into smaller pieces
prov_grid <- st_make_grid(bc, n = c(10, 10))
prov_grid <- st_sf(tile_id = seq_along(prov_grid), geometry = prov_grid)

# Chop the bc boundary up into tiles using prov_grid
bc_gridded <- st_intersection(st_cast(bc, "POLYGON"), prov_grid) %>%
  mutate(grid_area = as.numeric(st_area(.)))

# Get the edge grids - those with an area less than a full square (with a 10m2 tolerance)
edge_grids <- unique(bc_gridded$tile_id[bc_gridded$grid_area < (round(max(bc_gridded$grid_area)) - 10)])

# Plot just bc_bound edge grids to check
plot(bc_gridded[bc_gridded$tile_id %in% edge_grids, "grid_area"])

# Chop the roads up by the same 10x10 tile grid. This takes a while
roads_gridded <- st_intersection(roads_sf, prov_grid)

# Split into two data frames - those grids on the edge and those in the interior.
interior_roads <- roads_gridded[!roads_gridded$tile_id %in% edge_grids, ]
edge_roads <- roads_gridded[roads_gridded$tile_id %in% edge_grids, ]

# Map over only edge tiles and intersect roads with prov boundary in parallel
registerDoMC(3)
edge_roads_clipped_list <- foreach(id = edge_grids) %dopar% {
  st_intersection(edge_roads[edge_roads$tile_id == id, ],
                  st_geometry(bc_gridded[bc_gridded$tile_id == id, ]))
}

# Recombine list of tiles into one sf object
edge_roads_clipped <- do.call("rbind", edge_roads_clipped_list)

# Combine clipped edge roads with interior and recalculate lenghts
roads_clipped <- rbind(interior_roads, edge_roads_clipped) %>%
  mutate(rd_len = st_length(.))

# Remove intermediate objects
rm(edge_roads, edge_roads_clipped, edge_roads_clipped_list, interior_roads, roads_gridded)

# Save roads_clipped sf object to RDS & write out as geopackage format
# for use in other software
saveRDS(roads_clipped, file = "tmp/roads_clipped.rds")
write_sf(roads_clipped, "out/data/roads_clipped.gpkg")

# All Road Tabular Summaries --------------------------------------------------------

# Load data files from local folders - if required
roads_clipped <- readRDS("tmp/roads_clipped.rds")
road_types <- read_csv("data/TRANSPORT_LINE_TYPE_CODE.csv")
road_surfaces <- read_csv("data/TRANSPORT_LINE_SURFACE_CODE.csv")

# Sum of road segment lengths
total_length_roads <- units::set_units(sum(roads_clipped$rd_len), km) %>% 
  round(digits = 0) %>% 
  scales::comma()

# Sum of ALL road segment lengths by TRANSPORT_LINE_TYPE_CODE
length_by_type <- roads_clipped %>%
  st_set_geometry(NULL) %>%
  group_by(TRANSPORT_LINE_TYPE_CODE) %>%
  summarise(total_length = as.numeric(units::set_units(sum(rd_len), km))) %>%
  left_join(road_types, by = "TRANSPORT_LINE_TYPE_CODE") %>%
  dplyr::select(TRANSPORT_LINE_TYPE_CODE, DESCRIPTION, total_length)

# Sum of ALL road segment lengths by TRANSPORT_LINE_SURFACE_CODE
length_by_surface <- roads_clipped %>%
  st_set_geometry(NULL) %>%
  group_by(TRANSPORT_LINE_SURFACE_CODE) %>%
  summarise(total_length = as.numeric(units::set_units(sum(rd_len), km))) %>%
  left_join(road_surfaces, by = "TRANSPORT_LINE_SURFACE_CODE") %>%
  dplyr::select(TRANSPORT_LINE_SURFACE_CODE, DESCRIPTION, total_length)

# Write out summary CSV file
write_csv(length_by_type, "out/roads_by_type_summary.csv")
write_csv(length_by_surface, "out/roads_by_surface_summary.csv")

# SoE Road Data --------------------------------------------------------

# Filter out some transport line types & surfaces
exclude_surface <- c("O", "B", "D") ## overgrown, boat and decommisioned
exclude_type <- c("T", "TD", "FR", "F", "FP", "RP", "RWA", "RPM") ## ferry routes, non-motorized trails, proposed, pedestrian mall

soe_roads <- roads_clipped %>% 
  filter(!TRANSPORT_LINE_TYPE_CODE %in% exclude_type) %>% 
  filter(!TRANSPORT_LINE_SURFACE_CODE %in% exclude_surface)

# Save soe_roads sf object to RDS & write out 
saveRDS(soe_roads, file = "tmp/soe_roads.rds")
write_sf(soe_roads, "out/data/soe_roads.gpkg")

# SoE Road Tabular summary ------------------------------------------------------------

# Load data files from local folders 
soe_roads <- readRDS("tmp/soe_roads.rds")
road_types <- read_csv("data/TRANSPORT_LINE_TYPE_CODE.csv")
road_surfaces <- read_csv("data/TRANSPORT_LINE_SURFACE_CODE.csv")

# Sum of SOE road segment lengths
soe_total_length_roads <- units::set_units(sum(soe_roads$rd_len), km) %>% 
  signif(digits = 3) %>% 
  scales::comma()

# Summarize road lengths by type, collapsing types into broad categories (paved and unpaved, which includes loose, rough, unknown & seasonal)

soe_roads_summary <-  soe_roads %>% 
  st_set_geometry(NULL) %>%
  group_by(TRANSPORT_LINE_SURFACE_CODE) %>%
  left_join(road_surfaces, by = "TRANSPORT_LINE_SURFACE_CODE") %>%
  mutate(DESCRIPTION = recode(DESCRIPTION, loose = "Unpaved",
                              rough = "Unpaved",
                              seasonal = "Unpaved",
                              unknown = "Unpaved"),
         DESCRIPTION = R.utils::capitalize(DESCRIPTION)) %>% 
  group_by(DESCRIPTION) %>% 
  summarise(total_length = signif(as.numeric(units::set_units(sum(rd_len), km)), digits=3)) %>% 
  mutate(percent_total = round((total_length / sum(total_length))*100, digits=1))
soe_roads_summary

write_csv(soe_roads_summary, "out/soe_roads_by_type_summary.csv")

# Plotting ------------------------------------------------------------


# Bar chart of roads by surface type
# Colour palette
colrs <- c("Unpaved" = "#993404",
           "Paved" = "#000000")

soe_roads_sum_chart <- soe_roads_summary %>% 
  ggplot(aes(fct_reorder(DESCRIPTION, rev(total_length)), total_length)) +
  geom_col(aes(fill = DESCRIPTION), alpha = 0.8) +
  geom_text(aes(y = total_length, label = paste0(scales::comma(total_length), " km"), hjust=-.1), size = 4) +
  scale_fill_manual(values = colrs, labels = unique(soe_roads_summary$DESCRIPTION),
                    guide = FALSE) +
  theme_soe() +
  coord_flip() +
  labs(x = "", y = "Total Length (km)") +
  scale_y_continuous(limits = c(0,840000),
                     breaks=seq(0, 800000, 200000),
                     expand = c(0, 0), label = comma) +
  theme(panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.margin = unit(c(10, 5, 15, 5), "mm"))
plot(soe_roads_sum_chart)


# Saving bar plot

saveRDS(soe_roads_sum_chart, file = "tmp/soe_roads_sum_chart.rds")

# PNG
png_retina(filename = "./out/soe_roads_by_surface.png", width = 500, height = 500, units = "px", type = "cairo-png")
plot(soe_roads_sum_chart)
dev.off()

# SVG for the web
svg_px(file = "./out/soe_roads_by_surface.svg", width = 500, height = 500)
plot(soe_roads_sum_chart)
dev.off()

# Plot of soe_roads map

# colour palette
colrs2 <- c("L" = "#993404",
            "R" = "#993404",
           "P" = "#000000",
           "S" = "#993404",
           "U" = "#993404")

# Using the ggplot2 dev version for geom_sf
soe_roads_map <- ggplot() +
  geom_sf(data = bc_bound(), fill = NA, size = 0.2) +
  geom_sf(data = soe_roads, aes(colour = TRANSPORT_LINE_SURFACE_CODE), size = 0.1) +
  coord_sf(datum = NA) +
  scale_colour_manual(values = colrs2, guide = FALSE) +
  theme_minimal()

# NOTE: plotting soe_roads is SLOWWWWW
# plot(soe_roads_map)

# Saving map plot (NOTE: takes ~ 27-28 hours)
png_retina(filename = "./out/soe_roads_map.png", width = 500, height = 500, units = "px", type = "cairo-png")
plot(soe_roads_map)
dev.off()


