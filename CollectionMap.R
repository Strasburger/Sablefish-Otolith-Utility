library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggspatial)
theme_set(theme_bw())

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
rm(dir)

coll <- read.csv("CollectionMap.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

AK <- ggplot(data = world) +
  geom_sf(color = "black", fill = "light gray")+
  coord_sf(xlim = c(-170, -130), ylim = c(55, 72.5), expand = FALSE)+
  geom_point(data = coll, aes(x = EQLongitude, y = EQLatitude, shape = Collection), size = 2)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.55, "in"), pad_y = unit(0.40, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = "azure"), axis.title = element_blank(), legend.position = c(0.85, 0.58))

AK

ggsave("CollectionMap.tiff", plot = AK, width = 9.4, height = 5, units = "in", dpi = 600)

save.image(file = "CollectionMap.Rdata")
