popArea_region <- populatedAreas10[populatedAreas10$ADM0_A3 == iso3Code, ]
world_region <- st_crop(world,  xmin = regionInfoLookup[i, ll.lon], ymin = regionInfoLookup[i, ll.lat], xmax = regionInfoLookup[i, ur.lon], ymax = regionInfoLookup[i, ur.lat])
custom_bins <- round(seq.int(from = taveMin, to = taveMax, length = 5))
#   r_tave_region <- r_tave_region %>% mutate(mean_breaks = cut(mean, breaks = custom_bins))
g <- ggplot(data = world_region) +
  labs(title = titleText) + theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  #       geom_raster(data = r_tave_region, aes(x = x, y = y, fill = mean_breaks)) + 
  geom_raster(data = r_tave_region, aes(x, y, fill = mean)) + 
  # scale_fill_gradientn(colors = p, # name = legendText,
  #                      na.value = "grey50",
  #                      guide = "colorbar") +
  scale_fill_gradientn(colours=topo.colors(7),na.value = "transparent",
                       breaks=custom_bins,labels = custom_bins,
                       limits=c(taveMin, taveMax)) +
  geom_sf(fill = NA, color = "gray") + 
  
  geom_sf(data = roads_region, color = "red", lwd = 1) +
  geom_sf(data = populatedAreas_region) +
  geom_sf_label(data = populatedAreas_region,  aes(label = NAME), size = 5) +
  # theme(axis.title = element_blank()) + 
  #        scale_fill_manual(values = my_col, name = "Mean Temperature (°C)") + 
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
