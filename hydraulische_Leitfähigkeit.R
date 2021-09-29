#Tiefe 6cm
sht = excel_sheets("6cm.xlsx")
df = lapply(setNames(sht, sht), function(s) read_excel("6cm.xlsx", sheet=s))
df = bind_rows(df, .id="Sheet")



##6cm

library(gridExtra)
library(tidyr)
library(ggplot2)
library(dplyr)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin_l <- function(mapping = NULL, data = NULL, stat = "ydensity",
                               position = "dodge", trim = TRUE, scale = "area",
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolinL,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

geom_flat_violin_r <- function(mapping = NULL, data = NULL, stat = "ydensity",
                               position = "dodge", trim = TRUE, scale = "area",
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolinR,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolinL <-
  ggproto("GeomFlatViolinL", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x - width / 2,
                     xmax = x)
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data,
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin_l", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )


GeomFlatViolinR <-
  ggproto("GeomFlatViolinR", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x + width / 2,
                     xmax = x)
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data,
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin_R", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

p1 <- df %>%
  ggplot() +
  geom_point(aes(x=pF,y=A,color="1"),size=1,shape=1) +
  geom_point(aes(x=pF,y=B,color="2"),size=1,shape=1)+
  geom_path(aes(x=pF,y=C,color="3"))+
  geom_point(aes(x=pF,y=E,color="4"),size=1,shape=1)+
  scale_y_continuous(breaks=seq(-13,0,by=1),limits = c(-11.5,-3))+
  scale_x_continuous(breaks=seq(0,4,by=1),limits = c(-0.1,4))+
  geom_boxplot(mapping = aes(y = D), data = filter(df, !is.na(D)),
               fill = alpha("yellow", 0.5), width = 0.1) +
  labs(x="pF",y="log 10 K in m/s",title="")+
  theme_bw()+
  scale_color_manual(values=c("red2","yellowgreen","skyblue2","black"), name="", labels=c("Probennummer 17","Probennummer 1517","Fitting-Kurve","KU","KSat"))+
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0,1,0),
                                                  shape = c(1, 1,NA,1))))
p1



#Tiefe 17 cm

library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)




sht = excel_sheets("17cm.xlsx")
df = lapply(setNames(sht, sht), function(s) read_excel("17cm.xlsx", sheet=s))
df = bind_rows(df, .id="Sheet")




library(gridExtra)
library(tidyr)
library(ggplot2)
library(dplyr)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin_l <- function(mapping = NULL, data = NULL, stat = "ydensity",
                               position = "dodge", trim = TRUE, scale
                               = "area",
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolinL,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

geom_flat_violin_r <- function(mapping = NULL, data = NULL, stat = "ydensity",
                               position = "dodge", trim = TRUE, scale
                               = "area",
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolinR,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolinL <-
  ggproto("GeomFlatViolinL", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            ## ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x - width / 2,
                     xmax = x)
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data,
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin_l",
                             GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill =
                              "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )


GeomFlatViolinR <-
  ggproto("GeomFlatViolinR", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            ## ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x + width / 2,
                     xmax = x)
          },
          
          draw_group = function(data, panel_scales, coord) {
            ## Find the points for the line to go all the way around
            data <- transform(data,
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))
            
            ## Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            ## Close the polygon: set first and last point the same
            ## Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin_R",
                             GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill =
                              "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

ddf <- df %>%
  select(pF, A, B, C, D, E, G) %>%
  pivot_longer(!pF) %>%
  rename(x = pF, y = value, type = name) %>%
  drop_na() %>%
  mutate(type = factor(if_else(type == "E", NA_character_, type))) %>%
  mutate(type = factor(type, labels = c("Probennummer 15466","Probennummer 15718", "Probennummer 15720", "Fitting-Kurve","KU"))) %>%
  mutate(ptshape = factor(if_else(type %in% c("Fitting-Kurve", "Ksat"), NA_real_, 1)),
         ltype = factor(if_else(type =="Fitting-Kurve", 1, NA_real_)),
         bp = factor(if_else(type =="Ksat", 1, NA_real_)))



p1 <- ddf  %>%
  ggplot(aes(x = x, y = y, group = type)) +
  geom_boxplot(data = filter(ddf, is.na(type)), aes(y = y, fill = "Ksat"), width = 0.2) +
  geom_point(size = 2,aes(shape=ptshape, colour = type)) +
  geom_line(aes(linetype = ltype, colour = type)) +
  scale_y_continuous(breaks=seq(-13,0,by=1),limits = c(-11.5,-3)) +
  scale_x_continuous(breaks=seq(0,4,by=1),limits = c(-0.1,4)) +
  scale_color_manual(values=c("red2","yellowgreen","mediumpurple1","chocolate2","black"),
                     name = "", na.translate = FALSE) +
  scale_shape_manual(values = c(1, NA)) +
  scale_fill_manual(values = alpha("yellow", 0.5), name="") +
  theme_bw() +
  labs(x="pF",y="log 10 K in m/s",title="") +
  guides(colour = guide_legend(override.aes = list(size=c(2,2,2,1,2))),
         linetype = "none",
         shape = "none")+
  theme(text = element_text(size=13))

p1


#Tiefe 31cm

library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)




sht = excel_sheets("31cm.xlsx")
df = lapply(setNames(sht, sht), function(s) read_excel("31cm.xlsx", sheet=s))
df = bind_rows(df, .id="Sheet")




library(gridExtra)
library(tidyr)
library(ggplot2)
library(dplyr)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin_l <- function(mapping = NULL, data = NULL, stat = "ydensity",
                               position = "dodge", trim = TRUE, scale
                               = "area",
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolinL,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

geom_flat_violin_r <- function(mapping = NULL, data = NULL, stat = "ydensity",
                               position = "dodge", trim = TRUE, scale
                               = "area",
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolinR,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolinL <-
  ggproto("GeomFlatViolinL", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            ## ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x - width / 2,
                     xmax = x)
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data,
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin_l",
                             GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill =
                              "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )


GeomFlatViolinR <-
  ggproto("GeomFlatViolinR", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            ## ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x + width / 2,
                     xmax = x)
          },
          
          draw_group = function(data, panel_scales, coord) {
            ## Find the points for the line to go all the way around
            data <- transform(data,
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))
            
            ## Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            ## Close the polygon: set first and last point the same
            ## Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin_R",
                             GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill =
                              "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

ddf <- df %>%
  select(pF, A, B, C, D, E, G) %>%
  pivot_longer(!pF) %>%
  rename(x = pF, y = value, type = name) %>%
  drop_na() %>%
  mutate(type = factor(if_else(type == "E", NA_character_, type))) %>%
  mutate(type = factor(type, labels = c("Probennummer 15466","Probennummer 15718", "Probennummer 15720", "Fitting-Kurve","KU"))) %>%
  mutate(ptshape = factor(if_else(type %in% c("Fitting-Kurve", "Ksat"), NA_real_, 1)),
         ltype = factor(if_else(type =="Fitting-Kurve", 1, NA_real_)),
         bp = factor(if_else(type =="Ksat", 1, NA_real_)))



p1 <- ddf  %>%
  ggplot(aes(x = x, y = y, group = type)) +
  geom_boxplot(data = filter(ddf, is.na(type)), aes(y = y, fill = "Ksat"), width = 0.2) +
  geom_point(size = 2,aes(shape=ptshape, colour = type)) +
  geom_line(aes(linetype = ltype, colour = type)) +
  scale_y_continuous(breaks=seq(-13,0,by=1),limits = c(-11.5,-3)) +
  scale_x_continuous(breaks=seq(0,4,by=1),limits = c(-0.1,4)) +
  scale_color_manual(values=c("red2","yellowgreen","mediumpurple1","chocolate2","black"),
                     name = "", na.translate = FALSE) +
  scale_shape_manual(values = c(1, NA)) +
  scale_fill_manual(values = alpha("yellow", 0.5), name="") +
  theme_bw() +
  labs(x="pF",y="log 10 K in m/s",title="") +
  guides(colour = guide_legend(override.aes = list(size=c(2,2,2,1,2))),
         linetype = "none",
         shape = "none")+
  theme(text = element_text(size=13))

p1

#Tiefe 60cm

sht = excel_sheets("60cm.xlsx")
df = lapply(setNames(sht, sht), function(s) read_excel("60cm.xlsx", sheet=s))
df = bind_rows(df, .id="Sheet")


library(gridExtra)
library(tidyr)
library(ggplot2)
library(dplyr)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin_l <- function(mapping = NULL, data = NULL, stat = "ydensity",
                               position = "dodge", trim = TRUE, scale
                               = "area",
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolinL,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

geom_flat_violin_r <- function(mapping = NULL, data = NULL, stat = "ydensity",
                               position = "dodge", trim = TRUE, scale
                               = "area",
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolinR,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolinL <-
  ggproto("GeomFlatViolinL", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            ## ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x - width / 2,
                     xmax = x)
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data,
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin_l",
                             GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill =
                              "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )


GeomFlatViolinR <-
  ggproto("GeomFlatViolinR", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            ## ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x + width / 2,
                     xmax = x)
          },
          
          draw_group = function(data, panel_scales, coord) {
            ## Find the points for the line to go all the way around
            data <- transform(data,
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))
            
            ## Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            ## Close the polygon: set first and last point the same
            ## Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin_R",
                             GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill =
                              "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

ddf <- df %>%
  select(pF, A, B, C, D, E, G) %>%
  pivot_longer(!pF) %>%
  rename(x = pF, y = value, type = name) %>%
  drop_na() %>%
  mutate(type = factor(if_else(type == "E", NA_character_, type))) %>%
  mutate(type = factor(type, labels = c("5","15716","15725","Fitting-Kurve","KU"))) %>%
  mutate(ptshape = factor(if_else(type %in% c("Fitting-Kurve", "Ksat"), NA_real_, 1,size=4)),
         ltype = factor(if_else(type =="Fitting-Kurve", 1, NA_real_)),
         bp= factor(if_else(type =="Ksat", 1, NA_real_,size=4)))



p1 <- ddf  %>%
  ggplot(aes(x = x, y = y, group = type)) +
  geom_boxplot(data = ddf, aes(y = y, fill = "Ksat"), width = 0.1) +
  geom_point(size = 2,aes(shape=ptshape, colour = type)) +
  geom_line(size=1,aes(linetype = ltype, colour = type)) +
  scale_y_continuous(breaks=seq(-13,0,by=1),limits = c(-11.5,-3)) +
  scale_x_continuous(breaks=seq(0,4,by=1),limits = c(-0.1,4)) +
  scale_color_manual(values=c("red2","yellowgreen","mediumpurple1","yellow2","black"),
                     name = "", na.translate = FALSE) +
  scale_shape_manual(values = c(1, NA)) +
  scale_fill_manual(values = alpha("yellow", 0.5), name="") +
  theme_bw() +
  labs(x="volumetrischer Wassergehalt in %",y="log 10 K in m/s",title="") +
  guides(colour = guide_legend(override.aes = list(linetype = c(0,0,0,1,0),
                                                   shape = c(1, 1,1,NA,1),
                                                   size=c(2,2,2,1,2))),
         linetype = "none",
         shape = "none")

p1


