install.packages("BiocManager")
BiocManager::install(c("pacman", "tmap", "sf", "tidyverse", "terra"))

## 加载包，如果加载tmap报错 s4vector 之类的，大概率是raster包的问题，
## 解决方法：将raster包卸载后，源码编译安装 
## install.packages("raster", type = "source")
pacman::p_load(tidyverse, sf, terra, tmap)
rm(list = ls())
## 关闭s2，设置投影
sf::sf_use_s2(FALSE) ## sf包默认开启s2，可能会导致有些奇怪的报错，
crs_84 <- st_crs("EPSG:4326") ## WGS 84 大地坐标
crs_al <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lon_0=105") ## Albers Equal Area Conic投影

## 中国地图(http://datav.aliyun.com/portal/school/atlas/area_selector 获取地图数据)
china_raw <- sf::st_read("china-GS(2019)6379.json")
china_valid <- sf::st_make_valid(china_raw)

### 制作大陆、南海轮廓(_84指的是WGS84坐标下的边框，_al指的是crs_al投影下的边框)
china_bbox_84 <- st_bbox(c(xmin = 72, xmax = 137, ymin = 17.5, ymax = 55), crs = crs_84)
china_bbox_al <-
    st_intersection(
        china_valid,
        st_as_sfc(china_bbox_84)
    ) %>%
    st_transform(crs_al) %>%
    st_bbox()
nanhai_bbox_84 <- st_bbox(c(xmin = 107.5, xmax = 122, ymin = 3, ymax = 23), crs = crs_84)

################################# 绘制各图层 #############################
#### 经纬度背景图层, 该图层无法单独显示，需要与其他图层一起，才可以出图
p_bg <-
    tm_graticules(
        x = seq(60, 180, 10), y = seq(0, 60, 10),
        col = "grey90", ticks = FALSE, labels.size = 0.8
    )
p_bg0 <- ## 无横纵坐标数字的经纬度图层，用于南海
    tm_graticules(
        x = seq(60, 180, 10), y = seq(0, 60, 10),
        col = "grey90", ticks = FALSE, labels.show = FALSE
    )
#### 轮廓 图层
p_china <-
    tm_shape(china_valid, projection = crs_al, bbox = china_bbox_al, is.master = T) +
    tm_borders(col = "grey60")
p_nanhai <-
    tm_shape(china_valid, bbox = nanhai_bbox_84, is.master = T) +
    tm_borders(col = "grey60")
p_bg + p_china
p_bg0 + p_nanhai
#### 采样点图层 
# 采样点随机生成
set.seed(123)
sites <-
    data.frame(
        lng = runif(20, 90, 120),
        lat = runif(20, 30, 40),
        lab = letters[1:10],
        group = rep(c("A", "B", "C", "D"), 5)
    ) %>%
    st_as_sf(coords = c("lng", "lat"), crs = crs_84)
# 采样点图层
p_sites <-
    tm_shape(sites) +
    tm_symbols(
        col = "group", shape = "group",
        shapes.legend = 21:24, legend.shape.show = F,
        palette = c("red", "black", "grey", "purple")
    ) +
    tm_text("lab", just = c(0.5, -1.4))

## 简单预览
p_bg + p_china + p_sites

#### 气候图层(可从 https://www.worldclim.org/ 获取气候数据)
amt <- terra::rast("wc2.1_10m_bio_1.tif") ## 年均温
amt_china <- crop(amt, ext(china_raw)) %>% mask(vect(china_raw))
col <- c("#ff0000", "#ffff00", "#11fbc3", "#00a6ff", "#0000ff")
## 绘图
p_amt <-
    tm_shape(amt_china) +
    tm_raster(palette = rev(col), style = "cont", midpoint = 5, title = "AMT")
p_amt

### 海拔图层
ele <- terra::rast("wc2.1_2.5m_elev.tif") ## 年均温
ele_china <- crop(ele, ext(china_raw)) %>% mask(vect(china_raw))
col2 <- c(
    "#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4",
    "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3"
) ## 海拔配色
## 绘图
p_ele <-
    tm_shape(ele_china) +
    # tm_raster(palette = rev(col2), style = "cont", title = "altitude")
    tm_raster(palette = "-RdYlGn", style = "cont", midpoint = 2500, title = "altitude")
p_ele

### 汇总，注意图层顺序，如果没有设置alpha透明度的话，后出的图层会覆盖前面的图层
## AMT(年均温汇总)
p_amt_china  <- p_bg + p_amt + p_china + p_sites +
    tm_layout(
        inner.margins = c(0.02, 0.02, 0.02, 0.12),
        outer.margins = c(0.02, 0.02, 0.02, 0.02)
    )
p_amt_nanhai  <- p_bg0 + p_amt + p_nanhai + tm_layout(legend.show = FALSE)
p_amt_china
p_amt_nanhai
## 海拔汇总
p_ele_china <- p_bg + p_ele + p_china + p_sites +
    tm_layout(
        inner.margins = c(0.02, 0.02, 0.02, 0.12),
        outer.margins = c(0.02, 0.02, 0.02, 0.02)
    )
p_ele_nanhai <- p_bg0 + p_ele + p_nanhai + tm_layout(legend.show = FALSE)
p_ele_china
p_ele_nanhai

### 进行拼图 
## 转换为 grob 对象，
g_amt_china  <- tmap_grob(p_amt_china)
g_amt_nanhai <- tmap_grob(p_amt_nanhai)
g_ele_china  <- tmap_grob(p_ele_china)
g_ele_nanhai <- tmap_grob(p_ele_nanhai)

## 设置右下角出南海，南海有效叫位置，又下面的参数来微调，
## 当然个人更建议出图后，用 inkscape 或 AI 或 cdr 软件拖动，比调整参数来得快
vp <- grid::viewport(
    x = 1.007, y = 0.1,
    width = 0.2, height = .3,
    just = c("right", "bottom")
)
g_amt <- grid::grobTree(g_amt_nanhai, vp = vp) %>% grid::addGrob(g_amt_china, .)
g_ele <- grid::grobTree(g_ele_nanhai, vp = vp) %>% grid::addGrob(g_ele_china, .)
# plot界面看看样图
grid::grid.newpage(); grid::grid.draw(g_amt)
grid::grid.newpage(); grid::grid.draw(g_ele)

