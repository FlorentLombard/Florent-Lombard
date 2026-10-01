library(terra)
list.files("data/LC09_L2SP_196030_20240805_20240806_02_T1")

b2 <- rast("data/LC09_L2SP_196030_20240805_20240806_02_T1/LC09_L2SP_196030_20240805_20240806_02_T1_REFL_B2.tif")
b3 <- rast("data/LC09_L2SP_196030_20240805_20240806_02_T1/LC09_L2SP_196030_20240805_20240806_02_T1_REFL_B3.tif")
b4 <- rast("data/LC09_L2SP_196030_20240805_20240806_02_T1/LC09_L2SP_196030_20240805_20240806_02_T1_REFL_B4.tif")
b5 <- rast("data/LC09_L2SP_196030_20240805_20240806_02_T1/LC09_L2SP_196030_20240805_20240806_02_T1_REFL_B5.tif")
b6 <- rast("data/LC09_L2SP_196030_20240805_20240806_02_T1/LC09_L2SP_196030_20240805_20240806_02_T1_REFL_B6.tif")
b7 <- rast("data/LC09_L2SP_196030_20240805_20240806_02_T1/LC09_L2SP_196030_20240805_20240806_02_T1_REFL_B7.tif")
b2

crs(b2, describe = TRUE)

ncell(b2)

dim(b2)

res(b2)

nlyr(b2)

compareGeom(b2, b3)

s <- c(b5, b4, b3)
s

nlyr(s)

filenames <- paste0("data/LC09_L2SP_196030_20240805_20240806_02_T1/LC09_L2SP_196030_20240805_20240806_02_T1_REFL_B", 2:7, ".tif")
filenames

image <- rast(filenames)
nlyr(image)

par(mfrow = c(2, 2))
plot(b2, main = "Bleu", col = gray(0:100 / 100))
plot(b3, main = "Vert", col = gray(0:100 / 100))
plot(b4, main = "Rouge", col = gray(0:100 / 100))
plot(b5, main = "Proche infrarouge", col = gray(0:100 / 100))

par(mfrow = c(1, 1))

landsatRGB <- c(b4, b3, b2)
plotRGB(landsatRGB, stretch = "lin")

landsatFCC <- c(b5, b4, b3)
plotRGB(landsatFCC, stretch = "lin")

imagesub1 <- subset(image, 1:3)
imagesub2 <- image[[1:3]]
nlyr(image)

nlyr(imagesub1)

nlyr(imagesub2)

names(image)

names(image) <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")
names(image)

image[["NIR"]]

zone <- vect("data/LC09_L2SP_196030_20240805_20240806_02_T1/camargue.gpkg", layer = "camargue")
plot(image[["Red"]], main = "Image originale", range = c(0, 0.5))
plot(zone, add = TRUE, border = "red", lwd = 2)

imagecrop <- crop(image, zone)
plot(imagecrop[["Red"]], main = "Après crop()", range = c(0, 0.5))
plot(zone, add = TRUE, border = "red", lwd = 2)

imagezone <- mask(imagecrop, zone)
plot(imagezone[["Red"]], main = "Après mask()", range = c(0, 0.5))
plot(zone, add = TRUE, border = "red", lwd = 2)

par(mfrow = c(1, 3))
plot(image[["Red"]], main = "1. Image originale", range = c(0, 0.5))
plot(zone, add = TRUE, border = "red")
plot(imagecrop[["Red"]], main = "2. Après crop()", range = c(0, 0.5))
plot(zone, add = TRUE, border = "red")
plot(imagezone[["Red"]], main = "3. Après mask()", range = c(0, 0.5))
plot(zone, add = TRUE, border = "red")
par(mfrow = c(1, 1))

plotRGB(imagezone, r = 3, g = 2, b = 1, stretch = "lin")

writeRaster(imagezone, filename = "camargue_reflectance.tif", overwrite = TRUE)
file.exists("camargue_reflectance.tif")

pts_exemple <- vect("points_exemple.geojson")
plotRGB(imagezone, r = 3, g = 2, b = 1, stretch = "lin")
couleurs <- c("#1479b8", "#278044", "#ba8848", "#b62c47", "#9755a0")
points(pts_exemple, pch = 21, bg = couleurs[pts_exemple$classe],
       col = "white", cex = 1.2)
legend("bottomleft", legend = c("Eau", "Vegetation", "Sol nu", "Bati", "Cultures"),
       pch = 21, pt.bg = couleurs, col = "white", bty = "n")

df_exemple <- extract(imagezone, pts_exemple)
df_exemple$classe <- pts_exemple$classe
df_exemple$libelle <- pts_exemple$libelle
bandes <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")
moyennes <- aggregate(df_exemple[, bandes],
                      by = list(libelle = df_exemple$libelle),
                      FUN = mean, na.rm = TRUE)
moyennes

pts <- vect("data/LC09_L2SP_196030_20240805_20240806_02_T1/points_signatures.gpkg", layer = "points_signatures")
pts
head(as.data.frame(pts))
table(pts$classe, pts$libelle)
plotRGB(imagezone, r = 3, g = 2, b = 1, stretch = "lin")
points(pts, pch = 20, col = "yellow")

df <- extract(imagezone, pts)
head(df)
df$classe <- pts$classe
df$libelle <- pts$libelle
bandes <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")
ms <- aggregate(df[, bandes],
                by = list(classe = df$classe, libelle = df$libelle),
                FUN = mean, na.rm = TRUE)
ms
rownames(ms) <- ms$libelle
ms <- ms[, bandes]
ms

ms <- as.matrix(ms)
mycolor <- hcl.colors(nrow(ms), palette = "Dark 3")
plot(0, type = "n", xlim = c(1, ncol(ms)),
     ylim = range(ms, finite = TRUE), xaxt = "n",
     xlab = "Bandes", ylab = "Réflectance moyenne")
axis(1, at = 1:ncol(ms), labels = colnames(ms))
for (i in 1:nrow(ms)) {
  lines(1:ncol(ms), ms[i, ], type = "l", lwd = 3,
        lty = 1, col = mycolor[i])
}
title(main = "Signatures spectrales moyennes", font.main = 2)
legend("topleft", legend = rownames(ms),
       col = mycolor, lty = 1, lwd = 3, cex = 0.8, bty = "n")
