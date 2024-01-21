# Lego maps
# www.overfitting.net
# https://www.overfitting.net/2024/01/plegando-papel-con-r.html


library(tiff)  # save 16-bit TIFF's
library(png)  # save 8-bit PNG's
library(terra)  # resample

# Generic array resample function
# works both for matrix (grayscale images) or 3-channel arrays (colour images)
arrayresample=function(img, DIMX, DIMY, method='bilinear') {
    require(terra)
    
    raster=rast(img)
    rasterrs=rast(nrows=DIMY, ncols=DIMX, extent=ext(raster))
    rasterrs=resample(raster, rasterrs, method=method)
    return (as.array(rasterrs))
}


###########################################################

brick=readPNG("legobrick46x46.png")
# Pseudo 3D borders
DARK=0.4
LIGHT=0.6
brick[1:2, 1:46]=LIGHT
brick[1:46, 45:46]=DARK
brick[45:46, 1:46]=DARK
brick[1:46, 1:2]=LIGHT
brick[1, 45]=LIGHT
brick[46, 2]=DARK

MIDGRAY=median(brick)
LOGMIDGRAY=log(MIDGRAY)
print(paste0("Median of brick vs 128/255: ", MIDGRAY, " vs ", 128/255))
BRICKSIZE=nrow(brick)

LEGOSIZE=60
for (name in c('retrato', 'beso', 'guadarrama', 'girasoles')) {
    print(name)
    img=readPNG(paste0(name, ".png"))
    DIMY=nrow(img)
    DIMX=ncol(img)
    
    imglite=arrayresample(img, round(LEGOSIZE*DIMX/DIMY), LEGOSIZE)
    writePNG(imglite, paste0(name, "lite.png"))
    
    DIMY=nrow(imglite)
    DIMX=ncol(imglite)
    imgout=array(0, c(DIMY*BRICKSIZE, DIMX*BRICKSIZE, 3))
    
    imglite[imglite>0.95]=0.95  # clip highlights to prevent whitening
    imglite[imglite<0.05]=0.05  # clip shadows to prevent blackening
    for (i in 1:DIMY) {
        for (j in 1:DIMX) {
            for (k in 1:3) {
                invgamma=log(imglite[i, j, k])/LOGMIDGRAY
                imgout[((i-1)*BRICKSIZE+1):(i*BRICKSIZE),
                       ((j-1)*BRICKSIZE+1):(j*BRICKSIZE), k]=brick^invgamma
            }
        }
    }
    writeTIFF(imgout, paste0(name, "_lego.tif"), bits.per.sample=16, compression="LZW")
    writePNG(imgout, paste0(name, "_lego.png"))
}