# Lego maps
# www.overfitting.net
# https://www.overfitting.net/


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

# Read LEGO 1x1 brick
brick=readPNG("legobrick46x46.png")
BRICKSIZE=nrow(brick)
MIDGRAY=median(brick)  # median should be ~0.5 (8-bit 128/255)
LOGMIDGRAY=log(MIDGRAY)  # log precalculation
print(paste0("Median of brick vs 128/255: ", MIDGRAY, " vs ", 128/255))


# Add pseudo 3D borders to base brick
DARK=0.4
LIGHT=0.6
brick[1:2, 1:46]=LIGHT
brick[1:46, 45:46]=DARK
brick[45:46, 1:46]=DARK
brick[1:46, 1:2]=LIGHT
brick[1, 45]=LIGHT
brick[46, 2]=DARK


LEGOSIZE=60  # output vertical size (number of LEGO bricks)
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
            for (chan in 1:3) {
                # This gamma turns brick median (~0.5) into the pixel colour
                gamma=1/(log(imglite[i, j, chan])/LOGMIDGRAY)
                # Vectorized colouring for single brick RGB channel
                imgout[((i-1)*BRICKSIZE+1):(i*BRICKSIZE),
                       ((j-1)*BRICKSIZE+1):(j*BRICKSIZE), chan]=brick^(1/gamma)
            }
        }
    }
    # writeTIFF(imgout, paste0(name, "_lego.tif"), bits.per.sample=16, compression="LZW")
    writePNG(imgout, paste0(name, "_lego.png"))
}
