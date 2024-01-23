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

# Read and preprocess 1x1 LEGO brick
brick=readPNG("legobrick46x46.png")  #  46x46 pixels grayscale bitmap
BRICKSIZE=nrow(brick)
MIDGRAY=median(brick)  # median should be ~0.5 (8-bit 128/255)
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


name=c('retrato', 'beso', 'guadarrama', 'girasoles', 'africa')
K=c(8, 4, 7, 8, 10)  # k-means clusters

LEGOSIZE=60  # output vertical size (number of LEGO bricks)

# Pipeline: img (input) -> imglite (downsized) -> imgclust (clustered)
for (n in 1:length(name)) {
    print(paste0("Resizing, clustering (k=", K[n], ") and LEGOing '",
                 name[n], "'..."))
    
    ################################
    # 1. DOWNSIZE IMAGE
    
    img=readPNG(paste0(name[n], ".png"))
    DIMY=nrow(img)
    DIMX=ncol(img)
    
    imglite=arrayresample(img, round(LEGOSIZE*DIMX/DIMY), LEGOSIZE)
    writePNG(imglite, paste0(name[n], "_lite.png"))

    
    ################################
    # 2. K-MEANS CLUSTERING
    
    # MEDIAN AVERAGING
    DIMX=ncol(imglite)
    DIMY=nrow(imglite)

    R=imglite[,,1]
    G=imglite[,,2]
    B=imglite[,,3]
    dim(R)=length(R)
    dim(G)=length(G)
    dim(B)=length(B)
    
    M=array(0, c(length(R), 3))  # Matriz RGB
    colnames(M)=c("R","G","B")
    M[,1]=R
    M[,2]=G
    M[,3]=B

    # K-MEANS CLUSTERING
    NCOLOURS=K[n]  # k clusters
    set.seed(0)  # reproducible clustering
    kmeansfit=kmeans(subset(M, select=c("R","G","B")), centers=NCOLOURS,
                     nstart=1000, iter.max=500)  # high nstart can prevent from
    clustering=kmeansfit$cluster           # missing the tiniest clusters
    centers=kmeansfit$centers
    
    # Clustering histogram
    png(paste0(name[n],"_histogram.png"), width=512, height=400)
    breaks=seq(0, NCOLOURS, length.out=NCOLOURS+1)
    colores=c()
    for (h in 1:NCOLOURS) {
        colores=c(colores, rgb(centers[h,1], centers[h,2], centers[h,3]))
    }
    hist(clustering, breaks=breaks, col=colores, # lty="blank",
         main=paste0("'",name[n],"' cluster histogram (k=", K[n], ")"), axes=FALSE)
    axis(1, at=breaks, labels=TRUE)
    dev.off()
    
    # Clustered image
    imgcentersR=array(0, DIMY*DIMX)
    imgcentersG=imgcentersR
    imgcentersB=imgcentersR
    for (i in 1:NCOLOURS) {
        indices=which(clustering==i)
        imgcentersR[indices]=centers[i,1]
        imgcentersG[indices]=centers[i,2]
        imgcentersB[indices]=centers[i,3]
    }
    dim(imgcentersR)=c(DIMY,DIMX)
    dim(imgcentersG)=c(DIMY,DIMX)
    dim(imgcentersB)=c(DIMY,DIMX)
    
    imgclust=array(0, c(DIMY,DIMX,3))
    imgclust[,,1]=imgcentersR
    imgclust[,,2]=imgcentersG
    imgclust[,,3]=imgcentersB
    # writeTIFF(imgclust, paste0(name[n],"_clustered.tif"),
    #           bits.per.sample=16, compression="LZW")
    writePNG(imgclust, paste0(name[n],"_lite_clustered.png"))

    
    ################################
    # 3. BUILD FINAL IMAGE WITH LEGO BRICKS

    DIMY=nrow(imgclust)
    DIMX=ncol(imgclust)
    imgout=array(0, c(DIMY*BRICKSIZE, DIMX*BRICKSIZE, 3))
    # imgclust=imglite
    imgclust[imgclust>0.95]=0.95  # clip highlights to prevent whitening
    imgclust[imgclust<0.05]=0.05  # clip shadows to prevent blackening
    for (i in 1:DIMY) {
        for (j in 1:DIMX) {
            for (chan in 1:3) {
                # This gamma turns brick median (~0.5) into the pixel colour
                gamma=1/(log(imgclust[i, j, chan])/log(MIDGRAY))
                # Vectorized colouring for single brick RGB channel
                imgout[((i-1)*BRICKSIZE+1):(i*BRICKSIZE),
                       ((j-1)*BRICKSIZE+1):(j*BRICKSIZE), chan]=brick^(1/gamma)
            }
        }
    }
    # writeTIFF(imgout, paste0(name[n], "_lego.tif"), bits.per.sample=16, compression="LZW")
    writePNG(imgout, paste0(name[n], "_lego.png"))
}


