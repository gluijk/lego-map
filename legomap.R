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


# Function to draw an arbitrarily sized brick
drawbrick=function(img, xmin, xmax, ymin, ymax,
                   brick, BRICKSIZE, MIDGRAY, imgcolour) {
    DARK=0.3
    LIGHT=0.7
    
    imgout=img
    # Build base grayscale brick
    for (i in ymin:ymax) {
        for (j in xmin:xmax)
            for (chan in 1:3) { imgout[((i-1)*BRICKSIZE+1):(i*BRICKSIZE),
                                       ((j-1)*BRICKSIZE+1):(j*BRICKSIZE),
                                       chan]=brick
        }
    }
    
    # Brick limits in imgout
    rangeymin = (ymin-1)*BRICKSIZE+1
    rangeymax = ymax*BRICKSIZE
    rangexmin = (xmin-1)*BRICKSIZE+1
    rangexmax = xmax*BRICKSIZE
    
    # Draw pseudo 3D borders
    imgout[rangeymin:(rangeymin+1), rangexmin:rangexmax,]=LIGHT
    imgout[rangeymin:rangeymax, (rangexmax-1):rangexmax,]=DARK
    imgout[(rangeymax-1):rangeymax, rangexmin:rangexmax,]=DARK
    imgout[rangeymin:rangeymax, rangexmin:(rangexmin+1),]=LIGHT
    imgout[rangeymin, rangexmax-1,]=LIGHT
    imgout[rangeymax, rangexmin+1,]=DARK
    
    # Colour brick according to imgcolour
    for (chan in 1:3) {
        gamma=1/(log(imgcolour[ymin, xmin, chan])/log(MIDGRAY))
        imgout[rangeymin:rangeymax, rangexmin:rangexmax, chan]=
            imgout[rangeymin:rangeymax, rangexmin:rangexmax, chan]^(1/gamma)
    }

    return(imgout)
}



###########################################################

# Read and preprocess 1x1 LEGO brick
brick=readPNG("legobrick46x46.png")  #  46x46 pixels grayscale bitmap
brick=readPNG("legobrick25x25.png")  #  25x25 pixels grayscale bitmap
BRICKSIZE=nrow(brick)
MIDGRAY=median(brick)  # median should be ~0.5 (8-bit 128/255)
print(paste0("Median of brick vs 128/255: ", MIDGRAY, " vs ", 128/255))


# List of images to LEGOnize
name=c('perla', 'gioconda', 'modigliani', 'girasoles', 'picasso',
       'warhol', 'vangogh', 'urbino', 'katemoss', 'retrato', 'beso',
       'guadarrama', 'world', 'world2', 'world3', 'world4', 'world5', 'africa')
K=c(7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7)  # k-means clusters

name=c('sonic')
K=c(14)  # k-means clusters

name=c('pokemon')
K=c(4)  # k-means clusters

name=c('tenerife')
K=c(8)  # k-means clusters

LEGOSIZE=50  # output vertical size (number of LEGO bricks)


# Pipeline:
# img (input) -> imglite (downsized) -> imgclust (clustered) -> imgout (LEGO)
for (n in 1:length(name)) {
    print(paste0("Resizing, clustering (k=", K[n], ") and LEGOnizing '",
                 name[n], "'..."))
    
    ################################
    # 1. DOWNSIZE IMAGE
    
    img=readPNG(paste0(name[n], ".png"))
    DIMY=nrow(img)
    DIMX=ncol(img)
    
    img[img==0]=NA  # ignore blacks in resample to prevent creating mixed colours
    imglite=arrayresample(img, round(LEGOSIZE*DIMX/DIMY), LEGOSIZE)
    writePNG(imglite, paste0("lite_", name[n], ".png"))
    
    
    ################################
    # 2. K-MEANS CLUSTERING
    
    DIMX=ncol(imglite)
    DIMY=nrow(imglite)
    
    imglite[is.na(imglite)]=0
    
    # Rearrange imglite as a N x 3 array with RGB values in 3 columns
    M=cbind(c(imglite[,,1]), c(imglite[,,2]), c(imglite[,,3]))
    colnames(M)=c("R","G","B")
    
    # Standard k-means clustering
    NCOLOURS=K[n]  # k clusters
    set.seed(0)  # reproducible clustering
    kmeansfit=kmeans(subset(M, select=c("R","G","B")), centers=NCOLOURS,
                     nstart=1000, iter.max=500)  # high nstart can prevent from
    clustering=kmeansfit$cluster           # missing the tiniest clusters
    centers=kmeansfit$centers  # clustering centroids (average colours)
    
    # Clustering histogram
    png(paste0("hist_", name[n], ".png"), width=512, height=400)
    breaks=seq(0, NCOLOURS, length.out=NCOLOURS+1)
    colores=rgb(centers[,1], centers[,2], centers[,3])
    hist(clustering, breaks=breaks, col=colores, # lty="blank",
         main=paste0("'", name[n], "' cluster histogram (k=", K[n], ")"), axes=FALSE)
    axis(1, at=breaks, labels=TRUE)
    dev.off()
    
    # Build clustered coloured image
    imgclust=array(0, c(DIMY*DIMX,3))  # configure DIMY*DIMX x 3 array
    for (i in 1:NCOLOURS) {  # loop through clusters
        indices=which(clustering==i)
        for (chan in 1:3) imgclust[indices,chan]=centers[i,chan]
    }
    dim(imgclust)=c(DIMY,DIMX,3)  # redim to DIMY x DIMX x 3 array (RGB image)
    
    # writeTIFF(imgclust, paste0(name[n],"_clustered.tif"),
    #           bits.per.sample=16, compression="LZW")
    writePNG(imgclust, paste0("cluster_", name[n], ".png"))
    
    
    ################################
    # 3. BUILD FINAL IMAGE WITH LEGO BRICKS
    
    DIMY=nrow(imgclust)
    DIMX=ncol(imgclust)
    imgout=array(0, c(DIMY*BRICKSIZE, DIMX*BRICKSIZE, 3))
    
    # imgclust=imglite  # to preserve colour gradients output
    imgclust[imgclust>0.95]=0.95  # clip highlights to prevent whitening
    imgclust[imgclust<0.05]=0.05  # clip shadows to prevent blackening

    # Brute force LEGO brick fitting algorithm
    LEGOBRICKS=list(c(8,8), c(6,6), c(6,4), c(4,4), c(2,4), c(2,3), c(2,2),
                    c(4,1), c(3,1), c(2,1), c(1,1))  # hierarchical list
    NSIZES=length(LEGOBRICKS)

    NBRICKS=0
    # for (k in 1:NCOLOURS) {  # loop trough clusters
    for (k in c(1,3,4,5,6,7,8)) {  # ignore cluster 2 (Tenerife's sea)
        indices=which(clustering==k)
        imgclust1=array(0, c(DIMY, DIMX))
        imgclust1[indices]=1  # set to 1 pixels belonging to cluster k
        for (l in 1:NSIZES) {  # loop trough brick sizes
            DIMYBRICK=LEGOBRICKS[[l]][1]
            DIMXBRICK=LEGOBRICKS[[l]][2]
            AREABRICK=DIMXBRICK*DIMYBRICK

            # Try both 0º and 90º rotation on non-square bricks
            for (rotate in 1:ifelse(DIMYBRICK==DIMXBRICK, 1, 2)) {
                for (i in 1:(DIMY-DIMYBRICK+1)) {  # Y axis (rows)
                    for (j in 1:(DIMX-DIMXBRICK+1)) {  # X axis (cols)
                        imax=i+DIMYBRICK-1
                        jmax=j+DIMXBRICK-1
                        if (sum(imgclust1[i:imax, j:jmax])==AREABRICK) {  # all 1's=brick match
                            imgout=drawbrick(imgout, j, jmax, i, imax,
                                             brick, BRICKSIZE, MIDGRAY, imgclust)
                            imgclust1[i:imax, j:jmax]=0  # remove drawn brick
                            NBRICKS=NBRICKS+1
                        }
                    }
                }
                DIMYBRICK=LEGOBRICKS[[l]][2]  # will run only for non-square bricks
                DIMXBRICK=LEGOBRICKS[[l]][1]
            }
        }
    }

    # writeTIFF(imgout, paste0(name[n], "_lego.tif"),
    #           bits.per.sample=16, compression="LZW")
    writePNG(imgout, paste0("LEGO_", name[n], ".png"))
    print(paste0(NBRICKS, " bricks used"))
}
