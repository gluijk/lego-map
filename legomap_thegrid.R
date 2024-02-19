# Lego maps
# www.overfitting.net
# https://www.overfitting.net/2024/02/mapas-estilo-lego-con-r.html


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

# Array downsample function
# that allows to specify a background colour to be ignored in the resampling
# by not participating in the colour averaging
arraydownsample=function(img, DIMX=0, DIMY=0,
                         background=FALSE, backgroundcolour=c(0, 0, 0)) {

    DIMYorg=nrow(img)
    DIMXorg=ncol(img)
    if (!DIMX) DIMX=round(DIMY*DIMXorg/DIMYorg) else if (!DIMY) DIMY=round(DIMX*DIMYorg/DIMXorg)    

    imgout=array(0, c(DIMY, DIMX, 3))
    backgroundcolour=backgroundcolour/255  # convert 0..255 range to 0..1
    for (i in 1:DIMY) {
        imin=round(DIMYorg/DIMY*(i-1)+1)
        imax=round(DIMYorg/DIMY*i)
        for (j in 1:DIMX) {
            jmin=round(DIMXorg/DIMX*(j-1)+1)
            jmax=round(DIMXorg/DIMX*j)
            crop=img[imin:imax, jmin:jmax, ]
            AREA=(jmax-jmin+1)*(imax-imin+1)  # dim(crop)[1]*dim(crop)[2]
            r=crop[,,1]
            g=crop[,,2]
            b=crop[,,3]
            bgd=which(r==backgroundcolour[1] &
                      g==backgroundcolour[2] &
                      b==backgroundcolour[3])
            if (length(bgd)>AREA/2) {  # more than half the area is background
                for (chan in 1:3) imgout[i, j, chan]=backgroundcolour[chan]
            } else {
                if (length(bgd)) {  # some background in crop -> ignore it
                    imgout[i, j, 1]=mean(r[-bgd])
                    imgout[i, j, 2]=mean(g[-bgd])
                    imgout[i, j, 3]=mean(b[-bgd])
                } else {  # no background in crop -> average whole crop
                    imgout[i, j, 1]=mean(r)
                    imgout[i, j, 2]=mean(g)
                    imgout[i, j, 3]=mean(b)
                }
            }
        }
    }

    return (imgout)
}


# Function to draw an arbitrarily sized brick with borders
drawbrick=function(img,
                   ymin, ymax, xmin, xmax,
                   brick, BRICKSIZE, colgamma) {
    DARK=0.3
    LIGHT=0.7
    
    imgout=img  # creating a local variable is faster
    # Build base grayscale brick
    for (i in ymin:ymax) {
        for (j in xmin:xmax)
            imgout[((i-1)*BRICKSIZE+1):(i*BRICKSIZE),
                   ((j-1)*BRICKSIZE+1):(j*BRICKSIZE),]=replicate(3, brick)
    }
    
    # Brick limits in imgout
    rangeymin = (ymin-1)*BRICKSIZE+1
    rangeymax = ymax*BRICKSIZE
    rangexmin = (xmin-1)*BRICKSIZE+1
    rangexmax = xmax*BRICKSIZE
    
    # Draw pseudo 3D borders (1px width)
    imgout[rangeymin, rangexmin:rangexmax,]=LIGHT
    imgout[rangeymin:rangeymax, rangexmax:rangexmax,]=DARK
    imgout[rangeymax:rangeymax, rangexmin:rangexmax,]=DARK
    imgout[rangeymin:rangeymax, rangexmin:rangexmin,]=LIGHT

    # Colour brick according to imgcolour
    for (chan in 1:3) imgout[rangeymin:rangeymax, rangexmin:rangexmax, chan]=
        imgout[rangeymin:rangeymax, rangexmin:rangexmax, chan]^(1/colgamma[chan])
    
    return(imgout)
}


# Functions to write numbers in Inventory

NewBitmap = function(dimx, dimy, val=0) {
    # Crea bitmap de dimensiones dimx y dimy
    return(array(val,c(dimx,dimy)))
}

# Por Carlos Gil Bellosta
indices.drawline = function(x0, y0, x1, y1) {
    x0=round(x0)
    x1=round(x1)
    y0=round(y0)
    y1=round(y1)
    
    if (y0 == y1) return(cbind(x0:x1, y0)) # Recta de m=0 o un punto
    if (abs(x1 - x0) >= abs(y1 - y0)) { # Recta de 0 < |m| <= 1
        m = (y1 - y0) / (x1 - x0)
        cbind(x0:x1, round(y0 + m * ((x0:x1) - x0)))
    } else indices.drawline(y0, x0, y1, x1)[, 2:1]  # Recta de |m| > 1
    # Llamada traspuesta recursiva y traspuesta
}

DrawLine = function(img, x0, y0, x1, y1, inc=TRUE, val=1) {
    # Dibuja recta desde (x0,y0)-(x1,y1)
    # Por defecto método no destructivo y con valor=1
    indices=indices.drawline(x0, y0, x1, y1)
    if (inc) img[indices]=img[indices]+val
    else img[indices]=val
    
    return(img)
}

DrawRect = function(img, x0, y0, x1, y1, inc=TRUE, val=1, fill=FALSE) {
    # Dibuja rectángulo (x0,y0)-(x1,y1)
    # Por defecto método no destructivo, con valor=1 y sin relleno
    x0=round(x0)
    x1=round(x1)
    y0=round(y0)
    y1=round(y1)
    
    if (fill) {
        if (inc) img[x0:x1,y0:y1]=img[x0:x1,y0:y1]+val
        else img[x0:x1,y0:y1]=val
        
        return(img)
    } else {
        indices=which( ( (row(img)==x0         | row(img)==x1        ) &
                             (col(img)>=min(y0,y1) & col(img)<=max(y0,y1)) ) |
                           ( (col(img)==y0         | col(img)==y1        ) &
                                 (row(img)>=min(x0,x1) & row(img)<=max(x0,x1)) ) )
        if (inc) img[indices]=img[indices]+val
        else img[indices]=val
        
        return(img)
    }
}

DibujarNumero = function(img, x0, y0, inc=FALSE, val=1, fill=FALSE,
                         num, width, height) {
    # Dibuja cifra 0-9 en (x0,y0)
    # Por defecto método no destructivo y con valor=1
    
    if (num=='0') { 
        img=DrawRect(img, x0, y0, x0+width, y0-height, inc, val, fill)
    } else if (num=='1') {
        img=DrawLine(img, x0+width/2, y0, x0+width/2, y0-height, inc, val)
    } else if (num=='2') {
        img=DrawLine(img, x0, y0, x0+width, y0, inc, val)
        img=DrawLine(img, x0+width, y0, x0+width, y0-height/2, inc, val)
        img=DrawLine(img, x0+width, y0-height/2, x0, y0-height/2, inc, val)
        img=DrawLine(img, x0, y0-height/2, x0, y0-height, inc, val)
        img=DrawLine(img, x0, y0-height, x0+width, y0-height, inc, val)
    } else if (num=='3') {
        img=DrawLine(img, x0, y0, x0+width, y0, inc, val)
        img=DrawLine(img, x0, y0-height/2, x0+width, y0-height/2, inc, val)
        img=DrawLine(img, x0, y0-height, x0+width, y0-height, inc, val)
        img=DrawLine(img, x0+width, y0, x0+width, y0-height, inc, val)
    } else if (num=='4') {
        img=DrawLine(img, x0, y0, x0, y0-height/2, inc, val)
        img=DrawLine(img, x0, y0-height/2, x0+width, y0-height/2, inc, val)
        img=DrawLine(img, x0+width, y0, x0+width, y0-height, inc, val)
    } else if (num=='5') {
        img=DrawLine(img, x0+width, y0, x0, y0, inc, val)
        img=DrawLine(img, x0, y0, x0, y0-height/2, inc, val)
        img=DrawLine(img, x0, y0-height/2, x0+width, y0-height/2, inc, val)
        img=DrawLine(img, x0+width, y0-height/2, x0+width, y0-height, inc, val)
        img=DrawLine(img, x0+width, y0-height, x0, y0-height, inc, val)
    } else if (num=='6') {
        img=DrawRect(img, x0, y0-height/2, x0+width, y0-height, inc, val, fill)
        img=DrawLine(img, x0, y0, x0+width, y0, inc, val)
        img=DrawLine(img, x0, y0, x0, y0-height/2, inc, val)
    } else if (num=='7') {
        img=DrawLine(img, x0, y0, x0+width, y0, inc, val)
        img=DrawLine(img, x0+width, y0, x0+width, y0-height, inc, val)
    } else if (num=='8') {
        img=DrawRect(img, x0, y0, x0+width, y0-height/2, inc, val, fill)
        img=DrawRect(img, x0, y0-height/2, x0+width, y0-height, inc, val, fill)
    } else if (num=='9') {
        img=DrawRect(img, x0, y0, x0+width, y0-height/2, inc, val, fill)
        img=DrawLine(img, x0+width, y0-height/2, x0+width, y0-height, inc, val)
        img=DrawLine(img, x0, y0-height, x0+width, y0-height, inc, val)
    } else if (num=='-') {
        img=DrawLine(img, x0, y0-height/2, x0+width, y0-height/2, inc, val)
    } else if (num=='x') {
        img=DrawLine(img, x0, y0-height, x0+width, y0-height/2, inc, val)
        img=DrawLine(img, x0, y0-height/2, x0+width, y0-height, inc, val)
    } else {
        return(img)  # Cifra inválida
    }
    
    return(img)
}


###########################################################

legomap = function(img, name, k=8,
                   LEGOBRICKS=list(c(8,8), c(6,6), c(4,6), c(4,4), c(2,4),
                                   c(2,3), c(2,2), c(1,4), c(1,3), c(1,2),
                                   c(1,1)),  # hierarchical list
                   resize=TRUE, LEGOSIZEX=0, LEGOSIZEY=0,
                   background=FALSE, backgroundcolour=c(0, 0, 0),
                   randomcolours=FALSE) {
    # img: DIMY x DIMX x 3 array containing an RGB image
    # name: output PNG files will be created with this name
    # k: number of colours in the clustering (including background if exists)
    #    if k=0 no clustering is applied and gradients are preserved
    # resize: bool to indicate that image must be rescaled
    # LEGOSIZEX/LEGOSIZEY: output size in 1x1 LEGO bricks
    #     if any of them is 0, aspect ratio is preserved using the other one
    # background: bool to indicate that there is a background colour that
    #     must be ignored in the rescaling so it doesn't adulterate borders
    # backgroundcolour: the EXACT RGB colour to be isolated (0..255 int scale)
    # randomcolours: randomize clustering colours instead of using centroids

    
    require(png)  # read/save 8-bit PNG's
    require(terra)  # resample
    
    # brick=readPNG("legobrick46x46.png")  #  46x46 pixels grayscale bitmap
    brick=readPNG("tronbrick24x24.png")  #  24x24 pixels grayscale bitmap
    BRICKSIZE=nrow(brick)
    MIDGRAY=median(brick)  # median should be ~0.5 (8-bit 128/255)
    # print(paste0("Median of brick vs 128/255: ", MIDGRAY, " vs ", 128/255))

    
    # Pipeline:
    # img (input) -> imglite (downsized) -> imgclust (clustered) -> imgout (LEGO)
    
    ################################
    # 1. DOWNSIZE IMAGE
    
    # Resize image
    if (resize) {
        # print(paste0("Resizing '", name, "'..."))
        imglite=arraydownsample(img, LEGOSIZEX, LEGOSIZEY,
                                background, backgroundcolour)
    } else imglite=img  # no size change
    
    DIMY=nrow(imglite)
    DIMX=ncol(imglite)
    # writePNG(imglite, paste0(name, "_lite.png"))
    
    # Identify background
    if (background) {
        # print(paste0("Processing background on '", name, "'..."))
        dim(imglite)=c(DIMY*DIMX, 3)  # redim to DIMY*DIMX x 3 array (RGB list)
        imglitebackround=imglite*0
        for (i in 1:nrow(imglite))
            if (identical(imglite[i,], backgroundcolour/255)) imglitebackround[i,]=1
        
        dim(imglite)=c(DIMY, DIMX, 3)  # restore DIMY x DIMX x 3 array (RGB image)
        dim(imglitebackround)=dim(imglite)
        
        # Store background pixel locations separately
        # writePNG(imglitebackround, paste0(name, "_lite_bgd.png"))
        imgbackround=arrayresample(imglitebackround, method='near',
                                   DIMX*BRICKSIZE, DIMY*BRICKSIZE)
    }

    
    ################################
    # 2. K-MEANS CLUSTERING
 
    NCOLOURS=k  # k clusters
    
    # print(paste0("Clustering (k=", k, ") '", name, "'..."))
    
    # Rearrange imglite as a N x 3 array with RGB values in 3 columns
    M=cbind(c(imglite[,,1]), c(imglite[,,2]), c(imglite[,,3]))

    M2=M*0
    for (i in 1:nrow(M))
        if (!identical(M[i,], backgroundcolour/255)) M2[i,]=1

    # Standard k-means clustering
    set.seed(0)  # reproducible clustering
    colnames(M)=c("R", "G", "B")
    M3=M[
        M[,1]!=backgroundcolour[1]/255 |
        M[,2]!=backgroundcolour[2]/255 |
        M[,3]!=backgroundcolour[3]/255]
    dim(M3)=c(length(M3)/3,3)
    
    colnames(M3)=c("R", "G", "B")
    kmeansfit=kmeans(subset(M3, select=c("R", "G", "B")), centers=NCOLOURS,
                     nstart=2000, iter.max=1000)  # high nstart can prevent from
    clustering=kmeansfit$cluster           # missing the tiniest clusters
    centers=kmeansfit$centers  # clustering centroids (average colours)

    # Build clustered coloured image
    imgclust=array(0, c(DIMY*DIMX, 3))  # configure DIMY*DIMX x 3 array
    iPixel=1
    for (i in 1:nrow(M)) {
        if (M2[i,1]) {
            for (chan in 1:3) imgclust[i, chan]=centers[clustering[iPixel], chan]
            iPixel=iPixel+1
        } else for (chan in 1:3) imgclust[i, chan]=backgroundcolour[chan]/255
    }
    dim(imgclust)=c(DIMY, DIMX, 3)  # redim to DIMY x DIMX x 3 array (RGB image)
    
    # writePNG(imgclust, paste0(name, "_cluster.png"))


    ################################
    # 3. BUILD OUTPUT IMAGE WITH LEGO BRICKS
    
    # print(paste0("Building LEGO '", name, "'..."))
    
    SAFE=0.05  # margin from 0/1 to prevent colour clipping
    imgout=array(0, c(DIMY*BRICKSIZE, DIMX*BRICKSIZE, 3))
    
    imgclust[imgclust > 1-SAFE]=1-SAFE  # clip highlights to prevent whitening
    imgclust[imgclust <   SAFE]=SAFE  # clip shadows to prevent blackening
    colgamma=array(0, c(NCOLOURS, 3))  # gamma that will produce each channel's colour
    for (k in 1:NCOLOURS) {
        colk=centers[k,]
        colk[colk > 1-SAFE]=1-SAFE
        colk[colk <   SAFE]=SAFE
        for (chan in 1:3) colgamma[k, chan]=1/(log(colk[chan])/log(MIDGRAY))
    }
    
    # Brute force LEGO brick fitting algorithm
    NSIZES=length(LEGOBRICKS)
    # Order bricks to draw them horizontally in the Inventory
    for (size in 1:NSIZES) {
        if (LEGOBRICKS[[size]][1]>LEGOBRICKS[[size]][2]) {
            tmp=LEGOBRICKS[[size]][1]
            LEGOBRICKS[[size]][1]=LEGOBRICKS[[size]][2]
            LEGOBRICKS[[size]][2]=tmp
        }
    }
    
    Inventory=array(0, c(NCOLOURS, NSIZES))  # how many bricks of each colour and size
    for (k in 1:NCOLOURS) {  # loop trough clusters
        # indices=which(clustering==k)
        # imgclust1=array(0, c(DIMY, DIMX))
        # imgclust1[indices]=1  # set to 1 pixels belonging to cluster k
        
        iPixel=1
        dim(imgclust)=c(DIMY*DIMX, 3)  # redim to DIMY x DIMX x 3 array (RGB image)
        imgclust1=array(0, c(DIMY*DIMX))
        for (i in 1:nrow(M)) {
            if (M2[i,1]) {
                if (clustering[iPixel]==k) imgclust1[i]=1
                iPixel=iPixel+1
            }
        }
        dim(imgclust)=c(DIMY, DIMX, 3)  # redim to DIMY x DIMX x 3 array (RGB image)
        dim(imgclust1)=c(DIMY, DIMX)  # redim to DIMY x DIMX x 3 array (RGB image)
        
        for (size in 1:NSIZES) {  # loop trough brick sizes
            DIMYBRICK=LEGOBRICKS[[size]][1]
            DIMXBRICK=LEGOBRICKS[[size]][2]
            AREABRICK=DIMXBRICK*DIMYBRICK
            
            # Try both 0º and 90º rotation on non-square bricks
            NORIENTATIONS=ifelse(DIMYBRICK==DIMXBRICK, 1, 2)
            for (orientation in 1:NORIENTATIONS) {
                for (i in 1:(DIMY-DIMYBRICK+1)) {  # Y axis (rows)
                    for (j in 1:(DIMX-DIMXBRICK+1)) {  # X axis (cols)
                        imax=i+DIMYBRICK-1
                        jmax=j+DIMXBRICK-1
                        if (sum(imgclust1[i:imax, j:jmax])==AREABRICK) {  # all 1's=brick match
                            imgout=drawbrick(imgout,
                                             i, imax, j, jmax, 
                                             brick, BRICKSIZE, colgamma[k,])
                            imgclust1[i:imax, j:jmax]=0  # remove drawn brick
                            Inventory[k, size]=Inventory[k, size]+1
                        }
                    }
                }
                DIMYBRICK=LEGOBRICKS[[size]][2]  # will run only for non-square bricks
                DIMXBRICK=LEGOBRICKS[[size]][1]
            }
        }
    }
    NBRICKS=sum(Inventory)  # bricks used
    # print(paste0(NBRICKS, " bricks used"))
    

    # Restore colour in (non-LEGO) background
    if (background) {
        for (chan in 1:3) {
            indices=which(imgbackround[,,chan]==1)
            imgout[,,chan][indices]=backgroundcolour[chan]/255        
        }
    }

    # writePNG(imgout, paste0(name, "_LEGO.png"))
    
    return(imgout)
}




# Examples
img=readPNG("moto.png")
inventory=legomap(img, 'moto', k=30,
          resize=TRUE, LEGOSIZEY=45,
          background=TRUE, backgroundcolour=c(0, 0, 0))


# General animation parameters
DIMY=1080  # Full HD resolution
DIMX=1920

BRICKSIZE=24  # 1x1 brick size used
LEGOSIZEYMIN=1  # LEGOSIZEY: 1..45
LEGOSIZEYMAX=45


#########################
# 1/4: Blank beginning

NFRAMES=222
Offset=0

imgout=array(0, c(DIMY, DIMX, 3))  # blank Full HD RGB image
for (frame in 0:(NFRAMES-1)) {
    writePNG(imgout, paste0("thegrid_", ifelse(frame+Offset<10, "000",
        ifelse(frame+Offset<100, "00",
        ifelse(frame+Offset<1000, "0", ""))), frame+Offset, ".png"))
}


#########################
# 2/4: Grid generation

NFRAMES=894
Offset=222

NHOR=DIMY/BRICKSIZE-1
NVER=DIMX/BRICKSIZE-1
    
for (frame in 0:(NFRAMES-1)) {
    imgout=array(0, c(DIMY, DIMX, 3))  # blank Full HD RGB image
    
    NHORlines=NHOR/(NFRAMES-1)*frame
    NHORlinesC=floor(NHORlines)
    NVERlines=NVER/(NFRAMES-1)*frame
    NVERlinesC=floor(NVERlines)
    
    i=0
    j=0
    if (NHORlinesC) for (i in 1:NHORlinesC) imgout[i*BRICKSIZE, 1:DIMX,]=imgout[i*BRICKSIZE, 1:DIMX,]+0.5
    if (NVERlinesC) for (j in 1:NVERlinesC) imgout[1:DIMY, j*BRICKSIZE,]=imgout[1:DIMY, j*BRICKSIZE,]+0.5
    
    HORlinF=round((NHORlines-NHORlinesC)*DIMX)  # fraction of horizontal line to plot
    VERlinF=round((NVERlines-NVERlinesC)*DIMY)  # fraction of vertical line to plot

    if (HORlinF) imgout[(i+1)*BRICKSIZE, 1:HORlinF,]=imgout[(i+1)*BRICKSIZE, 1:HORlinF,]+0.5
    if (VERlinF) imgout[1:VERlinF, (j+1)*BRICKSIZE,]=imgout[1:VERlinF, (j+1)*BRICKSIZE,]+0.5
    
    imgout=imgout^2  # enhance crossings
    imgout=imgout*0.8  # bluish colour
    imgout[,,1]=0  # R=0
    
    writePNG(imgout, paste0("thegrid_", ifelse(frame+Offset<10, "000",
                    ifelse(frame+Offset<100, "00",
                    ifelse(frame+Offset<1000, "0", ""))), frame+Offset, ".png"))
}

imggrid=imgout  # keep entire grid


#########################
# 3/4: Clustered animation

NFRAMES=818
Offset=1116

kMIN=1  # k: 2..20
kMAX=20
gammak1=0.7  # upper gamma envelope for k
gammak2=0.4  # loer gamma envelope for k
N=15  # number of periods for k oscillations

# img=readPNG("guillermo225.png")
img=readPNG("lauratron.png")
kpre=-1
LEGOSIZEYpre=-1
for (frame in 0:(NFRAMES-1)) {
    # k=round( (kMAX-kMIN) * (frame/(NFRAMES-1))^(1/gammak) + kMIN)
    
    exp1=((frame/(NFRAMES-1))^(1/gammak1))
    exp2=((frame/(NFRAMES-1))^(1/gammak2))
    expfinal=(exp1-exp2)/2*(sin(2*pi*N*frame/(NFRAMES-1))+1) + exp2
    k=round( (kMAX-kMIN) * expfinal + kMIN)

    LEGOSIZEY=(LEGOSIZEYMAX-LEGOSIZEYMIN)/(NFRAMES-1)*frame+LEGOSIZEYMIN
    LEGOSIZEY=floor((LEGOSIZEY+1)/2)*2-1  # only odd values
    if (k>LEGOSIZEY^2) k=LEGOSIZEY^2  # in case more clusters than pixels
    
    if ((LEGOSIZEY != LEGOSIZEYpre) | (k != kpre)) {  # any changes in frame?
        print(paste0("Frame ", frame, ": k=", k, ", SIZE=", LEGOSIZEY))

        if (LEGOSIZEY==1) { LEGOBRICKS=list(c(1,1))
        } else if (LEGOSIZEY==2) { LEGOBRICKS=list(c(2,2), c(1,2), c(1,1))
        } else if (LEGOSIZEY==3) { LEGOBRICKS=list(c(2,3), c(2,2), c(1,3), c(1,2), c(1,1))
        } else if (LEGOSIZEY==4 | LEGOSIZEY==5) { LEGOBRICKS=list(c(4,4), c(2,4), c(2,3), c(2,2), c(1,4), c(1,3), c(1,2), c(1,1))
        } else if (LEGOSIZEY==6 | LEGOSIZEY==7) { LEGOBRICKS=list(c(6,6), c(4,6), c(4,4), c(2,4),
                                                   c(2,3), c(2,2), c(1,4), c(1,3), c(1,2),
                                                   c(1,1))
        } else LEGOBRICKS=list(c(8,8), c(6,6), c(4,6), c(4,4), c(2,4),
                               c(2,3), c(2,2), c(1,4), c(1,3), c(1,2),
                               c(1,1))    

        imgframe=legomap(img, 'thegrid', k=k, LEGOBRICKS=LEGOBRICKS,
                         resize=TRUE, LEGOSIZEY=LEGOSIZEY,
                         background=TRUE, backgroundcolour=c(0, 0, 0))
        DIMYframe=nrow(imgframe)
        DIMXframe=ncol(imgframe)
        NY=floor((DIMY/BRICKSIZE - DIMYframe/BRICKSIZE)/2)
        NX=floor((DIMX/BRICKSIZE - DIMXframe/BRICKSIZE)/2)  
        
        imgout=array(0, c(DIMY, DIMX, 3))  # blank Full HD RGB image
        imgout[(NY*BRICKSIZE+1):(NY*BRICKSIZE+DIMYframe),
               (NX*BRICKSIZE+1):(NX*BRICKSIZE+DIMXframe), ]=imgframe
        
        imgtmp=imgout[,,1]+imgout[,,2]+imgout[,,3]
        indices=which(imgtmp==0)  # no trace of image plotted
        for (chan in 1:3) imgout[,,chan][indices]=imggrid[,,chan][indices]

        kpre=k
        LEGOSIZEYpre=LEGOSIZEY
    }
    
    writePNG(imgout, paste0("thegrid_", ifelse(frame+Offset<10, "000",
                ifelse(frame+Offset<100, "00",
                ifelse(frame+Offset<1000, "0", ""))), frame+Offset, ".png"))
}

imglast=imgout  # keep last frame


#########################
# 4/4: Fade out

NFRAMES=373
Offset=1934

for (frame in 0:(NFRAMES-1)) {
    imgout=imglast*(1-frame/(NFRAMES-1))
    writePNG(imgout, paste0("thegrid_", ifelse(frame+Offset<10, "000",
        ifelse(frame+Offset<100, "00",
        ifelse(frame+Offset<1000, "0", ""))), frame+Offset, ".png"))
}




# Choose k gamma envelopes
gammak1=0.7
gammak2=0.4
N=15  # number of periods for k oscillations
kseries=c()
for (frame in 0:(NFRAMES-1)) {
    exp1=((frame/(NFRAMES-1))^(1/gammak1))
    exp2=((frame/(NFRAMES-1))^(1/gammak2))
    expfinal=(exp1-exp2)/2*(sin(2*pi*N*frame/(NFRAMES-1))+1)+exp2
    k=round( (kMAX-kMIN) * expfinal + kMIN)
    kseries=c(kseries, k)
}
plot(kseries, type='s')


# ffmpeg -framerate 24 -i thegrid_%4d.png -i tronlegacythegrid.wav
# -c:v libx264 -crf 18 -pix_fmt yuv420p thegrid.mp4
