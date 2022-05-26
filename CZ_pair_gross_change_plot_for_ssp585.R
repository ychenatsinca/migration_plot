# Date: 2018-03-08
# Author: Yi-Ying Chen
# Purpose: to get the gross land use change by comparing a pair of maps
library(maptools)
library(raster)
library(rgdal)


#pdf_fname=c(paste("LC_dynamics_",substr(x=inputfileA,start=nchar(inputfileA)-6,stop = nchar(inputfileA)-3),
#
#           "_", substr(x=inputfileB,start=nchar(inputfileB)-6,stop = nchar(inputfileB)-3),".pdf",sep=""))

#load the coastlines data by readOGR function from sp package
coastlines <- readOGR("./ne_110m_coastline_edit/ne_110m_coastline.shp")
library("sp")
# load the function
library("raster")
#setwd("C:/Rscripts/migration_global/")
source("src_function_ncdf4.R")

source("./src_function_ncdf4.R")

# array to raater funtion
fun_a2r <- function(input.arr,xmin=-180,xmax=180,ymin=-90.,ymax=90.)
{
  nx=nrow(input.arr)
  ny=ncol(input.arr)
  print(paste("nx:",nx,"ny:",ny,sep=" "))
  xmin=-180
  xmax=180.
  ymin=-90.
  ymax=90.
  out.grid <- raster(ncol=nx, nrow=ny)
  extent(out.grid) <- extent(xmin, xmax ,ymin,ymax)
  #assign values to raster obj
  tmp <- input.arr
  input.arr[1:(nx/2), ] <- tmp[((nx/2)+1):nx,]
  input.arr[((nx/2)+1):nx,] <- tmp[1:(nx/2), ]
  
  values(out.grid) <- c(input.arr[nx:1,])
  return(out.grid)
}

mask <- as.matrix(raster("./mask/land_frac.nc", varname="LANDFRAC_PFT"))
# land mask manipulation
mask[mask <= 0.5] <- NA
mask[mask > 0.5] <- 1.0
land_mask <- mask


#my.color for ploting
#my.color <- colorRampPalette(c("blue","lightblue","white","pink","red"))(32)
my.color <- colorRampPalette(
  c("cyan","cyan","cyan","blue","blue",
    "darkgreen","forestgreen",
    "springgreen3","springgreen1","plum",
    "tan4","tan1",
    "pink1",
    "gold","yellow","lightgoldenrod","wheat",
    "hotpink"))(18*7)

cz.color <- colorRampPalette(
  c("cyan","cyan","cyan","blue","blue",
    "darkgreen","forestgreen",
    "springgreen3","springgreen1","plum",
    "tan4","tan1",
    "pink1",
    "gold","yellow","lightgoldenrod","wheat",
    "hotpink"))(18)

ez.color <- colorRampPalette(
  c("cyan","blue",
    "springgreen4","springgreen1",
    "gold","hotpink"))(6)

ref.yr <- c("1950_2000")
exp.yr <- c("2010_2020", "2050_2060", "2090_2100")
exp.yr.txt <- c(" 2010s", "2050s", "2090s")



scenario <- c("ssp126","ssp245","ssp370","ssp585")
scenario.txt <- c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5")



pdf_fname=c(paste( "./pdf_files/","Gross_change.pdf",sep="") )
pdf(pdf_fname,width=10, height=6)
#pdf(pdf_file,width=12, height=10)
#par( oma=c(1,1,2,1), mar=c(3,2,5,2),plt = c(0.05,0.95,0.05,.95), mgp=c(1,1,0))

#layout(matrix(data=seq(1,24,1),nrow=4, ncol=6,  byrow=TRUE),
#       widths=c(1,1,1), heights=c(1,1,1))
#layout(matrix(data=c(1),nrow=1, ncol=1,  byrow=TRUE),
#       widths=c(1,1,1), heights=c(1,1,1))
#par(mfrow=c(4,6))


inputfileA <- c(paste("/lfs/home/ychen/TaiESM/CZ_10yrs/cz_nc_files/TaiESM1_histor_",  ref.yr,"_climate_zone.nc",sep=""))
LU_A <- as.matrix(raster(inputfileA, varname="CZ_index")) *mask


plot.gd <- fun_a2r(input.arr=t(LU_A[,ny:1]) )
leg.at  <-  c("2.5","4.5","6","7","8",
              "9","10","11","12",
              "13","14","15","16","17","18")
leg.txt <-  c("A","B","C","D","E",
              "F","G","H","I",
              "J","K","L","M","N","P")

par(oma=c(0,0,0,0), mar=c(0,0,0,0),plt = c(0.01,0.99,0.01,.99), mgp=c(1,1,0))
plot(plot.gd,  ylim=c(-65,90),xlim=c(-180,180), xaxt="n", yaxt="n",
     # main="Daily mean bias of Growing Degree-Days (oC)",
     xlab="", ylab="", col=cz.color,box=FALSE,axes=FALSE,
     smallplot=c(0.05,0.9, 0.1, 0.12), horizontal=T,
     axis.args=list(at=leg.at, labels=leg.txt, cex.axis=1.0),
     legend.args=list(text= "Historial Climate Zone (1950 to 2000)" , side=3, line=.1, cex=1.5))

#add coastline
lines(coastlines, lwd=1.5, add=T)
#


par(oma=c(1,0,1,0), mar=c(5,0,5,0),plt = c(0.01,0.99,0.01,.99), mgp=c(1,1,0), new=FALSE)
layout(matrix(data=seq(1,24,1),nrow=4, ncol=6,  byrow=TRUE),
       widths=c(1,1,1), heights=c(1,1,1))
#
# get  nc file for migration plot
for (isc in 1:4) {
for (jyr in 1:3) {


inputfileB <- c(paste("/lfs/home/ychen/TaiESM/CZ_10yrs/cz_nc_files/TaiESM1_",scenario[isc],"_",exp.yr[jyr],"_climate_zone.nc",sep=""))
LU_B <- as.matrix(raster(inputfileB, varname="CZ_index")) *mask

nx=192; ny=288
# open device as pdf file 
#pdf(pdf_fname,width=8, height=10)
#par(xpd=TRUE)
#text(x=-210,y=105, labels="a",cex=1.2, font=2)

 # diff array calculation
  DIF_LU <- (LU_B-LU_A)
  #
  # calculate the gross change matrix
  # Forest(1) Agriculture(4) Grass(2) Water(13) City/BareSoil(8)
  gross_MATRIX <- array(0, dim=c(18,18)) 
  # 
  #     Forest Agri  Grass   Water  City 
  #       
  # F     0     -3     -1     -12    -7
  # A     3      0      2      -9    -4
  # G     1     -2      0     -11    -6
  # W    12      9     11       0     5 
  # C     7      4      6      -5     0
  
  id <- seq(1, 18, 1)
  for (i in 1:18) {
    for (j in 1:18) {
      # change pixcel information
      dif_value <- (id[i]-id[j])
      org_value <- id[j]
      #print(paste("org:",org_value, "dif:",dif_value,sep=""))
      gross_MATRIX[j,i] <- length(DIF_LU[which(DIF_LU == dif_value & LU_A == org_value )]) 
      #if(i==j) gross_MATRIX[j,i]<- 0
    }
  }
  
  
  gross_MATRIX


#create gross change mask
gc_mask <- DIF_LU
gc_mask[gc_mask==0] <- NA
gc_mask[gc_mask!=0] <- 1
gc_mask <- gc_mask*LU_B

gc_mask.rst <- fun_a2r(input.arr=t(gc_mask[,ny:1]) )
#
gc_poly <- rasterToPolygons(gc_mask.rst, dissolve=TRUE)
#

ld_go <- TRUE

if( ld_go) {

# apply gc_mask to show the hot spot
#LU_B <- LU_B*gc_mask

plot.gd <- fun_a2r(input.arr=t(LU_B[,ny:1]) )

leg.at  <-  c("2.5","4.5","6","7","8",
              "9","10","11","12",
              "13","14","15","16","17","18")
leg.txt <-  c("A","B","C","D","E",
              "F","G","H","I",
              "J","K","L","M","N","P")

if ((isc==5) & (jyr==5) ) {
#plot(x=0,y=1,xaxt="n", yaxt="n",type="n")
#plot(coastlines,col="white", lwd=0.1, ylim=c(-56,90),xlim=(-180,180),add=T)
#
plot(plot.gd,  ylim=c(-65,90),xlim=c(-180,180), xaxt="n", yaxt="n",
     # main="Daily mean bias of Growing Degree-Days (oC)",
     xlab="", ylab="", col=cz.color,box=FALSE,axes=FALSE,
     smallplot=c(0.05,0.9, 0.2, 0.24), horizontal=T,
     axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.4),
     legend.args=list(text= "Climate Zone" , side=3, line=.1, cex=0.5))
}else{
#par(xpd=FALSE)

#start plot 

plot(plot.gd,  ylim=c(-65,85),xlim=c(-150,160), xaxt='n', yaxt='n',legend=FALSE,
     # main="Daily mean bias of Growing Degree-Days (oC)",
     # xlab="", ylab="Latitude", col=wes_palette("Zissou1", 50, type = "continuous"),
     xlab="", ylab="", col=cz.color, box=FALSE, axes=FALSE)#,
     #smallplot=c(0.05,.95, 0.2, 0.24), horizontal=T,
     #axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.4,line=0.25, gap.axis=0.0),
     #legend.args=list(text= "Climate Zone" , side=3, line=0.1, cex=0.5))
}

#add coastline
lines(coastlines, lwd=0.5, add=T)
#add gross change location
plot(gc_poly,lwd=0.1,col=NA,add=T,border="lightgray")

#degs.nlat <- seq(15,90, 30)
#axis(side = 2, at = degs.nlat, srt=0, las=1,
#     labels = paste0(degs.nlat,"?X","N") , tck = -0.02)
#degs.slat <- seq(-15,-90, -30)
#axis(side = 2, at = degs.slat, srt=0, las=1,
#     labels = paste0(degs.slat*-1,"?X","S") , tck = -0.02)
#degs.elon <- seq(45,180, 45)
#axis(side = 1, at = degs.elon, srt=0, las=1,
#     labels = paste0(degs.elon,"?X","E") , tck = -0.02)
#axis(side = 1, at = 0, srt=0, las=1,
#     labels = c("0") , tck = -0.02)
#degs.wlon <- seq(-45,-180, -45)
#axis(side = 1, at = degs.wlon, srt=0, las=1,
#     labels = paste0(degs.wlon*-1,"?X","W") , tck = -0.02)
  mtext(paste(scenario.txt[isc],exp.yr.txt[jyr],sep=" "),side=3,line=-2,cex=0.5)
} #ld_go


if (jyr <= 3) {
# get the gross change map 


library("circlize")
#pdf_fname=c(paste( "./pdf_files/","Gross_change",scenario[j],ref.yr,"_",exp.yr,".pdf",sep="") )
#pdf(pdf_file,width=12, height=10)
#layout(matrix(data=c(1,2,4,3),nrow=2, ncol=2,  byrow=TRUE),
#       widths=c(1,1,1), heights=c(1,1,1))
#layout(matrix(data=c(1),nrow=1, ncol=1,  byrow=TRUE),
#       widths=c(1,1,1), heights=c(1,1,1))
#par( oma=c(0,0,0,0), mar=c(4,4,4,4),plt = c(0.025,0.99,0.025,.99))
#par(xpd=TRUE)


  mat <- gross_MATRIX
 cz_name <-  c("A1","A2","A3","B1","B2","C","D","E",
              "F","G","H","I",
              "J","K","L","M","N","P")

 
  
#  cz_name <-c("Artic1","Artic2","Artic3","Bol","Bol/Alp",
#              "Ext.Cd.wt","Ext.Cd.mc","Cd.mc","Cl.Tmp.xec","Cl.Tmp.mi",
#              "Wm.Tmp.mc","Wm.Tmp.xc","Sub.Trp.","Hot.dy","Hot.ad",
#              "Ext.Hot.ad","Ext.Hot.xc","Trp")
  # the col and raw names
  rownames(mat) = cz_name 
  colnames(mat) = cz_name
  # try to have a circle plot
  #chordDiagram(mat, grid.col = 1:5, directional = 1, row.col = 1:5)
  
  
  #output the gross matrix data to a data.frame and save as a csv.file
  df_matrix <- data.frame()  
  df_matrix <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("From", "To", "Area") )
  
  for (i in 1:length(rownames(mat)) ) {
    for (j in 1:length(colnames(mat)) ) {
      # 
      newRow <- data.frame(From=colnames(mat)[i],To=rownames(mat)[j],Area=mat[i,j] ) 
      df_matrix <- rbind(df_matrix, newRow)
      print(df_matrix)
    } # end of j
  } # end of i
  
  write.table(x=df_matrix, 
              file="gross_exchange_data.csv",
              sep=",",
              row.names =FALSE,
              col.names=TRUE) 
  
  #read in the data.frame for both gross change and color information
  df0 <- read.csv(file = "gross_exchange_data.csv", stringsAsFactors=FALSE)
#  df1 <- read.csv(file = "gross_exchange_color.csv", stringsAsFactors=FALSE)
  
  df1<-data.frame(region=cz_name, order1=seq(1,18,1), col1=cz.color,reg1=cz_name,reg1=cz_name)
 
# start plot
#  par( oma=c(0,0,0,0), mar=c(3,2,5,2),plt = c(0.01,0.99,0.01,.99), mgp=c(1,1,0), new=FALSE)
 # par(oma=c(0,0,0,0), mar=c(3,2,5,2),plt = c(0.01,0.99,0.01,.99), mgp=c(1,1,0),new=FALSE)

  circos.par(start.degree = 120, clock.wise = TRUE)
  chordDiagram(x = df0, transparency = 0.15, grid.col = cz.color,
               directional = 1 ,   direction.type = c("arrows", "diffHeight"), diffHeight  = 0.1,
               annotationTrack = "grid", annotationTrackHeight = c(0.01, 0.025),
               link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
  #title(paste("Hitorial mean to ",exp.yr[jyr],sep=""))
  # add text and axis
  circos.trackPlotRegion(
    track.index = 1, 
    bg.border = NA, 
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      sector.index = get.cell.meta.data("sector.index")
      reg1 = df1$reg1[df1$region == sector.index]
      reg2 = df1$reg2[df1$region == sector.index]
      circos.text(x = mean(xlim), y =30, 
                  labels = reg1, facing = "bending", cex = 0.5)
      # circos.text(x = mean(xlim), y = 3.1,  labels = reg2, facing = "bending", cex = 1.2)
      circos.axis(h = "top", major.at = seq(from = 0, to = xlim[2], by = 750), 
                  minor.ticks = 25, major.tick.percentage =50,labels.niceFacing = TRUE,labels.cex = 0.25)
    })
  circos.clear()  
}# jyr ld 
  
#par(new=FALSE)
#par(oma=c(0,0,0,0), mar=c(3,2,5,2),plt = c(0.01,0.99,0.01,.99), mgp=c(1,1,0))


# show the plot
#plot(region_mask, ylim=c(21.5,25.5))

#par(new=FALSE)


} #end of ic
} #end of jyr

# close the device
dev.off()


