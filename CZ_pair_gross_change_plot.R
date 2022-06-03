# Date: 2018-03-08
# Author: Yi-Ying Chen
# Purpose: to get the gross land use change by comparing a pair of maps
library(maptools)
library(raster)
library(rgdal)
library(ggpubr)

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

nx=192; ny=288
#
mask <- as.matrix(raster("./mask/land_frac.nc", varname="LANDFRAC_PFT"))
# land mask manipulation
mask[mask <= 0.5] <- NA
mask[mask > 0.5] <- 1.0
land_mask <- mask

area <- as.matrix(raster("./mask/area.nc", varname="AREA"))
land_area <- area/1000000.
#convert to 1000000 km^2

nav_lat <- as.matrix(raster("./mask/latxy_longxy.nc", varname="LATIXY"))
nav_lon <- as.matrix(raster("./mask/latxy_longxy.nc", varname="LONGXY"))


#
#my.color for ploting
#my.color <- colorRampPalette(c("blue","lightblue","white","pink","red"))(32)
my.color <- colorRampPalette(
  c("cyan","cyan","cyan","blue","blue",
    "darkgreen","green2",
    "springgreen3","springgreen1","plum",
    "tan4","tan1",
    "pink1",
    "gold","yellow","lightgoldenrod","wheat",
    "hotpink"))(18*7)

cz.color <- colorRampPalette(
  c("cyan","cyan","cyan","blue","blue",
    "darkgreen","green2",
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
exp.yr <- c("2010_2020","2020_2030","2030_2040","2040_2050","2050_2060","2060_2070","2070_2080","2080_2090","2090_2100")
#exp.yr.txt <- c( "2020-2040","2040-2060","2060-2090")
#exp.yr.txt <- c( "2020-2040","2020-2040","2020-2090")
exp.yr.txt <- c("toward 2030s","toward 2040s","toward 2050s","toward 2060s",
                "toward 2070s","toward 2080s","toward 2090s","toward 2100s")


scenario <- c("ssp126","ssp245","ssp370","ssp585")
scenario.txt <- c("(SSP126)","(SSP245)","(SSP370)","(SSP585)")

#scenario <- c("ssp585")
#scenario.txt <- c("SSP585")



#layout(matrix(data=seq(1,24,1),nrow=4, ncol=6,  byrow=TRUE),
#       widths=c(1,1,1), heights=c(1,1,1))
#layout(matrix(data=c(1),nrow=1, ncol=1,  byrow=TRUE),
#       widths=c(1,1,1), heights=c(1,1,1))
#par(mfrow=c(4,6))


inputfileA <- c(paste("/lfs/home/ychen/TaiESM/CZ_10yrs/cz_nc_files/TaiESM1_histor_",  ref.yr,"_climate_zone.nc",sep=""))
LU_A <- as.matrix(raster(inputfileA, varname="CZ_index")) *mask

# assign GDD zone to Alphabat 
GDD_Z <- LU_A
cz_name <-  c("A","B","C","D","E","F","G","H",
              "I","J","K","L",
              "M","N","O","P","Q","R")
GDD_Z[GDD_Z <= 0.] <- NA
for (iz in 1:18) {
GDD_Z[GDD_Z == iz] <- cz_name[iz]
}


#te a tmp table for the analysis
org.table <- data.frame()
#assign value into the dataframe
id=0
for (ix in 1:nx) {
   for (iy in 1:ny) {
     # assign  zonal type  
     if (!is.na(GDD_Z[ix,iy])) {
     id = id +1
     if (nav_lat[ix,iy] > 60.) {  
                                 tmp <- data.frame(zon = c("N65"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 60.)  & (nav_lat[ix,iy] > 50.) ){
                                 tmp <- data.frame(zon = c("N55"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 50.)  & (nav_lat[ix,iy] > 40.) ){
                                 tmp <- data.frame(zon = c("N45"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 40.)  & (nav_lat[ix,iy] > 30.) ){
                                 tmp <- data.frame(zon = c("N35"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 30.)  & (nav_lat[ix,iy] > 20.) ){
                                 tmp <- data.frame(zon = c("N25"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 20.)  & (nav_lat[ix,iy] > 10. ) ){
                                 tmp <- data.frame(zon = c("N15"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 10.)  & (nav_lat[ix,iy] > 0. ) ){
                                 tmp <- data.frame(zon = c("N05"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -10 ) & (nav_lat[ix,iy] <= 0. ) ) {
                                 tmp <- data.frame(zon = c("S05"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -20 ) & (nav_lat[ix,iy] <= 10. ) ) {
                                 tmp <- data.frame(zon = c("S15"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -30 ) & (nav_lat[ix,iy] <= 20. ) ) {
                                 tmp <- data.frame(zon = c("S25"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -40 ) & (nav_lat[ix,iy] <= 30. ) ) {
                                 tmp <- data.frame(zon = c("S35"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -50.) & (nav_lat[ix,iy] < -40.)) {
                                 tmp <- data.frame(zon = c("S45"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -60.) & (nav_lat[ix,iy] < -50.)) {
                                 tmp <- data.frame(zon = c("S55"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if (nav_lat[ix,iy] < -60. ) {
                                 tmp <- data.frame(zon = c("S65"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }
     # combine table  #   print(tmp) 
     org.table <- rbind(org.table,tmp)
     #print(tmp)
     }
   }
}
# aggregate by area in the tmp.table
cz_table_org <- aggregate(org.table$area, by=list(gdd=org.table$gdd,zon=org.table$zon ), FUN=sum)
colnames(cz_table_org) <- c("gdd","zon","area")


plot.gd <- fun_a2r(input.arr=t(LU_A[,ny:1]) )
leg.at  <-  c("2.5","4.5","6","7","8",
              "9","10","11","12",
              "13","14","15","16","17","18")
leg.txt <-  c("C","E","F","G","H",
              "I","J","K","L",
              "M","N","O","P","Q","R")

#First page plot for historical plot 
par(oma=c(0,0,0,0), mar=c(0,0,0,0),plt = c(0.01,0.99,0.01,.99), mgp=c(1,1,0))
plot(plot.gd,  ylim=c(-65,90),xlim=c(-180,180), xaxt="n", yaxt="n",
     # main="Daily mean bias of Growing Degree-Days (oC)",
     xlab="", ylab="", col=cz.color,box=FALSE,axes=FALSE,
     smallplot=c(0.05,0.9, 0.1, 0.12), horizontal=T,
     axis.args=list(at=leg.at, labels=leg.txt, cex.axis=1.0),
     legend.args=list(text= "Historial Climate Zone (1950 to 2000)" , side=3, line=.1, cex=1.5))

#add coastline
lines(coastlines, lwd=2.0, add=T)
#



irun=0
# get  nc file for migration plot
for (isc in 1:4) {

#open pdf 
pdf_fname=c(paste( "./pdf_files/","Gross_change_2020_2100(ref_hist)",scenario[isc],".pdf",sep="") )
pdf(pdf_fname,width=24, height=12)
#pdf(pdf_file,width=12, height=10)
#par( oma=c(1,1,2,1), mar=c(3,2,5,2),plt = c(0.05,0.95,0.05,.95), mgp=c(1,1,0))

par(oma=c(1,0,1,0), mar=c(5,0,5,0),plt = c(0.01,0.99,0.01,.99), mgp=c(1,1,0), new=FALSE)
layout(matrix(data=c(1,2,3,4,5,6,7,8),nrow=2, ncol=4,  byrow=TRUE),
       widths=c(1,1,1), heights=c(1,1,1))
#
irun=0
for (jyr in 1:8) {
irun = irun +1
#inputfileA <- c(paste("/lfs/home/ychen/TaiESM/CZ_10yrs/cz_nc_files/TaiESM1_",scenario[isc],"_",exp.yr[jyr],"_climate_zone.nc",sep=""))
#inputfileA <- c(paste("/lfs/home/ychen/TaiESM/CZ_10yrs/cz_nc_files/TaiESM1_",scenario[isc],"_","2020_2030","_climate_zone.nc",sep=""))
inputfileA <- c(paste("/lfs/home/ychen/TaiESM/CZ_10yrs/cz_nc_files/TaiESM1_histor_",  ref.yr,"_climate_zone.nc",sep=""))

LU_A <- as.matrix(raster(inputfileA, varname="CZ_index")) *mask

# assign GDD zone to Alphabat 
GDD_Z <- LU_A

GDD_Z[GDD_Z <= 0.] <- NA
for (iz in 1:18) {
GDD_Z[GDD_Z == iz] <- cz_name[iz]
}


#te a tmp table for the analysis
org.table <- data.frame()
#assign value into the dataframe
id=0
for (ix in 1:nx) {
   for (iy in 1:ny) {
     # assign  zonal type  
     if (!is.na(GDD_Z[ix,iy])) {
     id = id +1
     if (nav_lat[ix,iy] > 60.) {  
                                 tmp <- data.frame(zon = c("N65"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 60.)  & (nav_lat[ix,iy] > 50.) ){
                                 tmp <- data.frame(zon = c("N55"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 50.)  & (nav_lat[ix,iy] > 40.) ){
                                 tmp <- data.frame(zon = c("N45"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 40.)  & (nav_lat[ix,iy] > 30.) ){
                                 tmp <- data.frame(zon = c("N35"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 30.)  & (nav_lat[ix,iy] > 20.) ){
                                 tmp <- data.frame(zon = c("N25"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 20.)  & (nav_lat[ix,iy] > 10. ) ){
                                 tmp <- data.frame(zon = c("N15"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 10.)  & (nav_lat[ix,iy] > 0. ) ){
                                 tmp <- data.frame(zon = c("N05"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -10 ) & (nav_lat[ix,iy] <= 0. ) ) {
                                 tmp <- data.frame(zon = c("S05"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -20 ) & (nav_lat[ix,iy] <= 10. ) ) {
                                 tmp <- data.frame(zon = c("S15"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -30 ) & (nav_lat[ix,iy] <= 20. ) ) {
                                 tmp <- data.frame(zon = c("S25"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -40 ) & (nav_lat[ix,iy] <= 30. ) ) {
                                 tmp <- data.frame(zon = c("S35"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -50.) & (nav_lat[ix,iy] < -40.)) {
                                 tmp <- data.frame(zon = c("S45"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -60.) & (nav_lat[ix,iy] < -50.)) {
                                 tmp <- data.frame(zon = c("S55"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if (nav_lat[ix,iy] < -60. ) {
                                 tmp <- data.frame(zon = c("S65"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }
     # combine table  #   print(tmp) 
     org.table <- rbind(org.table,tmp)
     #print(tmp)
     }
   }
}
# aggregate by area in the tmp.table
cz_table_org <- aggregate(org.table$area, by=list(gdd=org.table$gdd,zon=org.table$zon ), FUN=sum)
colnames(cz_table_org) <- c("gdd","zon","area")

#print(cz_table_org)
#print(cz_table_org)
#only for the first plot
if (jyr==1) {
data1 <- cz_table_org                                                 # Replicate original data
data1$zon <- factor(data1$zon,                                    # Change ordering manually
                  levels = c("S65","S55","S45","S35","S25","S15","S05",
                             "N05","N15","N25","N35","N45","N55","N65"))
#
bar_plot <- ggplot(data1, aes(x=zon, y=area, fill=gdd)) + geom_bar(stat="identity") + coord_flip() + scale_fill_manual(
      values=  c("A"="cyan","B"="cyan","C"="cyan","D"="blue","E"="blue",
                 "F"="darkgreen","G"="green2","H"="springgreen3","I"="springgreen1","J"="plum",
                 "K"="tan4","L"="tan1","M"="pink1","N"="gold","O"="yellow","P"="lightgoldenrod","Q"="wheat","R"="hotpink"))


ylab.txt <- bquote(.(scenario.txt[isc])~.(exp.yr[jyr])~EnZ~(Mkm^2))
bar_plot <- bar_plot +  ylab(ylab.txt) + xlab("Latitude")
bar_plot <- bar_plot + theme( legend.position="none")
#assign to bar_plot_1
assign(paste("bar_plot_init_",isc,sep=""), bar_plot)
}

inputfileB <- c(paste("/lfs/home/ychen/TaiESM/CZ_10yrs/cz_nc_files/TaiESM1_",scenario[isc],"_",exp.yr[jyr+1],"_climate_zone.nc",sep=""))
LU_B <- as.matrix(raster(inputfileB, varname="CZ_index")) *mask

GDD_Z <- LU_B
# assign GDD zone to Alphabat 
GDD_Z[GDD_Z <= 0.] <- NA
for (iz in 1:18) {
GDD_Z[GDD_Z == iz] <- cz_name[iz]
}

#te a tmp table for the analysis
tmp.table <- data.frame()
#assign value into the dataframe
id=0
for (ix in 1:nx) {
   for (iy in 1:ny) {
     # assign  zonal type  
     if (!is.na(GDD_Z[ix,iy])) {
     id = id +1
     if (nav_lat[ix,iy] > 60.) {  
                                 tmp <- data.frame(zon = c("N65"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 60.)  & (nav_lat[ix,iy] > 50.) ){
                                 tmp <- data.frame(zon = c("N55"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 50.)  & (nav_lat[ix,iy] > 40.) ){
                                 tmp <- data.frame(zon = c("N45"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 40.)  & (nav_lat[ix,iy] > 30.) ){
                                 tmp <- data.frame(zon = c("N35"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 30.)  & (nav_lat[ix,iy] > 20.) ){
                                 tmp <- data.frame(zon = c("N25"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 20.)  & (nav_lat[ix,iy] > 10. ) ){
                                 tmp <- data.frame(zon = c("N15"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] <= 10.)  & (nav_lat[ix,iy] > 0. ) ){
                                 tmp <- data.frame(zon = c("N05"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -10 ) & (nav_lat[ix,iy] <= 0. ) ) {
                                 tmp <- data.frame(zon = c("S05"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -20 ) & (nav_lat[ix,iy] <= 10. ) ) {
                                 tmp <- data.frame(zon = c("S15"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -30 ) & (nav_lat[ix,iy] <= 20. ) ) {
                                 tmp <- data.frame(zon = c("S25"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -40 ) & (nav_lat[ix,iy] <= 30. ) ) {
                                 tmp <- data.frame(zon = c("S35"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -50.) & (nav_lat[ix,iy] < -40.)) {
                                 tmp <- data.frame(zon = c("S45"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if ((nav_lat[ix,iy] >= -60.) & (nav_lat[ix,iy] < -50.)) {
                                 tmp <- data.frame(zon = c("S55"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }else if (nav_lat[ix,iy] < -60. ) {
                                 tmp <- data.frame(zon = c("S65"), gdd = GDD_Z[ix,iy] , area = land_area[ix,iy])
     }
     # combine table  #   print(tmp) 
     tmp.table <- rbind(tmp.table,tmp)
     #print(tmp)
     }
   }
}
# aggregate by area in the tmp.table
cz_table <- aggregate(tmp.table$area, by=list(gdd=tmp.table$gdd,zon=tmp.table$zon ), FUN=sum)
colnames(cz_table) <- c("gdd","zon","area")

#print(cz_table)
if (jyr==8) {
data1 <- cz_table                                                # Replicate original data
data1$zon <- factor(data1$zon,                                    # Change ordering manually
                  levels = c("S65","S55","S45","S35","S25","S15","S05",
                             "N05","N15","N25","N35","N45","N55","N65"))
#
bar_plot <- ggplot(data1, aes(x=zon, y=area, fill=gdd)) + geom_bar(stat="identity") + coord_flip() + scale_fill_manual(
       values=  c("A"="cyan","B"="cyan","C"="cyan","D"="blue","E"="blue",
                 "F"="darkgreen","G"="green2","H"="springgreen3","I"="springgreen1","J"="plum",
                 "K"="tan4","L"="tan1","M"="pink1","N"="gold","O"="yellow","P"="lightgoldenrod","Q"="wheat","R"="hotpink"))

ylab.txt <- bquote(.(scenario.txt[isc])~.(exp.yr[jyr+1])~EnZ~(Mkm^2))
bar_plot <- bar_plot +  ylab(ylab.txt) 
bar_plot <- bar_plot + theme( axis.title.y=element_blank() )
bar_plot <- bar_plot + theme( legend.position="none")
#assign to bar_plot_1
assign(paste("bar_plot_end_",isc,sep=""), bar_plot)
}




# calculate the diff table 
cz_table_dif <- data.frame()

# set org table as first table

for ( iz in 1:length(cz_table$gdd) ) {
    cz_table$gdd <- as.character(cz_table$gdd)
    cz_table_org$gdd <- as.character(cz_table_org$gdd)

    cz_table$zon <- as.character(cz_table$zon)
    cz_table_org$zon <- as.character(cz_table_org$zon)

    #print(paste("iz:",iz,sep=""))
    dif_area=0.
    area_org <- cz_table_org$area[(cz_table_org$gdd==cz_table$gdd[iz])&(cz_table_org$zon==cz_table$zon[iz])]
    area_new <- cz_table$area[iz] 
    if( length(area_new)>0 & length(area_org)>0 ) {
             dif_area <- sum(area_new - area_org) 
    } else if ( (length(area_org)==0) )  {
                        dif_area <- area_new 
    } else {  dif_area <- 0.0  }

#    if( (cz_table_org$gdd[iz] == "A") | (cz_table_org$gdd[iz] == "A1") | (cz_table_org$gdd[iz]=="A2")  ) {
#          print(paste("dif_area:",dif_area,"From A to new gdd_zn:",cz_table$gdd[iz], "zonal lat:",cz_table_org$zon[iz],sep=" "))
#    }
#    if( (cz_table_org$gdd[iz] == "B") | (cz_table_org$gdd[iz] == "B1")   ) {
#          print(paste("dif_area:",dif_area,"From B to new gdd_zn:",cz_table$gdd[iz], "zonal lat:",cz_table_org$zon[iz],sep=" "))
#    }
   #add positive or negative 
   if (dif_area > 0.) {
      tmp <- data.frame(gdd=cz_table$gdd[iz], zon=cz_table$zon[iz], pos.area=dif_area, neg.area=0.0)
   }else if(dif_area < 0.) {
      tmp <- data.frame(gdd=cz_table$gdd[iz], zon=cz_table$zon[iz], pos.area=0.0, neg.area=dif_area)   
   }else{
      tmp <- data.frame(gdd=cz_table$gdd[iz], zon=cz_table$zon[iz], pos.area=0.0, neg.area=0.0)
   }
      cz_table_dif <- rbind(cz_table_dif,tmp)
 }

cz_table_dif <- cz_table_dif[complete.cases(cz_table_dif), ]

library("ggplot2")

data1 <- cz_table_dif 
#data1$zon <-  factor(data1$zon,
#          levels= c(rev(sort(levels(data1$zon)))) )
data1$zon <- factor(data1$zon,                                    # Change ordering manually
                  levels = c("S65","S55","S45","S35","S25","S15","S05",
                             "N05","N15","N25","N35","N45","N55","N65"))

bar_plot <- ggplot(data1, aes(zon)) + ylim(-15,15) + geom_bar(aes(y=pos.area, fill=gdd), stat="identity") + geom_bar(aes(y=neg.area, fill=gdd), stat="identity") + coord_flip() + scale_fill_manual(
           values=  c("A"="cyan","B"="cyan","C"="cyan","D"="blue","E"="blue",
                 "F"="darkgreen","G"="green2","H"="springgreen3","I"="springgreen1","J"="plum",
                 "K"="tan4","L"="tan1","M"="pink1","N"="gold","O"="yellow","P"="lightgoldenrod","Q"="wheat","R"="hotpink"))

ylab.txt <- bquote(.(exp.yr.txt[jyr])~.(scenario.txt[isc])~(Mkm^2))
#ylab.txt <- c("SSP")
bar_plot <- bar_plot + ylab(ylab.txt) 
bar_plot <- bar_plot + guides(fill=guide_legend(title="EnZ"))
bar_plot <- bar_plot + theme(legend.position = c(0.9, 0.5), legend.key.size = unit(0.6, 'cm'), axis.title.y=element_blank(), 
                             axis.text = element_text(size = 16), axis.title = element_text(size = 20))
#

#if (jyr ==  1) bar_plot <- bar_plot + theme(legend.position = "none")

#add bar plot  to pdf
assign(paste("bar_plot_",(irun+1),sep=""), bar_plot)



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
      #gross_MATRIX[j,i] <- length(DIF_LU[which(DIF_LU == dif_value & LU_A == org_value )]) 
      #apply weigting of area 
      gross_MATRIX[j,i] <- sum(land_area[which(DIF_LU == dif_value & LU_A == org_value )] ,na.rm=T)
       
   #if(i==j) gross_MATRIX[j,i]<- 0
    }
  }
  
  
  gross_MATRIX



#if (jyr <= 3) {
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

 
  
  cz_name_txt <-c("Artic","Artic","Artic","Bol.","Bol./Alp.",
              "Et.Cd.wt","Et.Cd.mc","Cd.mc","Cl.Tm.xc","Cl.Tm.mi",
              "Wm.Tm.mc","Wm.Tm.xc","Sub.Tp.","Ht.dy","Ht.ad",
              "Et.Ht.ad","Et.Ht.xc","Tropical")
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
      
   } # end of j
  } # end of i
  
  print(df_matrix)
  
  table.fname = c(paste(scenario.txt[isc],exp.yr.txt[jyr],"_gross_exchange_data.csv",sep=""))
  write.table(x=df_matrix, 
              file=table.fname,
              sep=",",
              row.names =FALSE,
              col.names=TRUE) 
  
  #read in the data.frame for both gross change and color information
  df0 <- read.csv(file = table.fname, stringsAsFactors=FALSE)
#  df1 <- read.csv(file = "gross_exchange_color.csv", stringsAsFactors=FALSE)
  
  df1<-data.frame(region=cz_name, order1=seq(1,18,1), col1=cz.color,reg1=cz_name,reg2=cz_name_txt)
 
# start plot
#  par( oma=c(0,0,0,0), mar=c(3,2,5,2),plt = c(0.01,0.99,0.01,.99), mgp=c(1,1,0), new=FALSE)
 # par(oma=c(0,0,0,0), mar=c(3,2,5,2),plt = c(0.01,0.99,0.01,.99), mgp=c(1,1,0),new=FALSE)
  df1<-data.frame(region=cz_name, order1=seq(1,18,1), col1=cz.color,reg1=cz_name,reg2=cz_name_txt)

# start plot
set.color =  c("B"="cyan","D"="blue","F"="darkgreen","G"="green2",
                       "H"="springgreen3","I"="springgreen1","J"="plum","K"="tan4","L"="tan1","M"="pink1",
                       "N"="gold","O"="yellow","P"="lightgoldenrod","Q"="wheat","R"= "hotpink")

#         set.color=  c("A"="cyan","B"="cyan","C"="cyan","D"="blue","E"="blue",
#                 "F"="darkgreen","G"="green2","H"="springgreen3","I"="springgreen1","J"="plum",
#                 "K"="tan4","L"="tan1","M"="pink1","N"="gold","O"="yellow","P"="lightgoldenrod","Q"="wheat","R"="hotpink"))


  circos.par( start.degree = 85, clock.wise = TRUE, track.margin = c(-0.14, 0.16), circle.margin=c(0.01, 0.01, 0.01, 0.01))
  chordDiagram(x = df0, transparency = 0.15, grid.col = set.color,
               directional = 1 ,   direction.type = c("diffHeight","arrows"),
               annotationTrack = c("grid","axes"), annotationTrackHeight = c(0.005),
               link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE, link.arr.length = 0.08)
# add text and axis
circos.trackPlotRegion(
    track.index = 1,
    bg.border = NA,
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      sector.index = get.cell.meta.data("sector.index")
      reg1 = df1$reg2[df1$region == sector.index]
    #  reg2 = df1$reg2[df1$region == sector.index]
      circos.text(x = mean(xlim), y = 40,
                  labels = reg1, facing = "bending", cex = 1.8)
      # circos.text(x = mean(xlim), y = 3.1,  labels = reg2, facing = "bending", cex = 1.2)
      circos.axis(h = "top", major.at = seq(from = 0, to = xlim[2], by = 10),
                  minor.ticks = 1, major.tick.length=10,labels.niceFacing = TRUE,labels.cex =1.5)
    })

text(0, 0, paste(exp.yr.txt[jyr],"\n",scenario.txt[isc],sep=""), cex = 2.5)

#show circ parameter
circos.par
  circos.clear()

#}# jyr ld 
  
#par(new=FALSE)
#par(oma=c(0,0,0,0), mar=c(3,2,5,2),plt = c(0.01,0.99,0.01,.99), mgp=c(1,1,0))


# show the plot
#plot(region_mask, ylim=c(21.5,25.5))

#par(new=FALSE)
#create gross change mask
gc_mask <- DIF_LU
gc_mask[gc_mask==0] <- NA
gc_mask[gc_mask!=0] <- 1
gc_mask <- gc_mask*LU_B

gc_mask.rst <- fun_a2r(input.arr=t(gc_mask[,ny:1]) )
#
gc_poly <- rasterToPolygons(gc_mask.rst, dissolve=TRUE)
#
gc_point <- rasterToPoints(gc_mask.rst) 

ld_go <- FALSE

if( ld_go) {

# apply gc_mask to show the hot spot
#LU_B <- LU_B*gc_mask

plot.gd <- fun_a2r(input.arr=t(LU_B[,ny:1]) )
leg.at  <-  c("2.5","4.5","6","7","8",
              "9","10","11","12",
              "13","14","15","16","17","18")
leg.txt <-  c("C","E","F","G","H",
              "I","J","K","L",
              "M","N","O","P","Q","R")
#if ((jyr==1) | (jyr==2)| (jyr==3) ) {
#plot(x=0,y=1,xaxt="n", yaxt="n",type="n")
#plot(coastlines,col="white", lwd=0.1, ylim=c(-56,90),xlim=(-180,180),add=T)
#
par(xpd=FALSE)

#start plot 

plot(plot.gd,  ylim=c(-65,85),xlim=c(-150,165), xaxt='n', yaxt='n',legend=FALSE,
     # main="Daily mean bias of Growing Degree-Days (oC)",
     # xlab="", ylab="Latitude", col=wes_palette("Zissou1", 50, type = "continuous"),
     xlab="", ylab="", col=cz.color, box=FALSE, axes=FALSE)#,
     #smallplot=c(0.05,.95, 0.2, 0.24), horizontal=T,
     #axis.args=list(at=leg.at, labels=leg.txt, cex.axis=0.4,line=0.25, gap.axis=0.0),
     #legend.args=list(text= "Climate Zone" , side=3, line=0.1, cex=0.5))


#add coastline
lines(coastlines, lwd=0.3, add=T)
#add gross change location
#plot(gc_poly,lwd=0.3,col=NA,add=T,border="lightgray",lty="dotted")
points(gc_point,pch = 21, bg = NA, col = "white", 
     lwd = 0.1, cex = .15,add=T)
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
  mtext(paste(scenario.txt[isc],exp.yr.txt[jyr],sep=" "),side=3,line=-2,cex=1.5)

#} #end if

} #ld_go



} #end of jyr

dev.off()

#library("ggpubr")
library("gridExtra")
pdf(paste("./pdf_files/zonal_change_2020_2100_(ref_hist)",scenario[isc],".pdf",sep=""),width=24,height=12)
grid.arrange( bar_plot_2, bar_plot_3, bar_plot_4, bar_plot_5,
              bar_plot_6, bar_plot_7, bar_plot_8, bar_plot_9,
              ncol = 4, nrow = 2 )

dev.off()
} #end of isc

# close the device


