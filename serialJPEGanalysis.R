binning.jpeg  <- function(dir,bin.length.sec=3600,frame.interval.sec = 10,dir.out="bins"){
    # load package, add download if not there
    if (!"EBImage" %in% rownames(installed.packages())) {
        source("http://bioconductor.org/biocLite.R")
        biocLite("EBImage")
    }
    library(EBImage)
    
    #make list of file paths and make dirs
    files  <- list.files(path = dir, pattern = "*.jpg",full.names = F)
    dir.create("csv",showWarnings = FALSE)
    dir.create("delta",showWarnings = FALSE)
    dir.create(dir.out,showWarnings = FALSE)
    
    print("----------   Converting JPEGs to CSVs   ----------")
    p <- i  <- 0
    for (file in files) {
        img <- as.array(resize(channel(readImage(paste0(dir,file),type = "jpeg"),"gray"),h = 100))
        write.csv(img,file=paste0("csv/",file))
        i  <- i + 1
        p  <- (i/length(files))*100
        print(paste0(dir,file," - ",round(p,2),"%"))
     }
    print(" ")
    print("----------   Generating Differences Matrices    ----------")
    delta <- files[-1]
    baseline  <- files[-length(files)]
    p <- i  <- 0
    for(i in 1:length(delta)){
        array  <- read.csv(paste0("csv/",delta[i])) - read.csv(paste0("csv/",baseline[i]))
        write.table(x=abs(array), file = paste0("delta/",delta[i]),sep = ",")
        p  <- (i/length(delta))*100
        print(paste0(delta[i]," ",round(p,2)," %"))
    }
    #bin.length.sec  <- 3600   # function should input this value (default 1hr)
    #frame.interval.sec <- 10 # function shoud input this value (default 10 sec)
    frames.per.bin <- bin.length.sec/frame.interval.sec # function should input this value 36000=sec in hr
    bin.number  <- 1:round(length(delta)/frames.per.bin)
    if ((length(delta) %% 2) == 1) {
        write.table(x=data.frame(
            matrix(
                0,
                nrow=nrow(read.csv(paste0("delta/",delta[1]))),
                ncol=ncol(read.csv(paste0("delta/",delta[1])))
            )
        ), file = paste0("delta/0.jpg"),sep = ",")
    }
    #bin.number.char <-  sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin.number)
    print("----------   Adding Matrices and Saving     ----------")
    print(paste0("bin - start - finish"))
    delta <- list.files("delta/",pattern = "*.jpg",full.names = FALSE)
    for (bin in bin.number) {
        start  <- as.numeric((bin-1)*frames.per.bin+1)
        finish <- bin*frames.per.bin
        array  <- data.frame(matrix(
            0,
            nrow=nrow(read.csv(paste0("delta/",delta[1]))),
            ncol=ncol(read.csv(paste0("delta/",delta[1])))
        ))
        for (number in start:finish){
            array <- array + read.csv(paste0("delta/",delta[number]))
        }
        write.table(x=array, file = paste0(
            dir.out,"/","bin-",sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin),
            ".frame-",start,"to",finish,".secs",bin.length.sec,".raw.csv"),
            sep = ",",row.names = F,col.names = F)
        write.table(x=scale(array), file = paste0(
            dir.out,"/","bin-",sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin),
            ".frame-",start,"to",finish,".secs",bin.length.sec,".zscore.csv"),
            sep = ",",row.names = F,col.names = F)
        print(paste0(" ",bin," -   ",start,"  -   ",finish))
    }
}

binning.png  <- function(dir,bin.length.sec=3600,frame.interval.sec = 10,dir.out="bins"){
    # load package, add download if not there
    if (!"EBImage" %in% rownames(installed.packages())) {
        source("http://bioconductor.org/biocLite.R")
        biocLite("EBImage")
    }
    library(EBImage)
    
    #make list of file paths and make dirs
    files  <- list.files(path = dir, pattern = "*.png",full.names = F)
    dir.create("csv",showWarnings = FALSE)
    dir.create("delta",showWarnings = FALSE)
    dir.create(dir.out,showWarnings = FALSE)
    
    print("----------   Converting PNGs to CSVs   ----------")
    p <- i  <- 0
    for (file in files) {
        img <- as.array(resize(channel(readImage(paste0(dir,file),type = "png"),"gray"),h = 100))
        write.csv(img,file=paste0("csv/",file))
        i  <- i + 1
        p  <- (i/length(files))*100
        print(paste0(dir,file," - ",round(p,2),"%"))
    }
    print(" ")
    print("----------   Generating Differences Matrices    ----------")
    delta <- files[-1]
    baseline  <- files[-length(files)]
    p <- i  <- 0
    for(i in 1:length(delta)){
        array  <- read.csv(paste0("csv/",delta[i])) - read.csv(paste0("csv/",baseline[i]))
        write.table(x=abs(array), file = paste0("delta/",delta[i]),sep = ",")
        p  <- (i/length(delta))*100
        print(paste0(delta[i]," ",round(p,2)," %"))
    }
    #bin.length.sec  <- 3600   # function should input this value (default 1hr)
    #frame.interval.sec <- 10 # function shoud input this value (default 10 sec)
    frames.per.bin <- bin.length.sec/frame.interval.sec # function should input this value 36000=sec in hr
    bin.number  <- 1:round(length(delta)/frames.per.bin)
    if ((length(delta) %% 2) == 1) {
        write.table(x=data.frame(
            matrix(
                0,
                nrow=nrow(read.csv(paste0("delta/",delta[1]))),
                ncol=ncol(read.csv(paste0("delta/",delta[1])))
            )
        ), file = paste0("delta/0.png"),sep = ",")
    }
    #bin.number.char <-  sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin.number)
    print("----------   Adding Matrices and Saving     ----------")
    print(paste0("bin - start - finish"))
    delta <- list.files("delta/",pattern = "*.png",full.names = FALSE)
    
    for (bin in bin.number) {
        start  <- as.numeric((bin-1)*frames.per.bin+1)
        finish <- bin*frames.per.bin
        array  <- data.frame(matrix(
            0,
            nrow=nrow(read.csv(paste0("delta/",delta[1]))),
            ncol=ncol(read.csv(paste0("delta/",delta[1])))
        ))
        print(array)
                for (number in start:finish){
            array <- array + read.csv(paste0("delta/",delta[number]))
        }
        print("write tables")
        write.table(x=array, file = paste0(
            dir.out,"/","bin-",sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin),
            ".frame-",start,"to",finish,".secs",bin.length.sec,".raw.csv"),
            sep = ",",row.names = F,col.names = F)
        write.table(x=scale(array), file = paste0(
            dir.out,"/","bin-",sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin),
            ".frame-",start,"to",finish,".secs",bin.length.sec,".zscore.csv"),
            sep = ",",row.names = F,col.names = F)
        print(paste0(" ",bin," -   ",start,"  -   ",finish))
    }
    }

makeHeatmaps <- function(
        dir,
        bin.type = "zscore",
        my.colors = c("black","purple","blue", "red","orange","yellow","white")
){
    # load package, add download if not there
    if (!"gplots" %in% rownames(installed.packages())) {
        install.packages("gplots")
    }
    library(gplots)
    if (!"RColorBrewer" %in% rownames(installed.packages())) {
        install.packages("RColorBrewer")
    }
    library(RColorBrewer)
    
    dir <- paste0(dir,"/")
    my.colors <- colorRampPalette(my.colors)(n = 299)
    files <- list.files(path = dir,pattern = paste0("*.",bin.type,".csv"),full.names = T)
    pdf(file = paste0("heatmap.",bin.type,".pdf"),width = 11,height = 8.5)
    lapply(
        files,
        function(x) {heatmap.2(t(data.matrix(read.csv(x,header = F)[,-1])),
                               main = sub(dir,"",sub(paste0(".",bin.type,".csv"),"",x)), # heat map title
                               density.info="none",  # turns off density plot inside color legend
                               trace="none",         # turns off trace lines inside the heat map
                               margins =c(12,9),     # widens margins around plot
                               col=my.colors,       # use on color palette defined earlier
                               # breaks=col_breaks,    # enable color transition at specified limits
                               dendrogram="none",     # only draw a row dendrogram
                               na.color="black",
                               Rowv = FALSE,
                               Colv=FALSE)            # turn off column clustering
        }
    )
    dev.off()
}

# make function summing all bins
# need to make function to segment cage and bargraphs of activity in each sections
# need to make function to make bargraphs of activity automaticlly
