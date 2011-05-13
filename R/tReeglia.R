SuccEstimates <- function(Vol.pool, Lateral.area, Tree.object) {
# internal function
# version 1 2003-10-22

    Increment <- function(V.funct) {
    # internal function
    # computes volume or lateral surface increment, as the
    # difference between the volumes of two consecutive years
    # version 2 2003-10-22
        
        y <- matrix(ncol=1,nrow=length(V.funct), data=c(0, V.funct[1:length(V.funct)-1]))
        return(V.funct - y)
    }
        
    # builds an array of 3 squared coefficient of variations from the 
    # shrink coefficient[1], density[2], and Carbon content[3] standard errors
    CVs <- c(((Tree.object$"vols.ste"/Tree.object$"vols")^2),((Tree.object$"basd.ste"/Tree.object$"basd")^2),((Tree.object$"carb.ste"/Tree.object$"carb")^2))
    
    # computes tree fresh volume standard error
    # Bascietto M., Scarascia-Mugnozza G., A collection of functions to determine annual tree Carbon increment via stem analysis, Ann. Sci. For. (accepted)
    Vol.pool.ste <- Vol.pool * CVs[1]^0.5
    
    # computes tree annual volume increments by substraction of the volumes 
    # of consecutive years, and their standard error
    Vol.incr <- Increment(Vol.pool)
    Vol.incr.ste <- Vol.incr * CVs[1]^0.5
    
    # computes tree annual area increments by substraction of the 
    # areas of consecutive years, and their standard error
    #rownames(Lateral.area) <- rownames(RWdata)
    Lateral.area.incr <- Increment(Lateral.area)
    
    # builds a matrix holding tree annual dry-matter biomass (Mg or t) by converting 
    # fresh volume to dry matter using basic density (Mg/m^3), and standard errors
    Biomass.pool <- Vol.pool * Tree.object$"basd"
    Biomass.pool.ste <- Biomass.pool * (CVs[1] + CVs[2])^0.5
    
    # computes tree annual biomass increments (Mg/yr or t/yr) by converting fresh volume increments 
    # to dry matter increments using basic density (Mg/m^3), and standard errors 
    Biomass.incr <- Vol.incr * Tree.object$"basd"
    Biomass.incr.ste <- Biomass.incr * (CVs[1] + CVs[2])^0.5
    
    # builds a matrix holding tree annual Carbon biomass (MgC or tC) by applying C content 
    # coefficient to dry matter biomass, and standard errors
    Carbon.pool <- Biomass.pool * Tree.object$"carb"
    Carbon.pool.ste <- Carbon.pool * (CVs[1] + CVs[2] + CVs[3])^0.5

    # computes tree annual Carbon biomass increments (MgC/yr or tC/yr) by applying C content 
    # coefficient to dry matter increments, and standard errors
    Carbon.incr <- Biomass.incr * Tree.object$"carb"
    Carbon.incr.ste <- Carbon.incr * (CVs[1] + CVs[2] + CVs[3])^0.5
    
    # returns a dataframe
    return(data.frame("Vol.pool"=Vol.pool, "Vol.pool.ste"=Vol.pool.ste, "Vol.incr"=Vol.incr, "Vol.incr.ste"=Vol.incr.ste, "Biomass.pool"=Biomass.pool,"Biomass.pool.ste"=Biomass.pool.ste, "Biomass.incr"=Biomass.incr, "Biomass.incr.ste"=Biomass.incr.ste, "Carbon.pool"=Carbon.pool,"Carbon.pool.ste"=Carbon.pool.ste, "Carbon.incr"=Carbon.incr, "Carbon.incr.ste"=Carbon.incr.ste,"Lateral.area"=Lateral.area,"Lateral.area.incr"=Lateral.area.incr))
}

StemAnalysis <- function(RWdata, Tree.height=23.91, Tree.object) {
# version 2 2003-10-22

    RadiusFunct <- function(x) {
    # internal function, computes radius on individual columns of ring-widths
    # version 1 2003-10-20
           
        y <- matrix(nrow=length(x), ncol=1)
        
        j = 1
        
        while (is.na(x[j])) {
            j=j+1
            y[j] = NA
        }
        
        y[j] = x[j]
        j=j+1
        
        for (i in j:length(x)) y[i] = x[i] + y[i-1]
        
        return(y)
    }
    
    Gfunct <- function(x) {
    # internal function, computes basal area on individual columns of radia
    # version 1 2003-10-21
           
        y <- matrix(nrow=length(x), ncol=1)
        
        for (i in 1:length(x)) y[i] = x[i]^2*pi
        
        return(y)
    }
    
    CarmeanHeight <- function(rw, tree.height) {
    # performs interpolation of the height of the tree per each year
    # version 1 2003-10-21
    # Interpolates tree heights using Carmean's algorithm
    # Carmean W.H., Site index curves for Upland Oaks in the Central States, For. Sci. 18 (1972) 109-120.
    # and
    # Fabbio G., Frattegiani M., Manetti M.C., Il metodo di analisi del fusto. Confronto tra cinque metodi di stima della relazione altezza-età, Annali dell'Istituto Sperimentale per la Selvicoltura, Arezzo 19 (1988) 117-154.
    
        #
        # equation for the lowest log
        #
    
        height.cs <- as.numeric(colnames(rw))
        height.lower <- height.cs[1]
        loglength <- height.cs[2] - height.lower
        dead.rings.no <- length(na.omit(rw[,1])) - length(na.omit(rw[,2]))
    
        yearly.height <- matrix(nrow=length(rw[,1]), ncol=1)
        rownames(yearly.height) <- rownames(rw)
        
        for (j in 1:dead.rings.no) yearly.height[j] <- height.lower + j * (loglength / (0.5 + dead.rings.no))
        
        #
        # equation for middle logs
        #
    
        height.counter <- j + 1
        middle.log.no <- length(height.cs)-1
        for (log in 2:middle.log.no) {
            height.lower <- height.cs[log]
            loglength <- height.cs[log+1] - height.lower
            dead.rings.no <- length(na.omit(rw[,log])) - length(na.omit(rw[,log+1]))
            ring.finish <- height.counter + dead.rings.no - 1
    
            i=1
            for (j in height.counter:ring.finish) {
                yearly.height[j] <- height.lower + loglength / (2 * dead.rings.no) + (i - 1) * loglength / dead.rings.no
                i=i+1
            }
            height.counter = height.counter + dead.rings.no 
        }
        
        #
        # equation for highest log (from the highest cross-section to tree tip)
        #
    
        log=log+1
        height.lower <- height.cs[log]
        loglength <- tree.height - height.lower
        dead.rings.no <- length(na.omit(rw[,log]))
        ring.finish <- height.counter + dead.rings.no - 1
        foo <- loglength / dead.rings.no / 2
        loglength = tree.height + foo - height.lower
        
        i=1
        for (j in height.counter:ring.finish) {
            yearly.height[j] <- height.lower + loglength / (2 * dead.rings.no - 1) + (i - 1) * loglength / dead.rings.no
            i=i+1
        }
        
        return(yearly.height)
    }

    LogVolArea <- function(R.funct, G.funct, Tree.height) {
    # internal function
    # computes volume and lateral surface area of each log enclosed by two 
    # consecutive cross-section, on a yearly basis.
    # version 1 2003-10-21
    
        SumNoNA <- function(x) sum(na.omit(x))
   
        height.cs <- as.numeric(colnames(G.funct))
        volume <- matrix(ncol=ncol(G.funct),nrow=nrow(G.funct))
        area <- matrix(ncol=ncol(G.funct),nrow=nrow(G.funct))
        
        for (j in 1: length(G.funct[,1])) {
            
            log.no <- length(na.omit(G.funct[j,]))-1
            
            if (log.no > 0) for (i in 1:log.no) {
                volume[j,i] <- ((G.funct[j,i] + G.funct[j,i+1]) / 2) * (height.cs[i+1] - height.cs[i]) # volume of trapezium
                area[j,i] <- pi * (R.funct[j,i+1] + R.funct[j,i]) * (height.cs[i+1] - height.cs[i]) # surface area of trapezium
            }
            volume[j,log.no+1] <- (G.funct[j,log.no+1] / 3) * (Tree.height[j] - height.cs[log.no+1]) # volume of the hidden cone
            area[j,log.no+1] <- pi * R.funct[j,log.no+1] * (R.funct[j,log.no+1]^2 + (Tree.height[j] - height.cs[log.no+1])^2)^0.5 # surface area of the hidden cone
        }
        V <- (cbind(apply(volume,1,SumNoNA))) # sum of the volume of the logs and the hidden cone per each year (rows)
        A <- (cbind(apply(area,1,SumNoNA))) # sum of the surface area of the logs and the hidden cone per each year (rows)
        
        return(c(V,A))
    }

Yearly.tree.height <- CarmeanHeight(RWdata, Tree.height)
    
    # builds a matrix with identical dimensions of the rw matrix, 
    # containing the radius for each year, for each cross-section
    # and converts it to m
    RR <- cbind(apply(RWdata,2,RadiusFunct))
    RR <- RR / Tree.object$"tom"
    
    # builds a matrix with identical dimensions of the rw matrix, 
    # containing the basal area for each year, for each cross-section
    GG <- cbind(apply(RR,2,Gfunct))
    
    # computes tree volume and lateral surface area of each log enclosed by two 
    # consecutive cross-section, on a yearly basis.
    foo <- LogVolArea(RR, GG, Yearly.tree.height)
    
    # converts tree volume for mean room temperature and humidity to fresh volume
    Vol.pool <- matrix(foo[1:length(Yearly.tree.height)] * (Tree.object$"vols"))
    rownames(Vol.pool) <- rownames(RWdata)
    
    # extracts tree lateral surface area 
    Lateral.area <- matrix(foo[(length(Yearly.tree.height)+1):length(foo)])

    rm(foo)  
    
    # returns a dataframe
    return(data.frame(SuccEstimates(Vol.pool, Lateral.area, Tree.object), "Height"=Yearly.tree.height))
}

PoolPlot <- function(SA){
# plots 4 graphs of pools with plus or minus 1 standard error bars
# version 1 2003-10-22

    year <- as.numeric(rownames(SA))
    
    oldpar=par(mfrow=c(2,2))
    plot(y=SA$"Lateral.area", x=year, ylab = "Lateral surface area (m^2)", xlab = "Year", type="l")
    
    plot(y=SA$"Vol.pool", x=year, ylab = "Volume (m^3)", xlab = "Year", type="l")
    segments(x0=year, x1=year, y0=SA$"Vol.pool"+SA$"Vol.pool.ste", y1=SA$"Vol.pool"-SA$"Vol.pool.ste")
    
    plot(y=SA$"Biomass.pool", x=year, ylab = "Biomass pool", xlab = "Year", type="l")
    segments(x0=year, x1=year, y0=SA$"Biomass.pool"+SA$"Biomass.pool.ste", y1=SA$"Biomass.pool"-SA$"Biomass.pool.ste")

    plot(y=SA$"Carbon.pool", x=year, ylab = "Carbon pool", xlab = "Year", type="l")
    segments(x0=year, x1=year, y0=SA$"Carbon.pool"+SA$"Carbon.pool.ste", y1=SA$"Carbon.pool"-SA$"Carbon.pool.ste")

    par(oldpar)
}

IncrementPlot <- function(SA){
# plots 4 graphs of increments with plus or minus 1 standard error bars
# version 1 2003-10-22
    
    year <- as.numeric(rownames(SA))
    
    oldpar=par(mfrow=c(2,2))
    plot(y=SA$"Lateral.area.incr", x=year, ylab = "Lateral surface area increment (m^2/yr)", xlab = "Year", type="l")
    
    plot(y=SA$"Vol.incr", x=year, ylab = "Volume increment (m^3/yr)", xlab = "Year", type="l")
    segments(x0=year, x1=year, y0=SA$"Vol.incr"+SA$"Vol.incr.ste", y1=SA$"Vol.incr"-SA$"Vol.incr.ste")
    
    plot(y=SA$"Biomass.incr", x=year, ylab = "Biomass increment", xlab = "Year", type="l")
    segments(x0=year, x1=year, y0=SA$"Biomass.incr"+SA$"Biomass.incr.ste", y1=SA$"Biomass.incr"-SA$"Biomass.incr.ste")

    plot(y=SA$"Carbon.incr", x=year, ylab = "Carbon increment", xlab = "Year", type="l")
    segments(x0=year, x1=year, y0=SA$"Carbon.incr"+SA$"Carbon.incr.ste", y1=SA$"Carbon.incr"-SA$"Carbon.incr.ste")

    par(oldpar)
}

JoinBranchesToStem <- function(RWdata, Tree.object, branches) {
#
# joins the volume pool or the lateral surface area of the branches and the main stem,
# particularly useful for broadleaf tree species, showing complex branching architecture
#
# version 1 2003-10-22

    JoinBranches <- function(branches, start.sublist) {
    # internal function
    # joins the volume pool or the lateral surface area of the branches and the main stem,
    #
    # version 1 2002-10-22
    
        ifor.step = 15 # number of estimates in each SA dataframe
        tree.SA <- branches[[start.sublist]] # stores in tree.SA the volume pool, sublist 1 of list branches or the lateral surface area, sublist 13
        tree.year <- length(branches[[1]]) # stores the age of the tree
        ifor <- seq(from=(ifor.step+start.sublist), to=length(branches), by=ifor.step)
    
        for (i in ifor) {
            year <- length(branches[[i]])
            year.step <- tree.year-year
            for (j in 1:year) tree.SA[(j+year.step)] <- tree.SA[(j+year.step)] + branches[[i]][j]
            }
        return(tree.SA)   
    }

    Tree.vol <- JoinBranches(branches, 1)
    Tree.area <- JoinBranches(branches, 13)
    Tree.SA <- data.frame(SuccEstimates(Tree.vol, Tree.area, Tree.object=Tree.object), "Height"=branches$"Height")
    rownames(Tree.SA) <- rownames(RWdata)
    return(Tree.SA)
}

ExportFile <- function(SAdata, filename, ...) {
# version 1 20031022

    write.table(x=SAdata, file=filename, sep=",",...)
}

BuildTreeObject <- function(tom=1000000, tocm=10000, vols=1.0635, vols.ste=0.0, basd=0.51783, basd.ste=0.04281, carb=0.4577,carb.ste=0.00243) {
# version 1 2003-10-22
# Definitions of constants to be stored in Tree.features data.frame 


	return(data.frame("tom"=tom, "tocm"=tocm, "vols"=vols, "vols.ste"=vols.ste, "basd"=basd, "basd.ste"=basd.ste, "carb"=carb, "carb.ste"=carb.ste))
}

BuildAgeTable <- function(RWdata, Tree.object) {
# builds a matrix holding the number of rings, age and diameter for each cross-section,
# radius is converted to cm
# version 1 2003-10-22    

    CountRW <- function(x) length(na.omit(x))
    AgeFromRW <- function(x) length(x)-length(na.omit(x))

    Age.table <- cbind("no"=apply(RWdata,2,CountRW),"radius"=apply(RWdata,2,AgeFromRW),"age"=apply(RWdata,2,AgeFromRW))
    Age.table[,"radius"] <- Age.table[,"radius"] / Tree.object$"tocm"
    
    return(Age.table)
}

ImportRW <- function(filename) {
# imports ring-width file
# version 1 2003-10-18
    
    read.table(filename,head=F,nrows=1,sep=",")[-1]->height
    read.table(filename,head=F,skip=1,sep=",")[,-1]->rw
    read.table(filename,head=F,skip=1,sep=",")[,1]->year
    
    colnames(rw)<-as.character(height)
    rownames(rw)<-as.character(year)

    return(rw)
}
