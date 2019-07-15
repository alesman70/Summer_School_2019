library(ggplot2); theme_set(theme_bw())

participants <- c('nikipardalou@gmail.com','sana.khemiri@instm.rnrt.tn','rafik.zarrad@gmail.com','hanem.djabou@laposte.net','reda_fahim2010@yahoo.com','ines.haouas@instm.rnrt.tn','me.benziane@gmail.com','h.gazzar2@gmail.com','selfatimohamed@gmail.com','enrico.e.armelloni@gmail.com','martina.scanu@hotmail.it','hamdi.moussa@ogr.iu.edu.tr','rachele.corti19@gmail.com','adel_gaam@yahoo.fr','ilaria.costantini@an.ismar.cnr.it','caterina.sciagura@gmail.com','marco.barra@iamc.cnr.it','Sharif.jemaa@cnrs.edu.lb','jghabayman@gmail.com','gorfanid@gmail.com','filalitahar@gmail.com','myriamlteif@hotmail.com','reno.micallef@gov.mt','federicodimaio@libero.it','andrea.pierucci@unica.it')

participants <- participants[!participants %in% c("rafik.zarrad@gmail.com", "h.gazzar2@gmail.com")]

##participants <- c('Ale Ligas','Ale Man','Ale Orio','Chato','Coilin')

which(participants == "federicodimaio@libero.it")

n <- length(participants)

drumroll <- function(nroll = 15){
    for(i in 1:nroll){
        df <- data.frame(x = runif(n), y = runif(n), participant = participants)
        p <- ggplot(df, aes(x = x, y = y)) +
            geom_text(aes(x, y, label = participant), size = 5) +
            coord_cartesian(xlim = c(-0.2, 1.2)) +
            theme(
                axis.line=element_blank(),axis.text.x=element_blank(),                                                                               axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),legend.position="none",
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())
        print(p)
        Sys.sleep(.3)
    }
    df2 <- df[sample(1:nrow(df), 1), ]
    p1 <- p + geom_text(data = df2, aes(x, y, label = participant), size = 5, colour = "blue")
    print(p1)
    system(paste0("xcowsay -t 15 ", df2$participant, " will answer!!"))
}

drumroll(nroll = 10)

orange <- colorRampPalette(c("moccasin", "orange", "darkorange"))
oranges <- orange(3)

max2 <- function(x){
    x[which.max(x)] <- NA
    max(x, na.rm = TRUE)
}

rwalk <- function(nstep, sd, sleep.time, who){
    sd.low <- 0.1 * sd
    sd <- rep(sd, n)
    ##sd[who] <- sd.low 
    x <- y <- matrix(NA, nrow = n, ncol = nstep)
    x[,1] <- y[,1] <- 0
    for(j in 2:nstep){
        x[, j] <- rnorm(n, mean = x[, j - 1], sd = sd)
        y[, j] <- rnorm(n, mean = y[, j - 1], sd = sd)
    }
    xout <- x < -1 | x > 1
    yout <- y < -1 | y > 1
    xyout <- xout | yout
    idx <- unlist(lapply(apply(xyout, 1, FUN = function(z){which(z)}), "[", 1))
    idx[is.na(idx)] <- nstep
    for(i in 1:n){
        if(idx[i] < nstep){
            x[i, idx[i]:nstep] <- NA
            y[i, idx[i]:nstep] <- NA
        }        
    }    
    ##for(j in 1:(max(idx)-1)){    
    for(j in 1:(max2(idx)-1)){
        par(xpd = NA, mar = c(5, 5, 5, 5))
        plot(x[,j], y[,j], xlim = c(-1, 1), ylim = c(-1, 1), col = "darkorange", axes = F, xlab = "", ylab = "", pch = 19, xaxs = "i")
        box()
        ##title("who stays?")
        if(j > 3){
            segments(x[,j-1], y[,j-1], x[,j], y[,j], col = oranges[3])
            segments(x[,j-2], y[,j-2], x[,j-1], y[,j-1], col = oranges[2])
            segments(x[,j-3], y[,j-3], x[,j-2], y[,j-2], col = oranges[1])
        }
        text(x[,j], y[,j], label = participants, cex = 1)
        Sys.sleep(sleep.time)
    }
    ##browser()
    ##rnk <- order(idx, decreasing=T)
    ##if(rnk[1] == 1 | rnk[1] == 2){
    ##    system(paste0("xcowsay -t 10 ", participants[which.max(idx)], " will run a4a!!"))
    ##}else{
    ##    system(paste0("xcowsay -t 10 ", participants[which.max(idx)], " will run spict!!"))
    ##}    
    system(paste0("xcowsay -t 10 ", participants[which.max(idx)], " will answer!!"))
    ##browser()
}

library(ggplot2); theme_set(theme_bw())

participants <- paste("Team", 1:4)

n <- length(participants)

rwalk(nstep = 1000, sd = 0.1, sleep.time = 0.2, who = 23)

