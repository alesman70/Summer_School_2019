library(ggplot2); theme_set(theme_bw())
participants <- c('dhaker.troudiest@laposte.net', 'akram_turky@yahoo.com',
                  'quattrocchifederico@gmail.com', 'andrea.pierucci@hotmail.it',
                  'mennad.moussa@gmail.com', 'marcokule11@gmail.com', 'kouched_wael@yahoo.fr',
                  'nazliktu@gmail.com', 'merve.karakus@tarimorman.gov.tr',
                  'ekamberi@ubt.edu.al', 'kamel_benounnas@yahoo.fr',
                  'hanem.djabou@laposte.net', 'aymen.haj.82@gmail.com', 'mer_zaque@yahoo.fr', 'miriam.gambin@gov.mt', 'derbali10@gmail.com',
                  'federico.cali@irbim.cnr.it', 'gabriele.boscolopalo@gmail.com',
                  'savasyay@gmail.com')

participants <- participants[!participants %in% c("dhaker.troudiest@laposte.net", "nazliktu@gmail.com", "kamel_benounnas@yahoo.fr", "quattrocchifederico@gmail.com", "andrea.pierucci@hotmail.it", "akram_turky@yahoo.com", "mennad.moussa@gmail.com", "aymen.haj.82@gmail.com", "gabriele.boscolopalo@gmail.com", "derbali10@gmail.com")]

n <- length(participants)

orange <- colorRampPalette(c("moccasin", "orange", "darkorange"))
oranges <- orange(3)

max2 <- function(x){
    x[which.max(x)] <- NA
    max(x, na.rm = TRUE)
}

escape <- function(nstep, sd, sleep.time, who){
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
    for(j in 1:max2(idx)){
        par(xpd = NA, mar = c(5, 5, 5, 5), bg = "white")
        plot(x[,j], y[,j], xlim = c(-1, 1), ylim = c(-1, 1), col = "darkorange", axes = F, xlab = "", ylab = "", pch = 19, xaxs = "i")
        box()
        ##title("who stays?")
        if(j > 3){
            segments(x[,j-1], y[,j-1], x[,j], y[,j], col = oranges[3], lwd = 2)
            segments(x[,j-2], y[,j-2], x[,j-1], y[,j-1], col = oranges[2], lwd = 1.5)
            segments(x[,j-3], y[,j-3], x[,j-2], y[,j-2], col = oranges[1])
        }
        text(x[,j], y[,j], label = participants, cex = 0.9)
        Sys.sleep(sleep.time)
    }
    par(col.main = "red")
    title(paste(participants[which.max(idx)], " will answer!!"), col = "blue", cex = 1.5)
    par(col.main = "black")
    ##browser()
    ##rnk <- order(idx, decreasing=T)
    ##if(rnk[1] == 1 | rnk[1] == 2){
    ##    system(paste0("xcowsay -t 10 ", participants[which.max(idx)], " will run a4a!!"))
    ##}else{
    ##    system(paste0("xcowsay -t 10 ", participants[which.max(idx)], " will run spict!!"))
    ##}    
    ##system(paste0("xcowsay -t 10 ", participants[which.max(idx)], " will answer!!"))
    ##browser()
}

dev.off()
plot.new()
library(ggplot2); theme_set(theme_bw())

escape(nstep = 1e4, sd = 0.2, sleep.time = 0.01, who = NA)

## now slow down
escape(nstep = 1e4, sd = 0.1, sleep.time = 0.2, who = NA)



## with the teams
library(ggplot2); theme_set(theme_bw())
participants <- c("a4a", "spict", "xsa")
participants <- participants[!participants %in% c("")]
n <- length(participants)

escape(nstep = 1e4, sd = 0.1, sleep.time = 0.2, who = NA)

