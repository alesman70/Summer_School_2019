library(ggplot2)

participants <- c('dhaker.troudiest@laposte.net', 'akram_turky@yahoo.com',
                  'quattrocchifederico@gmail.com', 'andrea.pierucci@hotmail.it',
                  'mennad.moussa@gmail.com', 'marcokule11@gmail.com', 'kouched_wael@yahoo.fr',
                  'nazliktu@gmail.com', 'merve.karakus@tarimorman.gov.tr',
                  'ekamberi@ubt.edu.al', 'kamel_benounnas@yahoo.fr',
                  'hanem.djabou@laposte.net', 'aymen.haj.82@gmail.com', 'mer_zaque@yahoo.fr',
                  'helenaglamuzina1@gmail.com', 'miriam.gambin@gov.mt', 'derbali10@gmail.com',
                  'federico.cali@irbim.cnr.it', 'gabriele.boscolopalo@gmail.com','giacomosardo88@gamil.com',
                  'savasyay@gmail.com')

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
        Sys.sleep(.5)
    }
    df2 <- df[sample(1:nrow(df), 1), ]
    p1 <- p + geom_text(data = df2, aes(x, y, label = participant), size = 5, colour = "blue")
    print(p1)
    ##system(paste0("xcowsay -t 15 ", df2$participant, " will answer!!"))
}

drumroll(nroll = 20)
