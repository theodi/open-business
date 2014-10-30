setwd("~/git/open-business")
library(ggplot2)
theme_set(theme_minimal(base_family = "Helvetica Neue", base_size = 18))

# Read GitHub files
source_https <- function(url, ...) {
  require(RCurl)
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

source_https('https://raw.githubusercontent.com/theodi/R-projects/master/ODI-colours.R')

options(stringsAsFactors = FALSE)

sectors <- read.csv("data/master-sectors.csv")

# THIS WAS PAINFUL
sectors$Var1 <- reorder(sectors$Var1, sectors$Freq)

ggplot(sectors, aes(y = Freq, x = Var1)) + geom_bar(stat = "identity", fill = odi_turquoise) + coord_flip() +
  geom_hline(yintercept = seq(25, 100, 25), col = "white", size = 1.5) +
  geom_text(aes(label = Freq, y = - 2.5), stat = "identity", color = "black", size = 4) + 
  xlab("") + ylab("") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank())
ggsave("graphics/sector-counts.png", height = 4, width = 12)

