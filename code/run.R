
setwd('~/github/chessRooks')
source('code/source.R')


## look at a board
n <- 100
coords <- placeRooks(n)
(free <- numFreeSquares(coords, n))
plotBoard(coords, n)


## run many simulations
runMultipleSims(nValues = seq(1, 50, by = 1), nRuns = 5000)
runMultipleSims(nValues = seq(1, 60, by = 1), nRuns = 20000)
runMultipleSims(nValues = seq(1, 200, by = 1), nRuns = 1000)
runMultipleSims(nValues = seq(50, 200, by = 1), nRuns = 50000)
runMultipleSims(nValues = seq(210, 300, by = 10), nRuns = 50000)
runMultipleSims(nValues = seq(350, 500, by = 50), nRuns = 50000)
runMultipleSims(nValues = seq(600, 1000, by = 50), nRuns = 10000)



##
## remove all existing plots (in plots/ directory)
system('rm plots/*')
##
##
## plot fraction of free squares, against n
nRangeList <- list(c(1,  10),
                   c(3,  50),
                   c(20, 300))
combinedFilename <- 'data/combinedResults.rds'
df <- readRDS(combinedFilename)
for(nRange in nRangeList) {
    indToUse <- df$n >= nRange[1] & df$n <= nRange[2]
    df2 <- df[indToUse, ]
    plot(df2$n, df2$meanFracFree, type = 'l',
         xlab = 'n', ylab = 'mean fraction of free squares',
         main = 'Mean fraction of free (unattacked) squares on n x n board')
    lower <- df2$meanFracFree - 1.96 * sqrt(df2$varFracFree / df2$runs)
    upper <- df2$meanFracFree + 1.96 * sqrt(df2$varFracFree / df2$runs)
    lines(df2$n, lower, lty = 2, col = 'blue')
    lines(df2$n, upper, lty = 2, col = 'blue')
    filename <- paste0('plots/mean_frac_free_n_', nRange[1], '_', nRange[2], '.pdf')
    dev.copy2pdf(device = dev.cur(), file = filename)
}
##
##
## plot variance of fraction of free squares, against n
nRangeList <- list(c(1,  20),
                   c(3,  200),
                   c(20, 300))
combinedFilename <- 'data/combinedResults.rds'
df <- readRDS(combinedFilename)
for(nRange in nRangeList) {
    indToUse <- df$n >= nRange[1] & df$n <= nRange[2]
    plot(df$n[indToUse], df$varFracFree[indToUse], type = 'l',
         xlab = 'n', ylab = 'variance',
         main = 'Variance of fraction of free squares on n x n board')
    filename <- paste0('plots/var_frac_free_n_', nRange[1], '_', nRange[2], '.pdf')
    dev.copy2pdf(device = dev.cur(), file = filename)
}
##
##
## plot density of fraction of free squares, for a given value of n
nValues <- c(120, 150, 200)
for(n in nValues) {
    filename <- paste0('data/sim_n', n, '.rds')
    if(!file.exists(filename)) stop('no simulation results for n = ', n)
    thisList <- readRDS(filename)
    xs <- seq(0, 1, by = 0.001)
    ys <- dnorm(xs, mean(thisList$fracFree), sd(thisList$fracFree))
    plot(density(thisList$fracFree),
         xlab = 'fraction of free squares',
         ylab = 'probability density function',
         main = paste0('Distribution of fraction of free squares, n = ', n))
    lines(xs, ys, col = 'red')
    filename <- paste0('plots/density_frac_free_n_', n, '.pdf')
    dev.copy2pdf(device = dev.cur(), file = filename)
}
##
##
## plot probability of there being >= 1 free (unattacked) square on nxn board
nRangeList <- list(c(1, 10),
                   c(1, 20))
combinedFilename <- 'data/combinedResults.rds'
df <- readRDS(combinedFilename)
for(nRange in nRangeList) {
    indToUse <- df$n >= nRange[1] & df$n <= nRange[2]
    plot(df$n[indToUse], df$probabilitySafeSquare[indToUse], type = 'l',
         xlab = 'n', ylab = 'probability',
         main = 'Probability of at least one free (unattacked) square')
    filename <- paste0('plots/prob_any_free_n_', nRange[1], '_', nRange[2], '.pdf')
    dev.copy2pdf(device = dev.cur(), file = filename)
}



## making plots for manuscript

## FIGURE 1: Percentage of free squares
setEPS()
postscript('ms/plot1_percentage_free.eps', width = 5, height = 5)
combinedFilename <- 'data/combinedResults.rds'
df <- readRDS(combinedFilename)
range <- c(20, 500)
indToUse <- df$n >= range[1] & df$n <= range[2]
df2 <- df[indToUse, ]
indToUse <- df2$n %% 20 == 0
df2 <- df2[indToUse, ]
plot(df2$n, df2$meanFracFree*100, type = 'l',
     xlim = c(0, max(df2$n)),
     xlab = 'n', ylab = 'Percentage of unattacked squares',
     tcl = -0.3, mgp = c(2.5,.6,0),
     main = '', cex.axis = 1.2, cex.lab = 1.4)
abline(h = exp(-2), col = 'red')
dev.off()




## FIGURE 2: Distribution of free squares about the mean
setEPS()
postscript('ms/plot2_fluctuations.eps', width = 5, height = 2.5)
nValues <- c(50, 100, 500)
bwValues <- list(.3, .2, 'nrd0')
par(mfrow = c(1, 3), mai = c(.4, .1, .1, .1))
for(i in 1:3) {
    n <- nValues[i]
    filename <- paste0('data/sim_n', n, '.rds')
    if(!file.exists(filename)) stop('no simulation results for n = ', n)
    thisList <- readRDS(filename)
    percentageFree <- thisList$fracFree * 100
    plot(density(percentageFree, bw = bwValues[[i]]),
         bty = 'o', mgp = c(1.5, 0, 0),
         xlab = paste0('n = ', n), xaxt = 'n',
         ylab = '', yaxt = 'n',
         cex.lab = 1.7,
         main = '')
    xs <- seq(0, 100, by = 0.01)
    ys <- dnorm(xs, mean(percentageFree), sd(percentageFree))
    lines(xs, ys, col = 'red')
}
dev.off()







