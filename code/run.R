
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










