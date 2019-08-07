

## change (x,y) board coordinates into
## scalar "square ID" between 1.... n^2
coordsToID <- function(coords, n) {
    coords[1] + (coords[2]-1)*n
}

## change scalar "square ID" between 1.... n^2
## into (x,y) board coordinates
idToCoords <- function(id, n) {
    x <- id %% n
    if(x == 0) x <- n
    c(x, ceiling(id/n))
}

## return an nx2 array of "rook coordinates",
## resulting from randomly placing n rooks on an nxn board
placeRooks <- function(n, seed) {
    if(!missing(seed)) set.seed(seed)
    ids <- sample(1:(n^2), n)
    coords <- array(dim = c(n,2))
    coords[,1] <- ids %% n
    coords[,1][coords[,1] == 0] <- n
    coords[,2] <- ceiling(ids / n)
    coords
}

## given nx2 array of "rook coordinates" on a board,
## return the number of "free" (unattacked) squares on the board
numFreeSquares <- function(coords, n) {
    freeXs <- setdiff(1:n, coords[,1])
    freeYs <- setdiff(1:n, coords[,2])
    length(freeXs) * length(freeYs)
}

## rough plotting, showing nxn board, the locations
## of the rooks (specified by "coords") in black,
## and the "free" (unattacked) squares as small red dots,
## not necessarily fast or efficient
plotBoard <- function(coords, n) {
    numFree <- numFreeSquares(coords, n)
    plot(-1000, -1000, xlim = c(0,n+1), ylim = c(0,n+1), xlab = '', ylab = '', bty = 'n',
         main = paste0('n = ', n, ', numFree = ', numFree))
    segments(x0=  0.5, y0=  0.5, x1=n+0.5, y1=  0.5, lwd = 3)
    segments(x0=n+0.5, y0=  0.5, x1=n+0.5, y1=n+0.5, lwd = 3)
    segments(x0=n+0.5, y0=n+0.5, x1=  0.5, y1=n+0.5, lwd = 3)
    segments(x0=  0.5, y0=n+0.5, x1=  0.5, y1=  0.5, lwd = 3)
    points(x = coords[,1], y = coords[,2], pch = 4)
    freeXs <- setdiff(1:n, coords[,1])
    freeYs <- setdiff(1:n, coords[,2])
    ptype <- ifelse(n < 100, 1, '.')
    for(i in freeXs)
        for(j in freeYs)
            points(i, j, col = 'red', pch = ptype)
}

## load any existing simulation results for value of n,
## run an additional nRuns simulations,
## update the loaded simulation results with these nRuns results,
## and save back to the data file: data/sim_n{NVALUE}.rds
runSim <- function(n, nRuns, loadExisting = TRUE, dir = 'data') {
    filename <- paste0(dir, '/sim_n', n, '.rds')
    if(loadExisting && file.exists(filename)) {
        currentList <- readRDS(filename)
    } else {
        currentList <- list(n = n,
                            runs = 0,
                            numFree = numeric(),
                            fracFree = numeric(),
                            probabilitySafeSquare = NA)
    }
    if(currentList$n != n) stop('values of n do not match')
    newNumFree <- numeric(nRuns)
    for(i in seq_len(nRuns)) {
        coords <- placeRooks(n)
        newNumFree[i] <- numFreeSquares(coords, n)
    }
    currentList$runs <- currentList$runs + nRuns
    currentList$numFree <- c(currentList$numFree, newNumFree)
    currentList$fracFree <- currentList$numFree / (n^2)
    currentList$probabilitySafeSquare <- sum(currentList$numFree > 0) / currentList$runs
    saveRDS(currentList, filename)
    ## also write a .csv file, with the number of free squares from each sim
    tempDF <- data.frame(numFree = currentList$numFree)
    write.csv(tempDF, file = paste0('csv/sim_n', currentList$n, '.csv'), row.names = FALSE)
}

## combine all data/sim_n{NVALUE}.rds into one
## single file: combinedResults.rds, containing a data frame
combineAllResults <- function(dir = 'data', print = TRUE) {
    simFilenames <- list.files(dir, pattern = '^sim_n.+\\.rds', full.names = TRUE)
    nFiles <- length(simFilenames)
    df <- data.frame(n = rep(-1, nFiles),
                     runs = -1,
                     meanFracFree = -1,
                     varFracFree = -1,
                     probabilitySafeSquare = -1)
    for(i in 1:nFiles) {
        thisList <- readRDS(simFilenames[i])
        df[i, 'n']                      <- thisList$n
        df[i, 'runs']                   <- thisList$runs
        df[i, 'meanFracFree']           <- mean(thisList$fracFree)
        df[i, 'varFracFree']            <- var(thisList$fracFree)
        df[i, 'probabilitySafeSquare']  <- thisList$probabilitySafeSquare
    }
    ## sort this data frame based on increasing value of n
    ix <- sort(df$n, index.return = TRUE)$ix
    dfSorted <- df[ix,]
    rownames(dfSorted) <- 1:nrow(dfSorted)
    saveRDS(dfSorted, file = paste0(dir, '/combinedResults.rds'))
    write.csv(dfSorted, file = 'csv/combinedResults.csv', row.names = FALSE)
    if(print) print(dfSorted)
}

## run many simulations,
## loading and saving results,
## ranging over all values given in nValues, for n,
## and running nRuns simulations for each value of n.
## optionally, combine all sim_n...rds files into combinedResults.rds
runMultipleSims <- function(nValues, nRuns, dir = 'data', combineResults = TRUE, print = TRUE) {
    for(n in nValues) {
        if(print) message('n = ', n, ' ...')
        runSim(n = n, nRuns = nRuns, dir = dir)
    }
    if(combineResults)
        combineAllResults(dir = dir, print = print)
}




