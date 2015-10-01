package.env <- environment()

loadModule("mod_BINMatrix", TRUE)

BINMatrix <- function(path, n, p, type) {
    switch(
        type,
        int = new(BINMatrixInt, path, n, p),
        double = new(BINMatrixDouble, path, n, p)
    )
}

.dim <- function(x) {
    c(x$n, x$p)
}

`.[` <- function(x, i, j, drop) {
    dims <- dim(x)
    n <- dims[1]
    p <- dims[2]
    if (nargs() > 2) {
        # Case [i, j]
        if (missing(i)) {
            i <- 1:n
        } else if (class(i) == 'logical') {
            i <- which(rep_len(i, n))
        } else if (class(i) == 'character') {
            i <- sapply(i, function (name) {
                which(rownames(x) == name)
            }, USE.NAMES=FALSE)
        }
        if (missing(j)) {
            j <- 1:p
        } else if (class(j) == 'logical') {
            j <- which(rep_len(j, p))
        } else if (class(j) == 'character') {
            j <- sapply(j, function (name) {
                which(colnames(x) == name)
            }, USE.NAMES=FALSE)
        }
        subset <- x$read(i, j)
        # Let R handle drop behavior.
        if(drop == TRUE && (nrow(subset) == 1 || ncol(subset) == 1)) {
            subset <- subset[,]
        }
    } else {
        if (missing(i)) {
            # Case []
            i <- 1:n
            j <- 1:p
            subset <- x$read(i, j)
        } else {
            # Case [i]
            if (class(i) == 'matrix') {
                i <- as.vector(i)
                if (class(i) == 'logical') {
                    i <- which(rep_len(i, n * p))
                    # matrix treats NAs as TRUE
                    i <- sort(c(i, which(is.na(x[]))))
                }
            } else {
                if (class(i) == 'logical') {
                    i <- which(rep_len(i, n * p))
                }
            }
            subset <- x$read(i)
        }
    }
    return(subset)
}

`.[<-` <- function(x, i, j, ..., value) {
    if (missing(i)) {
        i <- 1:nrow(x)
    }
    if (missing(j)) {
        j <- 1:ncol(x)
    }
    m <- matrix(nrow = length(i), ncol = length(j), data = value)
    x$write(i, j, m)
    return(x)
}

evalqOnLoad({

    assign(paste0("dim.", BINMatrixInt), .dim, package.env)
    assign(paste0("dim.", BINMatrixDouble), .dim, package.env)

    setMethod('[', signature(x = BINMatrixInt), `.[`)
    setMethod('[', signature(x = BINMatrixDouble), `.[`)

    setReplaceMethod('[', signature(x = BINMatrixInt), `.[<-`)
    setReplaceMethod('[', signature(x = BINMatrixDouble), `.[<-`)

})
