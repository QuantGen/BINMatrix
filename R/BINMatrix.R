package.env <- environment()

loadModule("mod_BINMatrix", TRUE)

BINMatrix <- function(path, n, p, type) {
    switch(
        type,
        int = new(BINMatrixInt, path, n, p),
        double = new(BINMatrixDouble, path, n, p),
        char = new(BINMatrixChar, path, n, p)
    )
}

.dim <- function(x) {
    c(x$n, x$p)
}

evalqOnLoad({
    assign(paste0("dim.", BINMatrixInt), .dim, package.env)
    assign(paste0("dim.", BINMatrixDouble), .dim, package.env)
    assign(paste0("dim.", BINMatrixChar), .dim, package.env)
})
