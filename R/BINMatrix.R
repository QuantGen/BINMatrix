loadModule("mod_BINMatrix", TRUE)

BINMatrix <- function(path, n, p, type) {
    switch(
        type,
        int = new(BINMatrixInt, path, n, p),
        double = new(BINMatrixDouble, path, n, p),
        char = new(BINMatrixChar, path, n, p)
    )
}
