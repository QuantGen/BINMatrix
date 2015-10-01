#include <fstream>
#include <Rcpp.h>

bool file_exists(std::string& path) {
    std::ifstream file(path);
    return file.good();
}

void allocate_file(std::string& path, unsigned long long int size = 0) {
    std::ofstream file(path);
    if (size > 0) {
        file.seekp(size - 1);
        file << '\0';
    }
}

bool check_length(std::fstream& fs, unsigned long long int length) {
    fs.seekg(0, fs.end);
    bool length_matches = fs.tellg() == length;
    fs.seekg(0, fs.beg);
    return length_matches;
}

template <typename T, typename M, typename V>
class BINMatrix {
    public:
        BINMatrix(std::string, unsigned int, unsigned int);
        V read(Rcpp::IntegerVector);
        M read(Rcpp::IntegerVector, Rcpp::IntegerVector);
        void write(Rcpp::IntegerVector, Rcpp::IntegerVector, M);
        unsigned int get_n();
        unsigned int get_p();
    private:
        unsigned int n;
        unsigned int p;
        unsigned long long int length;
        std::fstream fs;
        T read(unsigned long long int);
        void write(unsigned long long int, T);
        unsigned long long int reduce_indexes(unsigned int, unsigned int);
        bool check_bounds(Rcpp::IntegerVector&);
        bool check_bounds(Rcpp::IntegerVector&, Rcpp::IntegerVector&);
};

template <typename T, typename M, typename V>
BINMatrix<T, M, V>::BINMatrix(std::string path, unsigned int n_, unsigned int p_) : n(n_), p(p_), length(n * p * sizeof(T)) {
    if (!file_exists(path)) {
        allocate_file(path, length);
    }
    fs.open(path, std::ios::in | std::ios::out | std::ios::binary);
    if (!check_length(fs, length)) {
        throw std::length_error("dimensions do not match file length");
    }
}

template <typename T, typename M, typename V>
V BINMatrix<T, M, V>::read(Rcpp::IntegerVector i) {
    check_bounds(i);
    // Convert to zero-based index.
    i = i - 1;
    // Reserve output vector.
    V out (i.size());
    // Iterate over indexes.
    for (unsigned int idx_i = 0; idx_i < i.size(); ++idx_i) {
        out(idx_i) = read(i[idx_i]);
    }
    return out;
}

template <typename T, typename M, typename V>
M BINMatrix<T, M, V>::read(Rcpp::IntegerVector i, Rcpp::IntegerVector j) {
    check_bounds(i, j);
    // Convert to zero-based index.
    i = i - 1; j = j - 1;
    // Reserve output matrix.
    M out (i.size(), j.size());
    // Iterate over row indexes.
    for (unsigned int idx_i = 0; idx_i < i.size(); ++idx_i) {
        // Iterate over column indexes.
        for (unsigned int idx_j = 0; idx_j < j.size(); ++idx_j) {
            out(idx_i, idx_j) = read(reduce_indexes(i[idx_i], j[idx_j]));
        }
    }
    return out;
}

template <typename T, typename M, typename V>
void BINMatrix<T, M, V>::write(Rcpp::IntegerVector i, Rcpp::IntegerVector j, M value) {
    check_bounds(i, j);
    // Convert to zero-based index.
    i = i - 1; j = j - 1;
    // Iterate over row indexes.
    for (unsigned int idx_i = 0; idx_i < i.size(); ++idx_i) {
        // Iterate over column indexes.
        for (unsigned int idx_j = 0; idx_j < j.size(); ++idx_j) {
            write(reduce_indexes(i[idx_i], j[idx_j]), value(idx_i, idx_j));
        }
    }
}

template <typename T, typename M, typename V>
unsigned int BINMatrix<T, M, V>::get_n() {
    return n;
}

template <typename T, typename M, typename V>
unsigned int BINMatrix<T, M, V>::get_p() {
    return p;
}

template <typename T, typename M, typename V>
T BINMatrix<T, M, V>::read(unsigned long long int index) {
    fs.seekg(index * sizeof(T));
    T buffer;
    fs.read(reinterpret_cast<char*>(&buffer), sizeof(T));
    return buffer;
}

template <typename T, typename M, typename V>
void BINMatrix<T, M, V>::write(unsigned long long index, T value) {
    fs.seekp(index * sizeof(T));
    fs.write(reinterpret_cast<char*>(&value), sizeof(T));
    fs.flush();
}

template <typename T, typename M, typename V>
unsigned long long int BINMatrix<T, M, V>::reduce_indexes(unsigned int i, unsigned int j) {
    return ((i * n) + j);
}

template <typename T, typename M, typename V>
bool BINMatrix<T, M, V>::check_bounds(Rcpp::IntegerVector& i) {
    if (Rcpp::is_true(Rcpp::any(i > n * p))) {
        Rcpp::stop("Invalid dimensions.");
    }
}

template <typename T, typename M, typename V>
bool BINMatrix<T, M, V>::check_bounds(Rcpp::IntegerVector& i, Rcpp::IntegerVector& j) {
    if (Rcpp::is_true(Rcpp::any(i > n)) || Rcpp::is_true(Rcpp::any(j > p))) {
        Rcpp::stop("Invalid dimensions.");
    }
}

RCPP_MODULE(mod_BINMatrix) {

    using namespace Rcpp ;

    Rcpp::IntegerVector (BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::*read_int_vector)(Rcpp::IntegerVector) = &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::read;
    Rcpp::IntegerMatrix (BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::*read_int_matrix)(Rcpp::IntegerVector, Rcpp::IntegerVector) = &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::read;
    void (BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::*write_int)(Rcpp::IntegerVector, Rcpp::IntegerVector, Rcpp::IntegerMatrix) = &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::write;
    class_<BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>>("BINMatrixInt")
    .constructor<std::string, unsigned int, unsigned int>()
    .method("read", read_int_vector)
    .method("read", read_int_matrix)
    .method("write", write_int)
    .property("n", &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::get_n)
    .property("p", &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::get_p)
    ;

    Rcpp::NumericVector (BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::*read_double_vector)(Rcpp::IntegerVector) = &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::read;
    Rcpp::NumericMatrix (BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::*read_double_matrix)(Rcpp::IntegerVector, Rcpp::IntegerVector) = &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::read;
    void (BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::*write_double)(Rcpp::IntegerVector, Rcpp::IntegerVector, Rcpp::NumericMatrix) = &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::write;
    class_<BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>>("BINMatrixDouble")
    .constructor<std::string, unsigned int, unsigned int>()
    .method("read", read_double_vector)
    .method("read", read_double_matrix)
    .method("write", write_double)
    .property("n", &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::get_n)
    .property("p", &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::get_p)
    ;

}
