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
        V read(Rcpp::IntegerVector i);
        M read(Rcpp::IntegerVector i, Rcpp::IntegerVector j);
        void write(unsigned long long int, T);
        void write(unsigned int, unsigned int, T);
        unsigned int get_n();
        unsigned int get_p();
    private:
        unsigned int n;
        unsigned int p;
        unsigned long long int length;
        std::fstream fs;
        T read(unsigned long long int);
        unsigned long long int reduce_indexes(unsigned int, unsigned int);
        bool check_bounds(unsigned long long int index);
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
    // Check if index is out of bounds.
    if (Rcpp::is_true(Rcpp::any(i > n * p))) {
        Rcpp::stop("Invalid dimensions.");
    }
    // Convert to zero-based index.
    i = i - 1;
    // Keep size of i.
    unsigned int size_i = i.size();
    // Reserve output vector.
    V out (size_i);
    // Iterate over indexes.
    for (unsigned int idx_i = 0; idx_i < size_i; ++idx_i) {
        out(idx_i) = read(i[idx_i]);
    }
    return out;
}

template <typename T, typename M, typename V>
M BINMatrix<T, M, V>::read(Rcpp::IntegerVector i, Rcpp::IntegerVector j) {
    // Check if indexes are out of bounds.
    if (Rcpp::is_true(Rcpp::any(i > n)) || Rcpp::is_true(Rcpp::any(j > p))) {
        Rcpp::stop("Invalid dimensions.");
    }
    // Convert to zero-based index.
    i = i - 1;
    j = j - 1;
    // Keep sizes of i and j.
    unsigned int size_i = i.size();
    unsigned int size_j = j.size();
    // Reserve output matrix.
    M out (size_i, size_j);
    // Iterate over row indexes.
    for (unsigned int idx_i = 0; idx_i < size_i; ++idx_i) {
        // Iterate over column indexes.
        for (unsigned int idx_j = 0; idx_j < size_j; ++idx_j) {
            out(idx_i, idx_j) = read(reduce_indexes(i[idx_i], j[idx_j]));
        }
    }
    return out;
}

template <typename T, typename M, typename V>
void BINMatrix<T, M, V>::write(unsigned long long index, T value) {
    check_bounds(index);
    fs.seekp(index * sizeof(T));
    fs.write(reinterpret_cast<char*>(&value), sizeof(T));
    fs.flush();
};

template <typename T, typename M, typename V>
void BINMatrix<T, M, V>::write(unsigned int i, unsigned int j, T value) {
    // Convert to zero-based index.
    --i;
    --j;
    write(reduce_indexes(i, j), value);
};

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
unsigned long long int BINMatrix<T, M, V>::reduce_indexes(unsigned int i, unsigned int j) {
    return ((i * n) + j);
}

template <typename T, typename M, typename V>
bool BINMatrix<T, M, V>::check_bounds(unsigned long long int index) {
    if (index >= n * p) {
        throw std::out_of_range("index is out of range");
    }
}

RCPP_MODULE(mod_BINMatrix) {

    using namespace Rcpp ;

    Rcpp::IntegerVector (BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::*read_int_vector)(Rcpp::IntegerVector) = &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::read;
    Rcpp::IntegerMatrix (BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::*read_int_matrix)(Rcpp::IntegerVector, Rcpp::IntegerVector) = &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::read;
    void (BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::*write_int_1)(unsigned long long int, int) = &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::write;
    void (BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::*write_int_2)(unsigned int, unsigned int, int) = &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::write;
    class_<BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>>("BINMatrixInt")
    .constructor<std::string, unsigned int, unsigned int>()
    .method("read", read_int_vector)
    .method("read", read_int_matrix)
    .method("write", write_int_1)
    .method("write", write_int_2)
    .property("n", &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::get_n)
    .property("p", &BINMatrix<int, Rcpp::IntegerMatrix, Rcpp::IntegerVector>::get_p)
    ;

    Rcpp::NumericVector (BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::*read_double_vector)(Rcpp::IntegerVector) = &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::read;
    Rcpp::NumericMatrix (BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::*read_double_matrix)(Rcpp::IntegerVector, Rcpp::IntegerVector) = &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::read;
    void (BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::*write_double_1)(unsigned long long int, double) = &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::write;
    void (BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::*write_double_2)(unsigned int, unsigned int, double) = &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::write;
    class_<BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>>("BINMatrixDouble")
    .constructor<std::string, unsigned int, unsigned int>()
    .method("read", read_double_vector)
    .method("read", read_double_matrix)
    .method("write", write_double_1)
    .method("write", write_double_2)
    .property("n", &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::get_n)
    .property("p", &BINMatrix<double, Rcpp::NumericMatrix, Rcpp::NumericVector>::get_p)
    ;

    Rcpp::CharacterVector (BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>::*read_char_vector)(Rcpp::IntegerVector) = &BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>::read;
    Rcpp::CharacterMatrix (BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>::*read_char_matrix)(Rcpp::IntegerVector, Rcpp::IntegerVector) = &BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>::read;
    void (BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>::*write_char_1)(unsigned long long int, char) = &BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>::write;
    void (BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>::*write_char_2)(unsigned int, unsigned int, char) = &BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>::write;
    class_<BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>>("BINMatrixChar")
    .constructor<std::string, unsigned int, unsigned int>()
    .method("read", read_char_vector)
    .method("read", read_char_matrix)
    .method("write", write_char_1)
    .method("write", write_char_2)
    .property("n", &BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>::get_n)
    .property("p", &BINMatrix<char, Rcpp::CharacterMatrix, Rcpp::CharacterVector>::get_p)
    ;

}
