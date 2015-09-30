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

template <typename T>
class BINMatrix {
    public:
        BINMatrix(std::string, unsigned int, unsigned int);
        T read(unsigned long long int);
        T read(unsigned int, unsigned int);
        void write(unsigned long long int, T);
        void write(unsigned int, unsigned int, T);
        unsigned int get_n();
        unsigned int get_p();
    private:
        unsigned int n;
        unsigned int p;
        unsigned long long int length;
        std::fstream fs;
        unsigned long long int reduce_indexes(unsigned int, unsigned int);
        bool check_bounds(unsigned long long int index);
};

template <typename T>
BINMatrix<T>::BINMatrix(std::string path, unsigned int n_, unsigned int p_) : n(n_), p(p_), length(n * p * sizeof(T)) {
    if (!file_exists(path)) {
        allocate_file(path, length);
    }
    fs.open(path, std::ios::in | std::ios::out | std::ios::binary);
    if (!check_length(fs, length)) {
        throw std::length_error("dimensions do not match file length");
    }
};

template <typename T>
T BINMatrix<T>::read(unsigned long long int index) {
    check_bounds(index);
    fs.seekg(index * sizeof(T));
    T buffer;
    fs.read(reinterpret_cast<char*>(&buffer), sizeof(T));
    return buffer;
};

template <typename T>
T BINMatrix<T>::read(unsigned int i, unsigned int j) {
    return read(reduce_indexes(i, j));
};

template <typename T>
void BINMatrix<T>::write(unsigned long long index, T value) {
    check_bounds(index);
    fs.seekp(index * sizeof(T));
    fs.write(reinterpret_cast<char*>(&value), sizeof(T));
    fs.flush();
};

template <typename T>
void BINMatrix<T>::write(unsigned int i, unsigned int j, T value) {
    write(reduce_indexes(i, j), value);
};

template <typename T>
unsigned int BINMatrix<T>::get_n() {
    return n;
};

template <typename T>
unsigned int BINMatrix<T>::get_p() {
    return p;
};

template <typename T>
unsigned long long int BINMatrix<T>::reduce_indexes(unsigned int i, unsigned int j) {
    // Convert to zero-based index
    --i; --j;
    // Convert two-dimensional to one-dimensional index
    return ((i * n) + j);
};

template <typename T>
bool BINMatrix<T>::check_bounds(unsigned long long int index) {
    if (index >= n * p) {
        throw std::out_of_range("index is out of range");
    }
};

RCPP_MODULE(mod_BINMatrix) {

    using namespace Rcpp ;

    int (BINMatrix<int>::*read_int_1)(unsigned long long int) = &BINMatrix<int>::read;
    int (BINMatrix<int>::*read_int_2)(unsigned int, unsigned int) = &BINMatrix<int>::read;
    void (BINMatrix<int>::*write_int_1)(unsigned long long int, int) = &BINMatrix<int>::write;
    void (BINMatrix<int>::*write_int_2)(unsigned int, unsigned int, int) = &BINMatrix<int>::write;
    class_<BINMatrix<int>>("BINMatrixInt")
    .constructor<std::string, unsigned int, unsigned int>()
    .method("read", read_int_1)
    .method("read", read_int_2)
    .method("write", write_int_1)
    .method("write", write_int_2)
    .property("n", &BINMatrix<int>::get_n)
    .property("p", &BINMatrix<int>::get_p)
    ;

    double (BINMatrix<double>::*read_double_1)(unsigned long long int) = &BINMatrix<double>::read;
    double (BINMatrix<double>::*read_double_2)(unsigned int, unsigned int) = &BINMatrix<double>::read;
    void (BINMatrix<double>::*write_double_1)(unsigned long long int, double) = &BINMatrix<double>::write;
    void (BINMatrix<double>::*write_double_2)(unsigned int, unsigned int, double) = &BINMatrix<double>::write;
    class_<BINMatrix<double>>("BINMatrixDouble")
    .constructor<std::string, unsigned int, unsigned int>()
    .method("read", read_double_1)
    .method("read", read_double_2)
    .method("write", write_double_1)
    .method("write", write_double_2)
    .property("n", &BINMatrix<double>::get_n)
    .property("p", &BINMatrix<double>::get_p)
    ;

    char (BINMatrix<char>::*read_char_1)(unsigned long long int) = &BINMatrix<char>::read;
    char (BINMatrix<char>::*read_char_2)(unsigned int, unsigned int) = &BINMatrix<char>::read;
    void (BINMatrix<char>::*write_char_1)(unsigned long long int, char) = &BINMatrix<char>::write;
    void (BINMatrix<char>::*write_char_2)(unsigned int, unsigned int, char) = &BINMatrix<char>::write;
    class_<BINMatrix<char>>("BINMatrixChar")
    .constructor<std::string, unsigned int, unsigned int>()
    .method("read", read_char_1)
    .method("read", read_char_2)
    .method("write", write_char_1)
    .method("write", write_char_2)
    .property("n", &BINMatrix<char>::get_n)
    .property("p", &BINMatrix<char>::get_p)
    ;

}
