#include <fstream>

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
    private:
        unsigned int n;
        unsigned int p;
        unsigned long long int length;
        std::fstream fs;
        unsigned long long int reduce_indexes(unsigned int, unsigned int);
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
    index *= sizeof(T);
    fs.seekg(index);
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
    index *= sizeof(T);
    fs.seekp(index);
    fs.write(reinterpret_cast<char*>(&value), sizeof(T));
    fs.flush();
};

template <typename T>
void BINMatrix<T>::write(unsigned int i, unsigned int j, T value) {
    write(reduce_indexes(i, j), value);
};

template <typename T>
unsigned long long int BINMatrix<T>::reduce_indexes(unsigned int i, unsigned int j) {
    // Convert to zero-based index
    --i; --j;
    // Convert two-dimensional to one-dimensional index
    return ((i * n) + j);
};

int main() {

    try {

        // char
        //BINMatrix<char> m_char("test", 6, 6);
        //m_char.write(1, 1, 'a');
        //m_char.write(1, 6, 'b');
        //m_char.write(2, 1, 'c');
        //m_char.write(6, 6, 'z');
        //std::cout << m_char.read(1, 1) << std::endl;
        //std::cout << m_char.read(1, 6) << std::endl;
        //std::cout << m_char.read(2, 1) << std::endl;
        //std::cout << m_char.read(6, 6) << std::endl;

        // int
        //BINMatrix<int> m_int("test", 6, 6);
        //m_int.write(1, 1, 1);
        //m_int.write(1, 6, 2);
        //m_int.write(2, 1, 3);
        //m_int.write(6, 6, 99);
        //std::cout << m_int.read(1, 1) << std::endl;
        //std::cout << m_int.read(1, 6) << std::endl;
        //std::cout << m_int.read(2, 1) << std::endl;
        //std::cout << m_int.read(6, 6) << std::endl;

        //double
        BINMatrix<double> m_double("test", 6, 6);
        m_double.write(1, 1, 1.1);
        m_double.write(1, 6, 2.2);
        m_double.write(2, 1, 3.3);
        m_double.write(6, 6, 99.99);
        std::cout << m_double.read(1, 1) << std::endl;
        std::cout << m_double.read(1, 6) << std::endl;
        std::cout << m_double.read(2, 1) << std::endl;
        std::cout << m_double.read(6, 6) << std::endl;

    } catch (std::length_error& ex) {
        std::cerr << ex.what() << std::endl;
    }

}
