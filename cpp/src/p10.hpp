#pragma once

#include <vector>


template<class T>
T last(const std::vector<T>& xs) { return xs.back(); }

template<class T>
T but_last(const std::vector<T>& xs) {
    return last({xs.begin(), xs.end() - 1}); 
}


template<class T>
T element_at(const std::vecto<T>& xs, std::size_t ix) {
    return xs.at(ix - 1);
}


template<class T>
std::size_t length(const std::vector<T>& xs) {
    return xs.size();
}


template<class T>
std::vector<T> reverse(const std::vector<T>& xs) {
    return { xs.end(), xs.begin() };
}
