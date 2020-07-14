#ifndef inspect
#define inspect
#include <any>
#include <array>
#include <concepts>
#include <iostream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>
namespace inspect {

template <typename T>
struct __class__ {
    static constexpr const char *dir[] = {};
    std::any get(T &t, std::string name);
    template <template <typename V> class callback_t>
    void get(T &t, std::string name);
};

template <typename T>
__class__<T> type_impl() {
    return __class__<T>();
}

template <typename T>
__class__<T> type(T t) { return type_impl<std::remove_cv_t<T>>(); }

} // namespace inspect
#endif