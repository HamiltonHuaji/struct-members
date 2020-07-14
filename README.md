# struct::members

本项目为c++提供现在还比较弱的静态反射能力, 基于cppast, 为习作项目.

编译后可以得到可执行文件`struct_members`

在你的文件中(假设叫`neko.cpp`)写如下代码

```cpp
#include "inspect.hpp"

// ...

#ifndef analysis
#define analysis
namespace inspect{
#include "neko.reflect"
}
#endif

```

并执行

```shell
./struct_members neko.cpp -D analysis > neko.reflect
```

即可愉快使用以下语法

```cpp
some_namespace::some_templated_class<some_template_args> v;
auto c = inspect::type(v);
std::any u = c.get(v, "some_member_variable");

template <typename T>
struct getter {
    static void get(T *t) {}
};
template <>
struct getter<char> {
    static void get(char *t) {
        std::cout << *t << "\n";
    }
};
c.get<getter>(v, "some_member_variable");
```
