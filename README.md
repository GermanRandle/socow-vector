# vector with small-object and copy-on-write optimizations

(С++ course homework)

In this task, you need to implement a class similar to `std::vector`, but with *small-object* and *copy-on-write* optimizations.

*small-object* implies that the vector can store a small number of elements without dynamic memory allocation. *copy-on-write* implies that copying/assigning large vectors does not copy all elements themselves, but postpones copying of the elements until a modifying operation is applied to the object.

The class to be implemented should be named `socow_vector` and should be in the header `socow-vector.h`. It should have two template parameters: the type of the objects stored and the size of the small buffer.

```cpp
template <typename T, size_t SMALL_SIZE>
struct socow_vector;
```

Due to the presence of *small-object* and *copy-on-write* optimizations, some operations have different computational complexity and/or provide different guarantees of exception safety:

* Copy constructor and assignment operator should work for `O(SMALL_SIZE)`, not `O(size)`.
* If the sizes of both `a` and `b` are less than `SMALL_SIZE`, `swap(a, b)` should provide the basic guarantee of exception safety, otherwise – the strong guarantee.
* If the sizes of both `a` and `b` are less than `SMALL_SIZE`, `a = b` should provide the basic guarantee of exception safety, otherwise – the strong guarantee.
* Non-constant operations like `operator[]`, `data()`, `front()`, `back()`, `pop_back()`, `begin()`, `end()` should work for O(size) and satisfy the strong guarantee of exception safety if copying for *copy-on-write* is required, and for O(1) and nothrow otherwise.
* Just like with the standard vector, `reserve` should guarantee that after executing `reserve(n)`, insertions into the vector will not lead to reallocations as long as the size <= `n`.
