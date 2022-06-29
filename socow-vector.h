#pragma once
#include <cstddef>
#include <algorithm>

template <typename T, size_t SMALL_SIZE>
struct socow_vector {
  using iterator = T*;
  using const_iterator = T const*;

  socow_vector() : size_(0), is_small_(true) {}

  socow_vector(socow_vector const& other) : size_(other.size_), is_small_(other.is_small_) {
    if (size_ <= SMALL_SIZE) {
      safe_copy(static_storage_, other.static_storage_, size_);
      is_small_ = true;
    } else {
      dynamic_storage_ = other.dynamic_storage_;
      dynamic_storage_->ref_count++;
    }
  }

  socow_vector& operator=(socow_vector const& other) {
    if (this == &other) {
      return *this;
    }
    socow_vector(other).swap(*this);
    return *this;
  }

  ~socow_vector() {
    buf_delete_();
  }

  T& operator[](size_t i) {
    return data()[i];
  }

  T const& operator[](size_t i) const {
    return data()[i];
  }

  T* data() {
    check_cow_();
    return is_small_ ? static_storage_ : dynamic_storage_->data;
  }

  T const* data() const {
    return const_data_();
  }

  size_t size() const {
    return size_;
  }

  T& front() {
    return data()[0];
  }

  T const& front() const {
    return data()[0];
  }

  T& back() {
    return data()[size_ - 1];
  }

  T const& back() const {
    return data()[size_ - 1];
  }

  void push_back(T const& el) {
    if (is_small_) {
      if (size_ == SMALL_SIZE) {
        transform_push_back_(el);
      } else {
        new(static_storage_ + size_) T(el);
      }
    } else if (dynamic_storage_->ref_count == 1) {
      push_back_dynamic_(el);
    } else {
      if (size_ != capacity()) {
        copy_on_write_();
      }
      push_back_dynamic_(el);
    }
    size_++;
  }

  void pop_back() {
    data()[--size_].~T();
  }

  bool empty() const {
    return size_ == 0;
  }

  size_t capacity() const {
    return is_small_ ? SMALL_SIZE : dynamic_storage_->capacity;
  }

  void reserve(size_t new_capacity) {
    if (new_capacity <= capacity()) {
      check_cow_();
      return;
    }
    dynamic_buf* new_buf = allocate(new_capacity);
    safe_copy(new_buf->data, data(), size_);
    buf_delete_();
    dynamic_storage_ = new_buf;
    is_small_ = false;
  }

  void shrink_to_fit() {
    if (size_ == capacity() || is_small_) {
      return;
    }
    if (size_ <= SMALL_SIZE) {
      dynamic_buf* init_dyn_storage = dynamic_storage_;
      try {
        safe_copy(static_storage_, dynamic_storage_->data, size_);
      } catch (...) {
        dynamic_storage_ = init_dyn_storage;
        throw;
      }
      dyn_buf_delete(init_dyn_storage, size_);
      is_small_ = true;
    } else {
      dynamic_buf* new_buf = allocate(size_);
      safe_copy(new_buf->data, dynamic_storage_->data, size_);
      buf_delete_();
      dynamic_storage_ = new_buf;
    }
  }

  void clear() {
    check_cow_();
    safe_clear(data(), size_);
    size_ = 0;
  }

  void swap(socow_vector& other) {
    if (is_small_) {
      if (other.is_small_) {
        swap_small_(other);
      } else {
        swap_different_(other);
      }
    } else {
      if (other.is_small_) {
        other.swap_different_(*this);
      } else {
        std::swap(dynamic_storage_, other.dynamic_storage_);
      }
    }
    std::swap(size_, other.size_);
    std::swap(is_small_, other.is_small_);
  }

  iterator begin() {
    return data();
  }

  iterator end() {
    return data() + size_;
  }

  const_iterator begin() const {
    return data();
  }

  const_iterator end() const {
    return data() + size_;
  }

  iterator insert(const_iterator pos, T const& el) {
    size_t it_shift = pos - const_data_();
    push_back(el);
    for (size_t i = size_ - 1; i > it_shift; i--) {
      std::swap(data()[i], data()[i - 1]);
    }
    return data() + it_shift;
  }
  
  iterator erase(const_iterator pos) {
    return erase(pos, pos + 1);
  }

  iterator erase(const_iterator first, const_iterator last) {
    size_t it_shift_left = first - const_data_();
    size_t it_shift_right = last - const_data_();
    size_t diff = last - first;
    check_cow_();
    for (size_t i = it_shift_right; i < size_; i++) {
      std::swap(data()[i - diff], data()[i]);
    }
    for (size_t i = 0; i < diff; i++) {
      pop_back();
    }
    return data() + it_shift_left;
  }

private:
  struct dynamic_buf {
    size_t capacity;
    size_t ref_count;
    T data[0];
  };

  void swap_small_(socow_vector &other) {
    for (size_t i = 0; i < std::min(size_, other.size_); i++) {
      std::swap(static_storage_[i], other.static_storage_[i]);
    }
    if (other.size_ > size_) {
      safe_copy(static_storage_ + size_,other.static_storage_ + size_,other.size_ - size_);
      safe_clear(other.static_storage_ + size_, other.size_ - size_);
    } else {
      safe_copy(other.static_storage_ + other.size_, static_storage_ + other.size_,size_ - other.size_);
      safe_clear(static_storage_ + other.size_, size_ - other.size_);
    }
  }

  void swap_different_(socow_vector& other) {
    dynamic_buf* init_dyn_buf = other.dynamic_storage_;
    try {
      safe_copy(other.static_storage_, static_storage_, size_);
    } catch (...) {
      other.dynamic_storage_ = init_dyn_buf;
      throw;
    }
    buf_delete_();
    dynamic_storage_ = init_dyn_buf;
  }

  void extend_push_back_(T const& el, size_t new_capacity) {
    dynamic_buf* new_buf = allocate(new_capacity);
    try {
      safe_copy(new_buf->data, const_data_(), size_);
    } catch (...) {
      operator delete(new_buf);
      throw;
    }
    try {
      new(new_buf->data + size_) T(el);
    } catch (...) {
      safe_clear(new_buf->data, size_);
      operator delete(new_buf);
      throw;
    }
    buf_delete_();
    dynamic_storage_ = new_buf;
  }

  void transform_push_back_(T const& el) {
    extend_push_back_(el, 2 * size_ + 1);
    is_small_ = false;
  }

  void push_back_dynamic_(T const& el) {
    if (size_ == dynamic_storage_->capacity) {
      extend_push_back_(el, 2 * dynamic_storage_->capacity + 1);
    } else {
      new(dynamic_storage_->data + size_) T(el);
    }
  }

  void copy_on_write_() {
    dynamic_buf* new_buf = allocate(capacity());
    try {
      safe_copy(new_buf->data, dynamic_storage_->data, size_);
    }
    catch (...) {
      operator delete(new_buf);
      throw;
    }
    dynamic_storage_->ref_count--;
    dynamic_storage_ = new_buf;
  }

  static void dyn_buf_delete(dynamic_buf* buf, size_t size) {
    if (buf->ref_count > 1) {
      buf->ref_count--;
    } else {
      safe_clear(buf->data, size);
      operator delete(buf);
    }
  }

  void buf_delete_() {
    if (is_small_) {
      safe_clear(static_storage_, size_);
    } else {
      dyn_buf_delete(dynamic_storage_, size_);
    }
  }

  void check_cow_() {
    if (!is_small_ && dynamic_storage_->ref_count > 1) {
      copy_on_write_();
    }
  }

  T const* const_data_() const {
      return is_small_ ? static_storage_ : dynamic_storage_->data;
  }

  static void safe_clear(T* src, size_t amount) {
    for (size_t i = amount; i > 0; i--) {
      src[i - 1].~T();
    }
  }

  static void safe_copy(T* dest, T const* src, size_t amount) {
    size_t copied = 0;
    try {
      for (size_t i = 0; i < amount; i++) {
        new(dest + i) T(src[i]);
        copied++;
      }
    }
    catch (...) {
      safe_clear(dest, copied);
      throw;
    }
  }

  static dynamic_buf* allocate(size_t cap) {
    auto* result = static_cast<dynamic_buf*>(operator new(sizeof(dynamic_buf) + sizeof(T) * cap));
    new(result) dynamic_buf{cap, 1};
    return result;
  }

  size_t size_;
  bool is_small_;
  union {
    T static_storage_[SMALL_SIZE];
    dynamic_buf* dynamic_storage_;
  };
};
