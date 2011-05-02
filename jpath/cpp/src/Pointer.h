#include <cstddef>

namespace jpath
{
    template <class T>
    class Pointer
    {
        public:
            Pointer(T*);
            ~Pointer();

            T* operator->();
            T& operator*();
            T* get();

        private:
            T* value;
    };
}

template <class T>
jpath::Pointer<T>::Pointer(T* value)
{
    this->value = value;
    upref(value);
}

template <class T>
jpath::Pointer<T>::~Pointer()
{
    downref(this->value);
}

template <class T>
T* jpath::Pointer<T>::get()
{
    return this->value;
}

template <class T>
T* jpath::Pointer<T>::operator ->()
{
    return this->value;
}

template <class T>
T& jpath::Pointer<T>::operator *()
{
    return *(this->value);
}

