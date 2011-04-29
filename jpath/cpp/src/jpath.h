#include <cstddef>

namespace jpath
{
    class Object;
    
    void downref(Object* object);
    void upref(Object* object);
    
    template <class T>
    class Node
    {
        public:
            Node<T>* next;
            Node<T>* previous;
            T value;

            Node(T value);
            ~Node();

            void insert_before(Node<T>* node);
            void insert_after(Node<T>* node);
            void remove();
            bool has_items();
            bool is_head();
    };
    
    typedef Node<Object*> GCNode;
    
    class Object
    {
        public:
            long long gc_ref_count;
            long long gc_internal_ref;
            bool gc_referenced;
            GCNode* gc_node;

            Object();

            virtual void gc_flag_refs() = 0;
            virtual void gc_add_refs(GCNode* node) = 0; // Make sure to add ref'd nodes AFTER the one specified
            virtual void gc_finalize();
            virtual void gc_unlink() = 0;
    };
    
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
    
    class GCContext
    {
        public:
            GCNode* list;

            GCContext();
            ~GCContext();

            void track(Object* object);
            long long collect();
    };

}

template <class T>
jpath::Node<T>::Node(T value)
{
    this->value = value;
    this->previous = this;
    this->next = this;
}

template <class T>
void jpath::Node<T>::insert_before(Node* node)
{
    node->previous = this->previous;
    node->next = this;
    this->previous->next = node;
    this->previous = node;
}

template <class T>
void jpath::Node<T>::insert_after(Node* node)
{
    node->next = this->next;
    node->previous = this;
    this->next->previous = node;
    this->next = node;
}

template <class T>
void jpath::Node<T>::remove()
{
    this->next->previous = this->previous;
    this->previous->next = this->next;
}

template <class T>
bool jpath::Node<T>::has_items()
{
    return this->next != this;
}

template <class T>
bool jpath::Node<T>::is_head()
{
    return this->value == NULL;
}

template <class T>
jpath::Node<T>::~Node()
{
    if (!this->is_head())
        this->remove();
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

