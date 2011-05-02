#include <cstddef>

namespace jpath
{
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
            void clear();
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
void jpath::Node<T>::clear()
{
    jpath::Node<T>* current = this->next;
    while (current != this)
    {
        delete current;
        current = this->next;
    }
}

template <class T>
jpath::Node<T>::~Node()
{
    if (!this->is_head())
        this->remove();
}
