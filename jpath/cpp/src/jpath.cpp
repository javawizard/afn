#include "jpath.h"

using namespace jpath;

template <class T>
Node<T>::Node(T* value)
{
    this->value = value;
    this->previous = this;
    this->next = this;
}

template <class T>
void Node<T>::insert_before(Node* node)
{
    node->previous = this->previous;
    node->next = this;
    this->previous->next = node;
    this->previous = node;
}

template <class T>
void Node<T>::insert_after(Node* node)
{
    node->next = this->next;
    node->previous = this;
    this->next->previous = node;
    this->next = node;
}

template <class T>
void Node<T>::remove()
{
    this->next->previous = this->previous;
    this->previous->next = this->next;
}

template <class T>
Node<T>::~Node()
{
    this->remove();
}

