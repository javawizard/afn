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
bool Node<T>::has_items()
{
    return this->next != this;
}

template <class T>
bool Node<T>::is_head()
{
    return this->value == NULL;
}

template <class T>
Node<T>::~Node()
{
    if (!this->is_head())
        this->remove();
}

jpath::GCContext::GCContext()
{
    this->list = new Node<Object> ();
}

jpath::GCContext::~GCContext()
{
    while (this->list->has_items())
    {
        GCNode* node = this->list->next;
        node->value->gc_node = NULL;
        delete node;
    }
    delete this->list;
}

void jpath::GCContext::track(Object *object)
{
    GCNode* node = new GCNode(object);
    this->list->insert_after(node);
    object->gc_node = node;
}

void jpath::upref(Object* object)
{
    object->ref_count += 1;
}

void jpath::downref(Object* object)
{
    object->ref_count -= 1;
    if (object->ref_count <= 0)//Time to garbage collect the object
    {
        object->finalize();
        object->unlink();
        delete object->gc_node;
        delete object;
    }
}

