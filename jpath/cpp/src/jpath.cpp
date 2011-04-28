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
    this->list = new Node<Object> (NULL);
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
    object->gc_ref_count += 1;
}

void jpath::downref(Object* object)
{
    object->gc_ref_count -= 1;
    if (object->gc_ref_count <= 0)//Time to garbage collect the object
    {
        object->finalize();
        object->unlink();
        delete object->gc_node;
        delete object;
    }
}

jpath::Object::Object()
{
    this->gc_ref_count = 1;
}

void jpath::Object::gc_finalize()
{
}

long long jpath::GCContext::collect()
{
    for (GCNode* node = this->list->next; node != this->list; node = node->next)
    {
        node->value->gc_internal_ref = 0;
        node->value->gc_referenced = false;
    }
    for (GCNode* node = this->list->next; node != this->list; node = node->next)
    {
        node->value->gc_flag_refs();
    }
    GCNode* root_set = new GCNode(NULL);
    for (GCNode* node = this->list->next; node != this->list; node = node->next)
    {
        if (node->value->gc_internal_ref < node->value->gc_ref_count)
            root_set.insert_after(new GCNode(node->value));
    }
    for(GCNode* node = root_set->next; node != root_set; node = root_set->next)
    {
        node->value->gc_referenced = true;
        node->value->gc_add_refs(node);
        delete node;
    }
    delete root_set;
    GCNode* garbage = new GCNode(NULL);
    for(GCNode* node = this->list->next; node != this->list;)
    {
        GCNode* next = node->next;
        if(!node->value->gc_referenced)
        {
            garbage->insert_after(node->value);
            delete node;
        }
        node = next;
    }
    for(GCNode* node = garbage->next; node != garbage; node = node->next)
    {
        node->value->gc_finalize();
    }
    for(GCNode* node = garbage->next; node != garbage; node = garbage->next)
    {
        delete node->value;
        delete node;
    }
    delete garbage;
}

