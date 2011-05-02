#include <cstddef>
#include "GCNode.h"

namespace jpath
{
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

using namespace jpath;

jpath::GCContext::GCContext()
{
    this->list = new GCNode(NULL);
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
            root_set->insert_after(new GCNode(node->value));
    }
    for (GCNode* node = root_set->next; node != root_set; node = root_set->next)
    {
        node->value->gc_referenced = true;
        node->value->gc_add_refs(node);
        delete node;
    }
    delete root_set;
    GCNode* garbage = new GCNode(NULL);
    for (GCNode* node = this->list->next; node != this->list;)
    {
        GCNode* next = node->next;
        if (!node->value->gc_referenced)
        {
            garbage->insert_after(new GCNode(node->value));
            delete node;
        }
        node = next;
    }
    for (GCNode* node = garbage->next; node != garbage; node = node->next)
    {
        node->value->gc_finalize();
    }
    for (GCNode* node = garbage->next; node != garbage; node = garbage->next)
    {
        delete node->value;
        delete node;
    }
    delete garbage;
}

