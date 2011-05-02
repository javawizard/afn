#include "jpath.h"

using namespace jpath;

void jpath::upref(Object* object)
{
    object->gc_ref_count += 1;
}

void jpath::downref(Object* object)
{
    object->gc_ref_count -= 1;
    if (object->gc_ref_count <= 0)//Time to garbage collect the object
    {
        object->gc_finalize();
        object->gc_unlink();
        delete object->gc_node;
        delete object;
    }
}

