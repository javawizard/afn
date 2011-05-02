#include <cstddef>

namespace jpath
{
    class GCNode;
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
}

using namespace jpath;

jpath::Object::Object()
{
    this->gc_ref_count = 1;
}

void jpath::Object::gc_finalize()
{
}

