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
            T* value;

            Node(T* value);
            ~Node();

            void insert_before(Node<T>* node);
            void insert_after(Node<T>* node);
            void remove();
            bool has_items();
            bool is_head();
    };
    
    class Object
    {
        public:
            long long gc_ref_count;
            long long gc_internal_ref;
            bool gc_referenced;
            Node<Object>* gc_node;
            
            Object();
            
            virtual void gc_flag_refs() = 0;
            virtual void gc_add_refs(GCNode* node) = 0; // Make sure to add ref'd nodes AFTER the one specified
            virtual void gc_finalize();
            virtual void gc_unlink() = 0;
    };
    
    typedef Node<Object> GCNode;
    
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

