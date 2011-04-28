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
            long long ref_count;
            long long internal_ref;
            bool referenced;
            Node<Object>* gc_node;
    };
    
    typedef Node<Object> GCNode;
    
    class GCContext
    {
        public:
            GCNode* list;

            GCContext();
            ~GCContext();
            
            void track(Object* object);
    };
}

