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
    };
    
    class Object
    {
        public:
            long long ref_count;
            long long internal_ref;
            bool referenced;
            Node<Object>* gc_node;
    };
    
    class GCContext
    {
        public:
            Node<Object>* list;

            GCContext();
            ~GCContext();
            
            void track(Object* object);
    };
}

