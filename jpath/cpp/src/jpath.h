namespace jpath
{
	class Object;

	class Node
	{
		public:
			Node* next;
			Node* previous;
			Object* value;
			Node(Object* value);
			~Node();
			void insert_before(Node* node);
			void insert_after(Node* node);
	};
}
