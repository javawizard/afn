#include "jpath.h"

using namespace jpath;

Node::Node(Object* value)
{
	this->value = value;
	this->previous = this;
	this->next = this;
}

void Node::insert_before(Node* node)
{
	node->previous = this->previous;
	node->next = this;
	this->previous->next = node;
	this->previous = node;
}

void Node::insert_after(Node* node)
{
	node->next = this->next;
	node->previous = this;
	this->next->previous = node;
	this->next = node;
}

Node::~Node()
{
	this->next->previous = this->previous;
	this->previous->next = this->next;
}
