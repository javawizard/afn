#include "jpath.h"
#include "stdio.h"

using namespace jpath;

int main()
{
    Node<const char*>* list = new Node<const char*>(NULL);
    list->insert_before(new Node<const char*>((const char*)"First item"));
    list->insert_before(new Node<const char*>("Second item"));
    list->insert_before(new Node<const char*>("Third item"));
    list->insert_before(new Node<const char*>("Fourth item"));
    Node<const char*>* node;
    for(node = list->next; node != list; node = node->next)
    {
        printf("%s\n", node->value);
    }
    printf("Removing second item\n");
    delete list->next->next;
    for(node = list->next; node != list; node = node->next)
    {
        printf("%s\n", node->value);
    }
}
