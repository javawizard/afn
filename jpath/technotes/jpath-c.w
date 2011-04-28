For performance reasons, I've decided to re-write the core JPath evaluator in C++. The parser and most of the libraries on top of it will still be written in Python, but the evaluator and the classes representing JSON data will all be written in C++.

Haven't decided how to include it yet. Architecture first...

So... I'm going to have an auto-generator that takes a file specifying the AST and generates representative classes. These will all subclass from some common type, I'm thinking AST or something. All classes inheriting from AST will have a field specifying what type they are. This will be a value from an enumeration with one entry corresponding to each production. The parser itself will still be written in Python, and I'm thinking it will use CTypes to call into this library and create instances of the AST classes.

Each AST class has a destructor that frees all of the AST nodes it references.

The syntax of ast.txt, the file that classes are generated from, is a bunch of lines starting with "AstNodeName:", where AstNodeName is the name of the class and node that will be generated.

If there aren't any values that the production needs (EmptyCollectionConstructor, for example, doesn't need any), then the line should consist only of the string "AstNodeName". Otherwise, after "AstNodeName:" comes a comma-separated list of values for the production.

Each value is of the form "type name", where type is the type of the production, optionally surrounded by brackets to indicate a list of that particular type, and name is the name of the field that will hold that production. Valid types are:

	ast: Another AST node
	string: A string of text. This will be represented as a value of type std::string.

I'll add more types as the need arises.

Each AST class has a function, left undefined by the generator, called evaluate. This function is called to evaluate an instance of that particular AST node. I'll then write implementations of this method for each AST node the generator generates classes for.

That's it for the AST representation. Now for the representation of JSON datatypes and values within the JPath interpreter.

==GC==
Quick intermediate note on memory management.

JPath will use a combination of two garbage collecting algorithms: reference counting and mark-and-sweep.

Because of JPath's design, reference cycles are extremely rare (and using JPath with just JSON datatypes and nothing else, reference cycles are impossible to create), but objects will be created and destroyed quite frequently. This is why I'm planning on using both collection approaches.

So, there will be some sort of garbage collection context. Inside the JPath interpreter, I expect that to be tied to the context representing a single invocation of a query, but it'll be separate to allow for objects to potentially live beyond that.

I'm also thinking I may add some macros to allow JPath to be combined with multi-thread support. This would likely slow down performance, but it would potentially speed up performance if lots of processors are available, and even if it doesn't, I like letting people do what they want as long as they know what they're doing. Multi-thread support would only be included if JPath were compiled with JPATH_GC_THREADSAFE defined, and it would function by including a lock in the GC context that upref, downref, and the mark-and-sweep collector all acquire during their operation. (Actually, upref may not need to be locked; I need to think this through and make sure.)

So, the context must last as long as the objects it constitutes, or whatever deletes the context must be responsible for deleting the objects as well.

Garbage collection context objects store a linked list of objects under control of that context. The linked list is structured as an instance of the class ListNode, which is a doubly-linked list node. The list is stored in the GC context as a ListNode whose item is NULL, and which points to the first and last items in the list. Each ListNode has three fields: next, which is a pointer to the next ListNode in the list, previous, which is a pointer to the previous ListNode in the list, and item, which is a pointer to the item that it references.

The ListNode corresponding to a particular item is stored in the item. When downref is called on an item and it results in the item's reference count going to zero, the item's finalize function is called (which tells the item to perform any cleanup it needs to do before collection happens), the item's unlink function is called (which tells the item to downref any other items it references so that they will eventually be garbage collected), the item's ListNode is removed from the GC context's object list, and the item and its corresponding ListNode are deleted.

Each item has two fields in addition to its ref_count field that are relevant to garbage collection: internal_ref and referenced, which are integers and booleans, respectively.

When a full collection is to be performed, four iterations are performed over the object set. I hope to optimize this a bit at some point, but since currently mark-and-sweep collections are rare, I haven't worried about this much yet. The iterations are:

On the first iteration, every object's internal_ref and referenced fields are set to 0 and false, respectively.

On the second iteration, objects are asked to increment the internal_ref fields of all objects that they reference (by calling the item's gc_flag_ref method). Those objects whose internal_ref fields are now less than their ref_count fields constitute the root set.

A linked list, one that uses ListNode objects, is then constructed. The third iteration then commences. It consists of adding all objects whose internal_ref fields are less than their ref_count fields (the root set, as described in the previous paragraph) to this newly-constructed linked list.

We then take a break from iterations and focus on this linked list that represents the root set. We have a loop that runs on the first item in the list until the list is empty. This loop marks the current item as referenced and asks the item to add all of the items it references to the list after the node representing this item (it passes in the node representing the current item to allow the item to do this; the method is gc_add_refs). It then deletes the node representing the current item, and moves on. Once this has finished, all items referenceable, directly or indirectly, from the root set have been marked as referenced.

We then perform the fourth, and last, iteration. On this iteration, we create another linked list and copy all objects not marked as referenced into this list, and we remove all objects not marked as referenced from the main list.

Then we iterate over this list of non-referenced objects twice. On the first iteration of this list, we call the finalize methods of all objects in it. On the second iteration, we delete each item, and then the corresponding linked list node, and we're done with garbage collection!

So I think I'm going to take a crack at implementing this to see if it actually works. I'll come back to design of the rest of the system in a bit, since I think I've got most of the rest of the system worked out.
















 