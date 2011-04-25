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

I'm thinking that for now, JPath will only use reference counting. It will track a list of objects created during the process of running a query so that any objects that somehow manage to get involved in a reference cycle will be removed at the end of the query.

Each invocation of a query will have some associated context that lives the entire life of the query. This context will be used to track all objects that get created during the life of the query invocation.

When an object (all objects tracked by the garbage collector must be instance of item or one of its subclasses) is created, its reference count is set to 1 and the object is added to the context's object list.

The object's reference count can then be modified as needed with UPREF(item) and DOWNREF(item).

When the object's reference count hits zero, the object is removed from the context's object list, all of the objects that it references (as per a function it provides that makes available a list of such objects) have their reference counts decremented, and the object is then destroyed.

The context's object list will be implemented using a std::set. The value type will be Item*, with the result that the set will sort itself based on the memory location at which the item is stored.

After a query has finished running, the result of the query is treated as a root set and a mark-and-sweep collection is performed on the object list.

TODO: figure out what order to destroy objects in there in etc and let objects not register themselves on the list if they don't want

















 