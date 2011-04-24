For performance reasons, I've decided to re-write the core JPath evaluator in C++. The parser and most of the libraries on top of it will still be written in Python, but the evaluator and the classes representing JSON data will all be written in C++.

I'm thinking this will likely be included as src/jpath/evaluator.so or something. And I'll probably build a DLL of it if I get any Windows users.