Atomic types: string, int, decimal, boolean, null

Compound types: list, map

JSQ-specific types: pair

Concept of items really being many items = collections, analog of sequences in XQuery

XPath-like syntax for a lot of stuff. Variables are prefixed with $.

Context is always a single item, or undefined. If something needs the context and the context is undefined, an exception is thrown. When a query starts, the context is implementation-defined; it could be a json document, or a list of all documents in a database, or anything, really.

/ says "take the left-hand collection and, for each item in it, run the right-hand side with that item as the context item. Then take all the resulting collections, flatten them out into one collection, and evaluate to it". / is the tightest binding operator.

[] says "take the stuff before it and, for each item, run the stuff inside the brackets with that item as context. If the result is a true value, evaluate to a collection containing just that particular item. If the result is a false value, evaluate to an empty collection. After running for all items, flatten the resulting collections and evaluate to that."

# is a pattern that says "take the expression to the right, evaluate it, then get the item at that index in the context list. If the context is actually a map, get the value of the item with a key being equal to the string conversion of that value, which allows for referencing items in a map whose names are integers".

@# is similar, but it only works on maps, and it gets the pair representing the matching item instead of the value.

Pattern behavior as I've thought it out thus far would be something like this, where $map holds a map, $pair holds a pair, $list holds a list, and $string holds a string: 

$map/foo gets the value of the entry with key "foo"
$map/@foo gets the pair representing the entry whose key is "foo"
$map/* gets all values in the specified map
$map/@* gets all pairs representing all entries in the specified map
$map/#"false" gets the value of the entry with key "false" as per the behavior of the # sign above. true, false, and null
	ordinarily mean their respective json values, so these have to be escaped like so when used as a pattern.
$pair/key gets the key of a pair
$pair/name also gets the key of a pair
$pair/value gets the value of a pair
$list/#1 gets the first item in the specified list
$list/#(1 to 4) gets the first four items in the specified list
$list/* gets a collection of all items in the specified list
$string/#1 gets the first character of the specified string, as a string of length 1
$string/#(1 to 4) gets the first through fourth characters of the specified string, as a string of length 1
$string/* gets all characters in the specified string, as a collection of 1-character strings



Expressions as I've thought them out would be something like this:

$x, $y, $z (in locations where commas don't otherwise have significance, the only one of which that I can think of right now is function invocation) is a collection containing $x, $y, and $z. Since collections automatically flatten themselves out, ((1, 2), 3, 4, 5) is the same as (1, 2, 3, 4, 5).

[$x] is a list containing the items in the collection $x. Since commas build up a collection, we can recover the usual json notation for creating a list: [1, 2, 3] is a list of the numbers 1, 2, and 3.

$x:$y is a pair with $x as the key and $y as the value. If $x and $y are collections with more than one item, then they must have the same number of items (or an exception will be thrown), and one pair is constructed for each pair of items in $x and $y. If $x and $y both contain no items, then the result is the empty collection.

{$x} is a map containing the pairs in the collection $x. Since : has just higher precedence than the comma used for collection construction, we can recover the usual json notation for creating a map: {"first": 1, "second": 2, "third": 3} is a map containing three pairs as expected in json. If any of the items in $x are themselves maps, all of their pairs will be included into the map. {$x, $y} and {$x/@*, $y/@*} would therefore be equivalent if $x and $y are maps. ($x ++ $y would also be equivalent, but that operator is discussed later.

false, true, and null are their respective json counterparts. These three strings must, as a result, be escaped when used as patterns, which # makes easy to do: they can be escaped as #"false", #"true", and #"null" respectively. This is one of JPath's few major deviations from XQuery philosophy: XQuery has no reserved keywords, while JPath has these three.

$x + $y adds x and y together. They should be numbers.

$x ++ $y concatenates $x and $y. If they're strings, the result is the string concatenation of y onto x. If they're lists, the result is the same as [$x/*, $y/*]. If they're maps, the result is the same as {$x/@*, $y/@*}.

$x × $y multiplies $x and $y, which should be numbers. I may allow for multiplying lists and such later. $x mul $y and $x times $y are provided as equivalents for users without a × on their keyboards.

$x ÷ $y divides $x by $y, both of which should be numbers. I haven't yet decided how precision is going to factor into that and whether they should follow Python's idea of truediv or whether divison should mirror Java's mechanisms instead. $x div $y and $x divided by $y are provided as equivalents for users without a ÷ on their keyboards.

$x and $y is a boolean representing the logical AND of the boolean values of $x and $y.

$x or $y is a boolean representing the logical OR of the boolean values of $x and $y.

$x otherwise $y evaluates to $x if $x is not the empty collection, otherwise it evaluates to $y.

$x = $y is a boolean representing whether any value in the collection $x is equal to a value in collection $y. () = () is false.

$x to $y is a collection of all numbers between, and including, $x and $y.

$x onward is a special value that can be passed to # to tell it to get all items in a list or string from $x onward. It's a sort of infinite collection. I still need to decide on some of the semantics of this, including whether or not the resulting value can be stored in a variable or anything like that.

. is the context node.



The effective boolean value of an object is used in a lot of places. false and the empty collection are false values. All others are true values, although I may move some of those over to the false value list at some point. This, of course, means that the proper way of selecting all maps in a list $foo that have an attribute named "bar" is $foo/*[@bar] instead of $foo/*[bar] because the latter would end up excluding maps where the value of the bar attribute is a false value.



FLWORs. This is a type of expression detailed enough that it warrants its own section. These are largely taken from XQuery with a few changes

for, let, where, and order by clauses can appear in any order in a FLWOR. A FLWOR has to have one, and only one, return statement.







=Recipes=
A map containing all entries in $map except those with keys "password" and "bankaccount":

{$map/@*[key != ("password", "bankaccount")]}


   



 































