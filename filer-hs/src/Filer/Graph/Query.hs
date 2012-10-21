
module Filer.Graph.Query where
import Data.ByteString.Lazy (ByteString)

data ValueQuery
    = IntValueQuery IntQuery
    | BoolValueQuery BoolQuery
    | StringValueQuery StringQuery
    | BinaryValueQuery BinaryQuery
    | AndV ValueQuery ValueQuery
    | OrV ValueQuery ValueQuery
    | NotV ValueQuery ValueQuery
    | AnyValue

data IntQuery
    = IntGreaterThan Integer
    | IntGreaterOrEqual Integer
    | IntLessThan Integer
    | IntLessOrEqual Integer
    -- | First int is min, second int is max
    | IntInRange Integer Integer
    | IntEqualTo Integer
    | AnyInt

data BoolQuery
    = BoolEqualTo Bool
    | AnyBool

data StringQuery
    = StringEqualTo String
    -- TODO: Add additional things for seeing if the string in question is
    -- contained within an attribute's value or things like that, or maybe even
    -- support regexes
    | AnyString

data BinaryQuery
    = BinaryEqualTo ByteString
    -- TODO: Add length queries, so that we can search for attributes with
    -- large values
    | AnyBinary

data RefQuery
    = RefHasAttributes AttributeQuery
    | PointsTo ObjectQuery
    | PointsFrom ObjectQuery
    | AndR RefQuery RefQuery
    | OrR RefQuery RefQuery
    | NotR RefQuery

data AttributeQuery
    = HasAttribute String ValueQuery
    | AndA AttributeQuery AttributeQuery
    | OrA AttributeQuery AttributeQuery
    | NotA AttributeQuery

data ObjectQuery
    = HasIncomingRef RefQuery
    | HasOutgoingRef RefQuery
    | ObjectHasAttributes AttributeQuery
    | HashIs Hash
    | AndO ObjectQuery ObjectQuery
    | OrO ObjectQuery ObjectQuery
    | NotO ObjectQuery

    
    
    
    
    
    
    
    