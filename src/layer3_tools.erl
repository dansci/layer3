-module(layer3_tools).

-export([struct_to_json/1]).

struct_to_json({struct, [{Key, Val}, Rest]}) ->
    "{" ++ "\"" ++ binary_to_list(Key) ++ "\"" ++ ":" ++ 
	struct_to_json(Val) ++ struct_to_json(Rest) ++ "},";
struct_to_json({struct, [{Key, Val}]}) ->
    "{" ++ "\"" ++ binary_to_list(Key) ++ "\"" ++  ":" ++
	struct_to_json(Val) ++ "},";
struct_to_json({Key, Val}) ->
    "\"" ++ binary_to_list(Key) ++ "\"" ++ ":" ++
	struct_to_json(Val);
struct_to_json(Val) when is_integer(Val) ->
    integer_to_list(Val) ++ ",";
struct_to_json(Val) when is_float(Val) ->
    float_to_list(Val) ++ ",";
struct_to_json(Val) when is_binary(Val) ->
    binary_to_list(Val) ++ ",".

