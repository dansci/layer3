-module(layer3_tools).

-export([struct_to_json/1]).

struct_to_json([]) ->
    "";

struct_to_json({struct, [{Key, Val}]}) ->
    "{" ++ "\"" ++ binary_to_list(Key) ++ "\"" ++  ":" ++
	struct_to_json(Val) ++ "}";

%% struct_to_json({struct, [{Key, Val} | Rest]}) ->
%%     "{" ++ "\"" ++ binary_to_list(Key) ++ "\"" ++  ":" ++
%% 	struct_to_json(Val) ++ "}," ++ struct_to_json(Rest);

struct_to_json({struct, [{Key, Val} | Rest]}) ->
    "{" ++ "\"" ++ binary_to_list(Key) ++ "\"" ++  ":" ++
	struct_to_json(Val) ++ "," ++ struct_to_json(Rest) ++ "}";

struct_to_json([{Key, Val}]) ->
    "\"" ++ binary_to_list(Key) ++ "\"" ++ ":" ++
	struct_to_json(Val);

struct_to_json([{Key, Val} | Rest]) ->
    struct_to_json([{Key, Val}]) ++"," ++ struct_to_json(Rest);

struct_to_json({Key, Val}) ->
    "\"" ++ binary_to_list(Key) ++ "\"" ++ ":" ++
	struct_to_json(Val);

struct_to_json(Val) when is_integer(Val) ->
    integer_to_list(Val);
struct_to_json(Val) when is_float(Val) ->
    float_to_list(Val);
struct_to_json(Val) when is_binary(Val) ->
    binary_to_list(Val);
struct_to_json(Val) when is_atom(Val) ->
    atom_to_list(Val).




%% To test this stupid parser:

%% DS = {struct, [{<<"cardA">>,
%% 		{struct, [{<<"channel1">>,
%% 			   {struct, [{<<"voltage">>, 8.2}]}},
%% 			  {<<"timestamp">>, 128988392},
%% 			  {<<"channel2">>,
%% 			   {struct, [{<<"voltage">>, 2.3}]}}]}},
%% 	       {<<"NTPLink">>, true},
%% 	       {<<"cardB">>,
%% 		{struct, [{<<"timestamp">>, 128988390}]}}]}.
      
