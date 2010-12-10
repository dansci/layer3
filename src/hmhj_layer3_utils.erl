-module(hmhj_layer3_utils).
-export([query_card/2, receive_data/1, form_ds/3, form_config_resp/1]).

%% Card = cardA | cardB | cardC | cardD
%% Action = read | status
%% queries the card-- to get data use receive_data/1
query_card(Card, Action) ->
    {ok, Req0} = hmhj_layer2:new_request(),
    {ok, Req1} = hmhj_layer2:set_dest(Req0, Card),
    {ok, Req2} = hmhj_layer2:set_action(Req1, Action),
    Req = Req2,
    hmhj_layer2:process_request(Req).

%% TODO: unify configuration block with the Action block
%% Action = read | status | configure
%% returns {error, no_such_key} | {error, timeout} | Term
receive_data(configure) ->
    receive {_R, Payload} ->
		   case Payload of
		Binary when is_binary(Binary) ->
		    binary_to_term(Binary);
		Term ->
		    Term
	    end
    after 500 ->
	    {error, timeout}
    end;

receive_data(Action) ->
    receive {_R, PList} ->
	    case proplists:get_value(Action, PList) of
		undefined ->
		    {error, no_such_key};
		Binary when is_binary(Binary) ->
		    binary_to_term(Binary);
		Term ->
		    Term
	    end
    after 500 ->
	    {error, timeout}
    end.

%% Takes the raw return sent back by hmhj_layer2:process_request and
%% turns it into a data structure mochijson2 can understand.

%%  TODO hasn't been tested on errors yet...  This first clause will
%%  probably have to call process_error or sth like that.
form_ds(_Action, {error, Reason}, _Card) ->
    {struct, [{error, {struct, [error_to_ds(Reason)]}}]};
form_ds(read, Term, Card) ->
    {struct, [{Card, 
	       data_to_ds(Term)}]};
form_ds(status, Term, Card) ->
    {struct, [{Card, 
	       config_to_ds(Term)}]}.

%% turns card data into mochijson2 language
data_to_ds([{Ms, S, _us}|VList]) ->
    Timestamp = 1000000*Ms + S,
    NumVList = lists:zip(lists:seq(1, length(VList)), VList),
    {struct, data_to_ds(NumVList, [{timestamp, Timestamp}])}.

data_to_ds([], Acc) ->
    lists:reverse(Acc);
data_to_ds([{N, V}|Rest], Acc) ->
    data_to_ds(Rest,
	       [{channel(N), {struct, [{voltage, V}]}}|Acc]).

%% turns config data into mochijson2 language
config_to_ds(Cfg) ->
    {struct, config_to_ds(Cfg, [])}.

config_to_ds([], Acc) ->
    lists:reverse(Acc);
config_to_ds([{gain, []}|Rest], Acc) ->
    config_to_ds(Rest, Acc);
config_to_ds([{gain, List}|Rest], Acc) ->
    [{Ch, Gain}|OtherGains] = List,
    config_to_ds([{gain, OtherGains}|Rest], 
		 [{channel(Ch), {struct, [{gain, Gain}]}}|Acc]);
config_to_ds([{Parameter, Value}|Rest], Acc) ->
    config_to_ds(Rest, [{Parameter, Value}|Acc]).

form_config_resp(L1resp) ->
    form_config_resp(L1resp, []).

form_config_resp([], []) ->
    ok;
form_config_resp([], Acc) ->
    {struct, [{error, {struct, lists:reverse(Acc)}}]};
form_config_resp([{_Req, ok}|Rest], Acc) ->
    form_config_resp(Rest, Acc);
form_config_resp([{Req, Bin}|Rest], Acc) when is_binary(Bin)->
    form_config_resp([{Req, binary_to_term(Bin)}|Rest], Acc);
form_config_resp([{_Req, {error, Reason}}|Rest], Acc) ->
    form_config_resp(Rest, [error_to_ds(Reason)|Acc]).
			
error_to_ds({nocard, Slot}) ->
    {details, {struct, [{nocard, Slot}]}};
error_to_ds({unsupported_method, {Model, {configure, Par}}}) ->
    {details, {struct, [{unsupported_method, configure}, 
     {model, Model}, {parameter, Par}]}};
error_to_ds({unsupported_method, {Model, Method}}) ->
    {details, {struct, [{unsupported_method, Method}, {model, Model}]}};
error_to_ds({bad_value, {write, Val}}) ->
    {details, {struct, [{bad_value, Val}]}};
error_to_ds({bad_value, {configure, {Model, {Par, Val}}}}) ->
    {details, {struct, [{bad_value, Val}, 
			{parameter, Par}, {model, Model}]}};
error_to_ds({bad_value, {configure, {Par, Val}}}) ->
    {details, {struct, [{bad_value, Val}, 
			{parameter, Par}]}};
error_to_ds(Atom) when is_atom(Atom) ->
    {details, {struct, [{error, Atom}]}}.

%% Trivial helper function
channel(N) when is_integer(N) ->
    list_to_atom("channel"++integer_to_list(N)).
