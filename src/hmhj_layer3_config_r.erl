-module(hmhj_layer3_config_r).
-export([init/1,
	 allowed_methods/2,
	 content_types_provided/2,
	 to_json/2
	]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) ->
    {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.


to_json(ReqData, State) ->
    Card = list_to_atom(wrq:path_info(card, ReqData)),
    CardRawConfig = get_config_data(Card),
    DS = {struct, [{Card, config_to_ds(CardRawConfig)}]},
    io:format("ds is ~p~n", [DS]),
    {mochijson2:encode(DS), ReqData, State}.
				
get_config_data(Card) ->
    {ok, Req0} = hmhj_layer2:new_request(),
    {ok, Req1} = hmhj_layer2:set_dest(Req0, Card),
    {ok, Req2} = hmhj_layer2:set_action(Req1, status),
    Req = Req2,
    
    hmhj_layer2:process_request(Req),
    
    receive {_R, PList} ->
    	    Data = case proplists:get_value(status, PList) of
		       undefined ->
			   {error, no_status_key};
		       BinData ->
			   binary_to_term(BinData)
		   end
    after 500 ->
	    Data = timeout
    end,

    case Data of
	timeout ->
	    {readError, timeout};
	{error, Reason} ->
	    {readError, Reason};
	List ->
	    List
    end.


%% config_to_ds(Cfg) ->
%%     {struct, config_to_ds(Cfg, [])}.

 config_to_ds(Cfg) ->
     {struct, config_to_ds(Cfg, [])}.

config_to_ds([], Acc) ->
    Acc;
config_to_ds([{gain, []}|Rest], Acc) ->
    config_to_ds(Rest, Acc);
config_to_ds([{gain, List}|Rest], Acc) ->
    [{Ch, Gain}|OtherGains] = List,
    config_to_ds([{gain, OtherGains}|Rest], 
		 [{channel(Ch), {struct, [{gain, Gain}]}}|Acc]);
config_to_ds([{Parameter, Value}|Rest], Acc) ->
    config_to_ds(Rest, [{Parameter, Value}|Acc]).

channel(N) when is_integer(N) ->
    list_to_atom("channel"++integer_to_list(N)).
