-module(hmhj_layer3_read).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined};
init([channel]) ->
    {ok, channel}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% to_json(ReqData, State) ->
%%     Card = list_to_atom(wrq:path_info(card, ReqData)),
%%     CardRawData = get_card_data(Card),

%%     Struct = 
%% 	case CardRawData of
%% 	    {readError, Reason} ->
%% 		[{readError, Reason}];
%% 	    {T, V} when State == undefined ->
%% 		[{timestamp, T} | V];
%% 	    {T, Vi} when State == channel ->
%% 		Channel = list_to_atom(wrq:path_info(channel, ReqData)),
%% 		V = select_channel(Channel, Vi),
%% 		[{timestamp, T} | V]
%% 	end,
%%     CardData = form_card_ds(Card, Struct),
%%     TotalDS = {struct, CardData},
%%     Json = mochijson2:encode(TotalDS),
%%     {Json, ReqData, State}.

to_json(ReqData, State) ->
    Card = list_to_atom(wrq:path_info(card, ReqData)),
    hmhj_layer3_utils:query_card(Card, read),
    RawData = hmhj_layer3_utils:receive_data(read),
    DS = {struct, [{Card, hmhj_layer3_utils:form_ds(read, RawData)}]},
    {mochijson2:encode(DS), ReqData, State}.

%% Returns {Timestamp, [V1, V2, ..., Vn]} or {readError, reason}
get_card_data(Card) ->
    {ok, Req0} = hmhj_layer2:new_request(),
    {ok, Req1} = hmhj_layer2:set_dest(Req0, Card),
    {ok, Req2} = hmhj_layer2:set_action(Req1, read),
    Req = Req2,
    
    hmhj_layer2:process_request(Req),

    receive {_R, PList} ->
    	    Data = case proplists:get_value(read, PList) of
		       undefined ->
			   {error, no_read_key};
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
	[Tstamp|VoltageList] ->
	    {Ms, S, _us} = Tstamp,
	    Timestamp = 1000000*Ms + S,
	    %% Coerce voltages
	    NumberedVoltages = number_voltages(VoltageList),
	    {Timestamp, channels_to_struct(NumberedVoltages)}
    end.

%% Returns [Vm] from [V1, V2, ..., Vm, ..., Vn] given arg m
select_channel(Channel, Voltages) ->
    Data = proplists:lookup(Channel, Voltages),
    case Data of
	none ->
	    [{readError, no_such_channel}];
	_other ->
	    [Data]
    end.
	    

%% Forms a data structure for card data containing the supplied
%% timestamp and list of voltages
form_card_ds(Card, Struct) ->
    [{Card,
      {struct, Struct}}].

number_voltages(V) -> 
    lists:zip(lists:seq(1, length(V)), V).
    
channels_to_struct(V) ->
    lists:map(fun structify_channels/1, V).

structify_channels({N, V}) ->
    %% the appending may be inefficient; I don't know a better way
    {list_to_atom("channel" ++ integer_to_list(N)),
     {struct, [{voltage, V}]}}.

