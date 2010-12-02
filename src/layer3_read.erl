-module(layer3_read).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("deps/hmhj_layer2/include/l2request.hrl").

init([]) ->
    {ok, undefined};
init([channel]) ->
    {ok, channel}.


to_html(ReqData, State) ->
    Card = list_to_atom(wrq:path_info(card, ReqData)),
    CardRawData = get_card_data(Card),

    Struct = 
	case CardRawData of
	    {readError, Reason} ->
		[{readError, Reason}];
	    {T, V} when State == undefined ->
		[{timestamp, T} | V];
	    {T, Vi} when State == channel ->
		Channel = list_to_atom(wrq:path_info(channel, ReqData)),
		V = select_channel(Channel, Vi),
		[{timestamp, T} | V]
	end,
    CardData = form_card_ds(Card, Struct),
    TotalDS = {struct, CardData},
    Json = mochijson2:encode(TotalDS),
    {Json, ReqData, State}.

%% Returns {Timestamp, [V1, V2, ..., Vn]} or {readError, reason}
get_card_data(Card) ->
    %% in future I may want to keep track of the return value as a
    %% unique ID.

    hmhj_layer2:process_request(
      #l2request{from=self(), to=Card, action=read}),

    receive {R, PList} ->
	    Data = case proplists:get_value(read, PList) of
		       undefined ->
			   {error, no_read_key};
		       BinData ->
			   binary_to_term(BinData)
		   end,
    after 500 ->
	    Data = timeout
    end,
    
    case Data of
	timeout ->
	    {readError, timeout};
	{error, Reason} ->
	    {readError, Reason};
	{Tstamp,VoltageList} ->
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

