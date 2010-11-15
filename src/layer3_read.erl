-module(layer3_read).
-export([init/1, to_html/2]).


-include_lib("webmachine/include/webmachine.hrl").
-include_lib("deps/hmhj_layer2/include/l2request.hrl").

init([]) ->
    {ok, undefined};
init([channel]) ->
    {ok, channel}.


to_html(ReqData, undefined) ->
    Card = list_to_atom(wrq:path_info(card, ReqData)),
    CardRawData = get_card_data(Card),

    case CardRawData of
	{readError, Reason} ->
	    CardData = form_error_ds(Card, Reason);
	{T, V} ->
	    CardData = form_card_ds(Card, {T, V})
    end,	    
    TotalDS = {struct, CardData},
    Json = mochijson2:encode(TotalDS),
    {Json, ReqData, undefined};

to_html(ReqData, channel) ->
    Channel = list_to_atom(
		wrq:path_info(channel, ReqData)),
    Card = list_to_atom(wrq:path_info(card, ReqData)),
    CardRawData = get_card_data(Card),

    case CardRawData of
	{readError, Reason} ->
	    CardData = form_error_ds(Card, Reason);
	{T, Vi} ->
	    V = select_channel(Channel, Vi),
	    CardData = form_card_ds(Card, {T, V})
    end,
    TotalDS = {struct, CardData},
    Json = mochijson2:encode(TotalDS),
    {Json, ReqData, undefined}.

structify_channels({N, V}) ->
    %% the appending may be inefficient; I don't know a better way
    {list_to_atom("channel" ++ integer_to_list(N)),
     {struct, [{voltage, V}]}}.

%% Returns {Timestamp, [V1, V2, ..., Vn]} or {readError, reason}
get_card_data(Card) ->
    hmhj_layer2:process_request(
      #l2request{from=self(), to=Card, action=read}),

    receive {R, BinData} ->
	    Data = binary_to_term(BinData)
    end,

    case Data of
	{error, Reason} ->
	    CardData = {readError, Reason};
	{Tstamp,VoltageList} ->
	    {Ms, S, _us} = Tstamp,
	    Timestamp = 1000000*Ms + S,
	    %% Coerce voltages
	    NumberedVoltages = lists:zip(lists:seq(1, 
						   length(VoltageList)),
					 VoltageList),
	    CardData = {Timestamp,
			lists:map(fun structify_channels/1,
				  NumberedVoltages)}
    end,
    CardData.
	    

%% Returns [Vm] from [V1, V2, ..., Vm, ..., Vn] given arg m
select_channel(Channel, Voltages) ->
    [proplists:lookup(Channel, Voltages)].

%% Forms a data structure for card data containing the supplied
%% timestamp and list of voltages
form_card_ds(Card, {Tstamp, Voltages}) ->
    [{Card,
      {struct, [{timestamp, Tstamp}] ++
	   Voltages}}].

form_error_ds(Card, Reason) ->
    [{Card,
      {struct, [{readError, Reason}]}}].
