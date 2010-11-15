-module(layer3_read).
-export([init/1, to_html/2]).


-include_lib("webmachine/include/webmachine.hrl").
-include_lib("deps/hmhj_layer2/include/l2request.hrl").

init([]) ->
    {ok, undefined}.

to_html(ReqData, State) ->
    PathDict = wrq:path_info(ReqData),
    PathKeys = dict:fetch_keys(PathDict),
    
    Card = list_to_atom(wrq:path_info(card, ReqData)),
    CardRawData = get_card_data(Card),
    {T, Vi} = CardRawData,
    case lists:member(channel, PathKeys) of
	true ->
	    Channel = list_to_atom(
			wrq:path_info(channel, ReqData)),
	    V = select_channel(Channel, Vi);
	false ->
	    V = Vi
    end,
    CardData = form_card_ds(Card, {T, V}),
    TotalDS = {struct, CardData},
    
    Json = mochijson2:encode(TotalDS),
    
    {"<html>" ++ Json ++ "</html>",
     ReqData, State}.

structify_channels({N, V}) ->
    {list_to_atom("channel" ++ integer_to_list(N)),
     {struct, [{voltage, V}]}}.

%% Returns {Timestamp, [V1, V2, ..., Vn]}
get_card_data(Card) ->
    hmhj_layer2:process_request(
      #l2request{from=self(), to=Card, action=read}),

    receive {R, BinData} ->
	    Data = binary_to_term(BinData)
    end,

    case Data of
	{error, Reason} ->
	    CardData = nil,
	    {error, Reason};
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
    %% pick out the required channel
   [proplists:lookup(Channel, Voltages)].

%% Forms a data structure for card data containing the supplied
%% timestamp and list of voltages
form_card_ds(Card, {Tstamp, Voltages}) ->
    CardDS = [{Card,
	       {struct, [{timestamp, Tstamp}] ++
		    Voltages}}].
