-module(layer3_read).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("deps/hmhj_layer2/include/l2request.hrl").

init([]) ->
    {ok, undefined}.

to_html(ReqData, State) ->
    %% FIXME: may break when card or channel wasn't specified in URI
    PathKeys = dict:fetch_keys(wrq:path_info(ReqData)),
    Card = list_to_atom(wrq:path_info(card, ReqData)),


    %% FIXME: need support for multiple cards
    hmhj_layer2:process_request(
      #l2request{from=self(), to=Card, action=read}),
    
    receive {R, BinData} ->
	    Data = binary_to_term(BinData)
    end,

    {Tstamp, VoltageList} = Data,
    %% Coerce timestamp
    {Ms, S, _us} = Tstamp,
    Timestamp = 1000000*Ms + S,
    %% Coerce voltages
    NumberedVoltages = lists:zip(lists:seq(1, length(VoltageList)),
				 VoltageList),
    ChannelDS1 = lists:map(fun structify_channels/1,
			  NumberedVoltages),

    %% pick out the required channel
    case lists:member(channel, PathKeys) of 
	true ->
	    Channel = wrq:path_info(channel, ReqData),
	    %% FIXME: check that the channel's there!
	    ChannelDS = [proplists:lookup(
			   list_to_binary(Channel), ChannelDS1)];
	false ->
	    ChannelDS = ChannelDS1
    end,

    CardDS = [{atom_to_binary(Card, utf8),
	       {struct, [{<<"timestamp">>, Timestamp}] ++
		    ChannelDS}}],
    
    TotalDS = {struct, CardDS},

    Json = layer3_tools:struct_to_json(TotalDS),

    {"<html>" ++ Json ++ "</html>",
     ReqData, State}.

structify_channels({N, V}) ->
    {list_to_binary("channel" ++ integer_to_list(N)),
     {struct, [{<<"voltage">>, V}]}}.
