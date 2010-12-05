-module(hmhj_layer3_write).

-export([init/1,
	 allowed_methods/2,
	 content_types_provided/2,
	 process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) ->
    {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", process_post}], ReqData, State}.

process_post(ReqData, State) ->
    Body = wrq:req_body(ReqData),
    DataStruct = mochijson2:decode(Body),
    send_requests(DataStruct),
    {true, ReqData, State}.


%% I may want to keep track of the return values, if any, given by
%% layer2
send_requests({struct, []}) ->
    ok;
send_requests({struct, [CardDS|Rest]}) ->
    %% no error checking to see that the keys are indeed card names
    {CardNo, {struct, Proplist}} = CardDS,
    Payload = form_payload(Proplist),
    send_l2req(binary_to_atom(CardNo, utf8), write, Payload),
    send_requests({struct, Rest}).

form_payload(Proplist) ->
    form_payload(Proplist, []).

form_payload([], Acc) ->
    Acc;
form_payload([{ChannelName, Val}|T], Acc) when is_binary(ChannelName),
					       is_integer(Val) ->
    "channel" ++ NumString = binary_to_list(ChannelName),
    Channel = list_to_integer(NumString),
    form_payload(T, [{Channel, Val}|Acc]).

send_l2req(Card, Action, PLoad) ->
    {ok, Req0} = hmhj_layer2:new_request(),
    {ok, Req1} = hmhj_layer2:set_dest(Req0, Card),
    {ok, Req2} = hmhj_layer2:set_action(Req1, Action),
    {ok, Req3} = hmhj_layer2:set_payload(Req2, PLoad),
    Req = Req3,
    hmhj_layer2:process_request(Req),
    
    receive {_, _} ->
	    ok
    end.
