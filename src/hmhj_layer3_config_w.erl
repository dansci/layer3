-module(hmhj_layer3_config_w).
-export([init/1,
	 allowed_methods/2,
	 content_types_accepted/2,
	 malformed_request/2,
	 process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) ->
    {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", process_post}], ReqData, State}.

%% checks for JSON syntax errors
malformed_request(ReqData, State) ->
    try mochijson2:decode(wrq:req_body(ReqData)) of
	_Val ->
	    {false, ReqData, State}
    catch
	_:_ ->
	    {true, ReqData, State}
    end.


process_post(ReqData, State) ->
    Body = wrq:req_body(ReqData),
    DataStruct = mochijson2:decode(Body),
    ResponseDS = process_request(DataStruct),
    %% FIXME: hacky check to see if there were any errors.
    NewReqData = case ResponseDS of
		     ok ->
			 ReqData;
		     R ->
			 wrq:append_to_response_body(
			   mochijson2:encode(R), ReqData)
		 end,
    {true, NewReqData, State}.

%% Allow only configuration of one card at a time
process_request({struct, [CardDS|[]]}) ->
    {CardNo, {struct, Proplist}} = CardDS,
    Payload = form_req_payload(Proplist),
    send_l2req(binary_to_atom(CardNo, utf8), configure, Payload),
    Response = hmhj_layer3_utils:receive_data(configure),
    hmhj_layer3_utils:form_config_resp(Response).
%% FIXME return an error that would indicate that only one card's
%% configuration can be done per request.

form_req_payload(Proplist) ->
    form_req_payload(Proplist, []).

form_req_payload([], Acc) ->
    lists:reverse(Acc);
form_req_payload([{ChannelName, {struct, List}}|T], Acc) ->
    "channel" ++ NumString = binary_to_list(ChannelName),
    Channel = list_to_integer(NumString),
    %% if the gain wasn't being set, this match will fail.
    %% as of 101207, I think the gain is the only thing that can be set
    [{<<"gain">>, Val}] = List,
    form_req_payload(T, [{gain, {Channel, Val}}|Acc]);
form_req_payload([{Key, Val}|T], Acc) when is_binary(Key), is_number(Val) ->
    %% this is for things like channel gain sets
    form_req_payload(T, [{binary_to_atom(Key, utf8), Val}|Acc]);
form_req_payload([{Key, Val}|T], Acc) when is_binary(Key), is_binary(Val) ->
    form_req_payload(T, [{binary_to_atom(Key, utf8), 
		      binary_to_atom(Val, utf8)}|Acc]).

send_l2req(Card, Action, PLoad) ->
    {ok, Req0} = hmhj_layer2:new_request(),
    {ok, Req1} = hmhj_layer2:set_dest(Req0, Card),
    {ok, Req2} = hmhj_layer2:set_action(Req1, Action),
    {ok, Req3} = hmhj_layer2:set_payload(Req2, PLoad),
    Req = Req3,
    hmhj_layer2:process_request(Req),

    %% FIXME do something with this...
    receive {_, _} ->
	    ok
    end.

