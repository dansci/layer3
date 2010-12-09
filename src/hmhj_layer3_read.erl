-module(hmhj_layer3_read).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined};
init([channel]) ->
    {ok, channel}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, State) ->
    Card = list_to_atom(wrq:path_info(card, ReqData)),
    hmhj_layer3_utils:query_card(Card, read),
    RawData = hmhj_layer3_utils:receive_data(read),
    DS = hmhj_layer3_utils:form_ds(read, RawData, Card),
    io:format("data structure is ~p~n", [DS]),
    {mochijson2:encode(DS), ReqData, State}.
