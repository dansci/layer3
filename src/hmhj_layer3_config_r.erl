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
    hmhj_layer3_utils:query_card(Card, status),
    RawData = hmhj_layer3_utils:receive_data(status),
    DS = {struct, [{Card, hmhj_layer3_utils:form_ds(status, RawData)}]},
    {mochijson2:encode(DS), ReqData, State}.
