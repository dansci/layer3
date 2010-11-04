-module(layer3_read).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("deps/hmhj_layer2/include/l2request.hrl").

init([]) ->
    {ok, undefined}.

to_html(ReqData, State) ->
    Card = wrq:path_info(card, ReqData),
    Channel = wrq:path_info(channel, ReqData),
    hmhj_layer2:process_request(
      #l2request{from=self(), to=Card, action=read}),
    
    receive {R, BinData} ->
	    Data = binary_to_term(BinData)
    end,
    
    {"<html>Data was read.  It'll be listed here soon... </html>",
     ReqData, State}.
