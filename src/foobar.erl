-module(foobar).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Url| [Tout | ContentType]] = Args) ->
    io:format("Args: ~p~n", [Args]),
    %%[Host | Args2] = Args,
    %%[Tout | _] = Args2,
    Timeout = binary_to_integer(list_to_binary(Tout)),
    {ConnPid, What} = prepare(Url),
    io:format("What is ~s~n", [What]),
    Response = request_2(ConnPid, Timeout, ContentType, What),
    io:format("~s~n", [Response]),
    response(Response, true),
    stats(Response), 
    io:format("Custom Body if needed ~n~s~n", [custom_body()]),
    %% request_1(ConnPid),
    erlang:halt(0).

custom_body() ->
   Body = 
      #{
         foo => <<"bar">>
      },
   jsx:encode(Body).

response([], _JustLength) -> io:format("Finish~n", []);

response([Part | State], JustLength) ->
   case JustLength of
     false -> io:format("~s~n~9..0B~n", [Part, byte_size(Part)]);
     true -> io:format("~9..0B~n", [byte_size(Part)])
   end,
   response(State, JustLength).

stats(Response) ->
   Size = lists:foldl(fun(X, Sum) -> Sum + byte_size(X) end, 0, Response),
   Elements = length(Response),
   io:format("Size: ~9..0B Elements: ~9..0B~n", [Size, Elements]).

prepare(Url) ->
   [Host | After] = binary:split(binary:list_to_bin(Url), <<"/">>),
   io:format("After is ~s~n", [After]),
   After2 =
   case length(After) of
      0 -> 
	   "/" ;
      1 -> 
	   [V | _] = After,
	   io:format("prepare match 1 ~s~n", [V]),
	   %%binary:list_to_bin(lists:join(<<"">>, [<<"/">>, V]));
	   lists:join(<<"">>, [<<"/">>, V]);
      _ -> 
	   io:format("prepare match more than one~n", []),
	   [V1 | _] = lists:join(<<"/">>, [<<"/">>, After]),
	   V1
   end,
   io:format("After2 is ~s~n", [After2]),
   
   application:ensure_all_started(gun),
   application:ensure_all_started(jsx),
   Host2 = binary:bin_to_list(Host),
   
   {ok, ConnPid} = gun:open(Host2, 443),
   io:format("gun:open executed on host ~s~n", [Host2]),
   case gun:await_up(ConnPid) of
      {ok, Protocol} -> io:format("Protocol ~s~n", [Protocol]);
      _AnyValue -> io:format("Error attempting to get protocol~n", [])
   end,
   {ConnPid, After2}.

request_1(ConnPid) ->
   StreamRef = gun:get(ConnPid, "/"),
   case gun:await(ConnPid, StreamRef) of
     {response, fin, Status, Headers} ->
        no_data;
     {response, nofin, Status, Headers} ->
        {ok, Body} = gun:await_body(ConnPid, StreamRef),
        io:format("~s~n", [Body])
   end,
   io:format("request_1 end~n",[]).

build_headers(<<"0">>) ->
  io:format("Empty headers~n", []),
  [];

build_headers(ContentType) ->
  io:format("headers ~s~n", [ContentType]),
  [{<<"content-type">>, ContentType}].

request_2(ConnPid, Timeout, ContentType, What) ->
   HDRS = build_headers(ContentType),
   %% StreamRef = gun:get(ConnPid, What, HDRS),
   StreamRef = gun:get(ConnPid, What),
   
   MRef = monitor(process, ConnPid),
   receive
        {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
            no_data;
        {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
            receive_data(ConnPid, MRef, StreamRef, []);
        {'DOWN', MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason)
    after Timeout ->
        exit(timeout)
    end.

receive_data(ConnPid, MRef, StreamRef, Response) ->
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            %% io:format("~s~n", [Data]),
	    NewResponse = [Data | Response],
            io:format("~s~n", [<<"partial">>]),
            receive_data(ConnPid, MRef, StreamRef, NewResponse);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            %% io:format("~s~n", [Data]),
            io:format("request_2 end~n",[]),
	    NewResponse = [Data | Response],
	    lists:reverse(NewResponse);
        {'DOWN', MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason)
    after 1000 ->
        exit(timeout)
    end.
  
%%====================================================================
%% Internal functions
%%====================================================================
