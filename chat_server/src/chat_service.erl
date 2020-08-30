-module(chat_service).

-export([interpret_message/1]).
-export([to_message/2]).

interpret_message({<<"join">>, [{<<"name">>, ChannelName}]}) ->
    pg:join(ChannelName, self()),
    % broadcast({channels}),
    ok;
interpret_message({<<"leave">>, [{<<"name">>, ChannelName}]}) ->
    pg:leave(ChannelName, self()),
    % broadcast({channels}),
    ok;
interpret_message({<<"message">>,
                   [{<<"channel">>, ChannelName}, {<<"text">>, Text}, {<<"user">>, User}]}) ->
    broadcast({message, ChannelName, Text, User}),
    ok;
interpret_message(_) ->
    {error, to_message(error, <<"Unknown command">>)}.

%broadcast({channels}) ->
%    Groups = pg:which_groups(),
%    [[Pid ! {message, to_message(info, Groups)}
%        || Pid <- pg:get_members(Group)]
%    || Group <- Groups];
broadcast({message, ChannelName, Text, User}) ->
    [Pid ! {message, to_message(message, ChannelName, Text, User)}
     || Pid <- pg:get_members(ChannelName), Pid =/= self()];
broadcast(_) ->
    ignore.

to_message(message, ChannelName, Payload, User) ->
    #{type => message,
      channel => ChannelName,
      text => Payload,
      time => get_time(),
      user => User}.

to_message(Type, Payload) ->
    #{type => Type, payload => Payload, time => get_time()}.

get_time() ->
    {{Year, Month, Day}, {Hour, Minute, Seconds}} = calendar:universal_time(),
    TimeChars =
        io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                      [Year, Month, Day, Hour, Minute, Seconds]),
    TimeString = lists:flatten(TimeChars),
    list_to_binary(TimeString).

%% Note: Users tells the server their name, so no records about the user
%% are stored. Users may have same name or pretend to be someone else.
%% To solve this, authentication can be used but it is out of scope for this
%% project.
%% ---
%% Note: An user may join a channel several times and must leave a channel the
%% same number of times (see doc of pg module).
%% ---
%% Note: An user does not have to be subscribed to a channel to send messages to
%% it.
