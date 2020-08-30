-module(websocket_handler).

-behaviour(gen_server).

-export([start_link/0, connect/0, disconnect/0, join/1, leave/1, message/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {conn_pid, timer_ref}).

start_link() ->
    init([]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    gen_server:cast(?MODULE, connect).

disconnect() ->
    gen_server:call(?MODULE, disconnect).

join(Channel) ->
    Message = #{type => join, payload => #{name => list_to_binary(Channel)}},
    gen_server:cast(?MODULE, {send, Message}),
    ok.

leave(Channel) ->
    Message = #{type => leave, payload => #{name => atom_to_binary(Channel)}},
    gen_server:cast(?MODULE, {send, Message}),
    ok.

message(Channel, Text, From) ->
    Message =
        #{type => message,
          payload =>
              #{channel => atom_to_binary(Channel),
                text => list_to_binary(Text),
                user => list_to_binary(From)}},
    gen_server:cast(?MODULE, {send, Message}),
    ok.

%% Server functions

init([]) ->
    {ok, #state{conn_pid = undefined, timer_ref = undefined}}.

handle_call(disconnect, _From, #state{conn_pid = undefined} = State) ->
    {reply, ok, State};
handle_call(disconnect, _From, #state{conn_pid = ConnPid, timer_ref = TimerRef}) ->
    timer:cancel(TimerRef),
    Reply = gun:shutdown(ConnPid),
    {reply, Reply, #state{conn_pid = undefined, timer_ref = undefined}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(connect, #state{conn_pid = undefined} = State) ->
    {Status, Payload} = try_connect(),
    case Status of
        ok ->
            ok;
        error ->
            logger:error(to_logger_message(Payload)),
            error
    end,
    {noreply, State#state{conn_pid = Payload}};
handle_cast({send, Message}, #state{conn_pid = ConnPid} = State) ->
    JsonString = jsone:encode(Message),
    gun:ws_send(ConnPid, {text, JsonString}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gun_up, ConnPid, _Protocol}, State) ->
    upgrade_to_ws(ConnPid),
    {noreply, State};
handle_info({gun_upgrade, ConnPid, _StreamRef, [<<"websocket">>], _Headers}, #state{}) ->
    logger:notice(to_logger_message("Connected to server")),
    TimeInSeconds = 30000,
    {ok, TimerRef} = timer:send_interval(TimeInSeconds, ping_server),
    gui:message("main", "Connected to server", "Server"),
    {noreply, #state{conn_pid = ConnPid, timer_ref = TimerRef}};
handle_info({gun_response, _ConnPid, _StreamRef, _Frame, Status, _Headers}, State) ->
    logger:error(to_logger_message(Status)),
    {noreply, State};
handle_info({gun_error, _ConnPid, _StreamRef, Reason}, State) ->
    logger:error(to_logger_message(Reason)),
    {noreply, State};
handle_info({gun_ws, _ConnPid, _StreamRef, Frame}, State) ->
    logger:notice(to_logger_message("Received ws message")),
    {text, BinaryMessage} = Frame,
    MessageMap = jsone:decode(BinaryMessage),
    case maps:get(<<"type">>, MessageMap) of
        <<"ok">> ->
            RequestMessageString = maps:get(<<"payload">>, MessageMap),
            RequestMessageMap = jsone:decode(RequestMessageString),
            case maps:get(<<"type">>, RequestMessageMap) of
                <<"join">> ->
                    RequestPayload = maps:get(<<"payload">>, RequestMessageMap),
                    ChannelName = maps:get(<<"name">>, RequestPayload),
                    gui:join(binary:bin_to_list(ChannelName));
                <<"leave">> ->
                    RequestPayload = maps:get(<<"payload">>, RequestMessageMap),
                    ChannelName = maps:get(<<"name">>, RequestPayload),
                    gui:leave(binary:bin_to_list(ChannelName));
                <<"message">> ->
                    RequestPayload = maps:get(<<"payload">>, RequestMessageMap),
                    Channel = maps:get(<<"channel">>, RequestPayload),
                    Text = maps:get(<<"text">>, RequestPayload),
                    User = maps:get(<<"user">>, RequestPayload),
                    gui:message(binary:bin_to_list(Channel),
                                binary:bin_to_list(Text),
                                binary:bin_to_list(User));
                _ ->
                    ok
            end,
            ok;
        <<"message">> ->
            Channel = maps:get(<<"channel">>, MessageMap),
            Text = maps:get(<<"text">>, MessageMap),
            User = maps:get(<<"user">>, MessageMap),
            gui:message(binary:bin_to_list(Channel),
                        binary:bin_to_list(Text),
                        binary:bin_to_list(User));
        _ ->
            ok
    end,
    {noreply, State};
handle_info(ping_server, #state{conn_pid = ConnPid} = State) ->
    gun:ws_send(ConnPid, ping),
    {noreply, State};
handle_info(Msg, State) ->
    logger:error(to_logger_message(Msg)),
    {noreply, State}.

%% internal functions

try_connect() ->
    Host = "localhost",
    Port = 8080,
    gun:open(Host, Port).

upgrade_to_ws(ConnPid) ->
    Path = "/chat",
    gun:ws_upgrade(ConnPid, Path).

to_logger_message(Message) ->
    #{module => ?MODULE,
      pid => self(),
      payload => lists:flatten(io_lib:format("~p", [Message]))}.
