-module(chat_service_test).

-include_lib("eunit/include/eunit.hrl").

%% Should interpret join message
interpret_message_join_test() ->
    %% Given
    meck:new(pg, [unstick]),
    meck:expect(pg,
                join,
                fun (_Name, _Pid) ->
                        ok
                end),
    Test_channel_name = <<"channel-name">>,

    %% When
    Actual = chat_service:interpret_message({<<"join">>, [{<<"name">>, Test_channel_name}]}),

    %% Then
    ?assertEqual(ok, Actual),
    ?assert(meck:called(pg, join, [Test_channel_name, self()])),
    ?assert(meck:validate(pg)),

    %% After
    meck:unload(pg).

%% Should interpret leave message
interpret_message_leave_test() ->
    %% Given
    meck:new(pg, [unstick]),
    meck:expect(pg,
                leave,
                fun (_Name, _Pid) ->
                        ok
                end),
    Test_channel_name = <<"channel-name">>,

    %% When
    Actual = chat_service:interpret_message({<<"leave">>, [{<<"name">>, Test_channel_name}]}),

    %% Then
    ?assertEqual(ok, Actual),
    ?assert(meck:called(pg, leave, [Test_channel_name, self()])),
    ?assert(meck:validate(pg)),

    %% After
    meck:unload(pg).

%% Should interpret chat message
interpret_message_chat_test() ->
    %% Given
    meck:new(pg, [unstick]),
    Test_channel_name = <<"channel-name">>,
    Test_message_text = <<"Hello World!">>,
    Test_message_from = <<"Test-user-1">>,
    Test_pid =
        spawn(fun () ->
                      timer:sleep(3000)
              end),
    meck:expect(pg,
                get_members,
                fun (_Name) ->
                        [Test_pid]
                end),

    %% When
    Actual =
        chat_service:interpret_message({<<"message">>,
                                        [{<<"channel">>, Test_channel_name},
                                         {<<"text">>, Test_message_text},
                                         {<<"user">>, Test_message_from}]}),

    %% Then
    ?assertEqual(ok, Actual),
    ?assert(meck:called(pg, get_members, [Test_channel_name])),
    ?assert(meck:validate(pg)),
    Received_messages = erlang:process_info(Test_pid, messages),
    ?assertEqual(messages, element(1, Received_messages)),
    ?assertEqual(1, length(element(2, Received_messages))),
    [{message, Received_message}] = element(2, Received_messages),
    ?assertEqual(message, maps:get(type, Received_message)),
    ?assertEqual(Test_channel_name, maps:get(channel, Received_message)),
    ?assertEqual(Test_message_text, maps:get(text, Received_message)),
    ?assertEqual(Test_message_from, maps:get(user, Received_message)),

    %% After
    exit(Test_pid, kill),
    meck:unload(pg).

%% Should return error when interpreting unknown message
interpret_message_unknown_test() ->
    %% When
    Actual = chat_service:interpret_message({<<"unknown">>, []}),

    %% Then
    ?assertEqual(2, tuple_size(Actual)),
    ?assertEqual(error, element(1, Actual)),
    Map = element(2, Actual),
    ?assertEqual(error, maps:get(type, Map)),
    ?assertEqual(<<"Unknown command">>, maps:get(payload, Map)).
