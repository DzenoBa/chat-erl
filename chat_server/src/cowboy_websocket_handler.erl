-module(cowboy_websocket_handler).

-export([init/2, terminate/3]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Request, Options) ->
    {cowboy_websocket, Request, Options}.

websocket_init(State) ->
    logger:notice(to_logger_message("New connecton")),
    {[], State}.

websocket_handle({text, Message}, State) ->
    logger:notice(to_logger_message("Message from client received")),
    {Status, Payload} = parse_message({decode, Message}),
    Response =
        case Status of
            ok ->
                case chat_service:interpret_message(Payload) of
                    ok ->
                        chat_service:to_message(ok, Message);
                    {error, PayloadFromService} ->
                        logger:error(to_logger_message(Payload)),
                        PayloadFromService
                end;
            error ->
                logger:error(to_logger_message(Payload)),
                chat_service:to_message(error, <<"Invalid data">>)
        end,
    {[{text, jiffy:encode(Response)}], State};
websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({message, Message}, State) ->
    logger:notice(to_logger_message("Sending message to client")),
    {[{text, jiffy:encode(Message)}], State};
websocket_info(_Info, State) ->
    {[], State}.

terminate(_Reason, _Request, _State) ->
    logger:notice(to_logger_message("Terminating")),
    ok.

%% internal functions

parse_message({decode, JsonString}) ->
    {Status, Payload} = decode_json(JsonString),
    case Status of
        ok ->
            parse_message({validate, Payload});
        error ->
            {error, Payload}
    end;
parse_message({validate, DecodedJson}) ->
    {Status, Payload} = validate_data(DecodedJson),
    case Status of
        ok ->
            parse_message({parse, Payload});
        error ->
            {error, Payload}
    end;
parse_message({parse, Value}) ->
    {Attributes} = Value,
    Type = proplists:get_value(<<"type">>, Attributes),
    {Payload} = proplists:get_value(<<"payload">>, Attributes),

    %% Reorder list
    PayloadOrder = [<<"name">>, <<"channel">>, <<"text">>, <<"user">>],
    PayloadWithUndefined = [{X, proplists:get_value(X, Payload)} || X <- PayloadOrder],
    NewPayload = [{X, Y} || {X, Y} <- PayloadWithUndefined, Y =/= undefined],

    {ok, {Type, NewPayload}};
parse_message(_) ->
    {error, "Could not parse message"}.

validate_data(Data) ->
    Schema =
        jiffy:decode(<<"{\"type\": \"object\",\"properties\": {\"type\": {\"type\": "
                       "\"string\", \"required\": true},\"payload\": {\"type\": \"object\",\""
                       "required\": true,\"properties\": { \"name\": {\"type\": \"string\"},\""
                       "channel\": {\"type\": \"string\"},\"text\": {\"type\": \"string\"},\""
                       "user\": {\"type\": \"string\"}}}}}">>),
    jesse:validate_with_schema(Schema, Data).

decode_json(Message) ->
    try jiffy:decode(Message) of
        Value ->
            {ok, Value}
    catch
        error:Error ->
            {error, Error}
    end.

to_logger_message(Message) ->
    #{module => ?MODULE,
      pid => self(),
      payload => lists:flatten(io_lib:format("~p", [Message]))}.
