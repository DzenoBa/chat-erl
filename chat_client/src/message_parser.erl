-module(message_parser).

-export([start/0]).
-export([parse/1]).

% Generate & compile the lexer and parser
start() ->
    leex:file(lexer),
    compile:file(lexer),
    yecc:file(parser),
    compile:file(parser),
    ok.

parse(Message) ->
    case lexer:string(Message) of
        {error, _, _} ->
            ignore_command(Message);
        {ok, Tokens, _} ->
            case parser:parse(Tokens) of
                {ok, Command} ->
                    Command;
                {error, _} ->
                    ignore_command(Tokens, Message)
            end
    end.

ignore_command(Message) ->
    {message, Message}.

ignore_command(_, Message) ->
    {message, Message}.
