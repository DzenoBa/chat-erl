-module(message_page).

-behaviour(wx_object).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_event/2]).
-export([terminate/2]).

-include_lib("wx/include/wx.hrl").

-record(state, {name, type, viewMessages, sendInput, nick}).

start_link(Name, Parent, Type) ->
    wx_object:start_link({local, Name}, ?MODULE, [Name, Parent, Type], []).

init([Name, Parent, Type]) ->
    Panel = wxPanel:new(Parent),

    ViewMessages =
        wxTextCtrl:new(Panel,
                       ?wxID_ANY,
                       [{style, ?wxDEFAULT bor ?wxTE_MULTILINE bor ?wxTE_RICH}]),
    wxTextCtrl:setEditable(ViewMessages, false),
    wxTextCtrl:setInsertionPointEnd(ViewMessages),

    SendBox = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Send message"}]),
    SendInput = wxTextCtrl:new(Panel, ?wxID_ANY, [{style, ?wxTE_PROCESS_ENTER}]),

    %% Sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, ViewMessages, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(SendBox, SendInput, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, SendBox, [{flag, ?wxEXPAND}]),

    wxWindow:setSizer(Panel, MainSizer),

    wxTextCtrl:connect(SendInput, command_text_enter),

    print_info_text(ViewMessages, Type),

    Nick =
        case Type of
            console ->
                "Undefined";
            chat ->
                wx_object:call(main, nick)
        end,

    {Panel,
     #state{name = Name,
            type = Type,
            viewMessages = ViewMessages,
            sendInput = SendInput,
            nick = Nick}}.

handle_call(nick, _From, #state{nick = Nick} = State) ->
    Reply = Nick,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({message, Message, From}, #state{viewMessages = ViewMessages} = State) ->
    wxTextCtrl:writeText(ViewMessages, "\n" ++ From ++ ": " ++ Message),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_event(#wx{event = #wxCommand{type = command_text_enter, cmdString = Item}},
             #state{sendInput = SendInput, nick = Nick} = State) ->
    Res = message_parser:parse(Item),
    clear_text(SendInput),
    case Res of
        nick ->
            write_command({info, "Your nick is " ++ Nick}, State),
            {noreply, State};
        {nick, NewNick} ->
            write_command({nick, NewNick}, State),
            {noreply, State#state{nick = NewNick}};
        _ ->
            send(Res, State),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

%% internal functions

write_command({Command, Param}, #state{viewMessages = ViewMessages}) ->
    Message = "> " ++ atom_to_list(Command) ++ " " ++ Param,
    wxTextCtrl:writeText(ViewMessages, "\n" ++ Message);
write_command(Command, #state{viewMessages = ViewMessages}) ->
    Message = "> " ++ atom_to_list(Command),
    wxTextCtrl:writeText(ViewMessages, "\n" ++ Message).

send(connect = Command, #state{type = console} = State) ->
    write_command(Command, State),
    websocket_handler:connect();
send(disconnect = Command, #state{type = console} = State) ->
    write_command(Command, State),
    websocket_handler:disconnect();
send({join, Channel} = Command, #state{type = console} = State) ->
    write_command(Command, State),
    websocket_handler:join(Channel);
send(leave = Command, #state{type = chat, name = Name} = State) ->
    write_command(Command, State),
    websocket_handler:leave(Name);
send({message, Message}, #state{type = chat, name = Name, nick = Nick}) ->
    websocket_handler:message(Name, Message, Nick);
send(_, State) ->
    write_command({unknown, "Invalid command"}, State).

clear_text(TextCtrl) ->
    wxTextCtrl:setValue(TextCtrl, "").

print_info_text(TextCtrl, Type) ->
    case Type of
        console ->
            wxTextCtrl:writeText(TextCtrl,
                                 "Welcome!\nCommands:\n* Type \"/connect\" to connect to the "
                                 "server & \"/disconnect\" to disconnect from it.\n* Type \"/join "
                                 "#<channel-name>\" to join a channel.\n* Type \"/nick\" to see "
                                 "your nick and \"/nick <name>\" to set a nick.");
        chat ->
            wxTextCtrl:writeText(TextCtrl,
                                 "You have joined this channel!\nType \"/leave\" to leave this "
                                 "channel.");
        _ ->
            ok
    end.
