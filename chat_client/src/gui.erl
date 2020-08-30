-module(gui).

-export([start_link/0, join/1, leave/1, message/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_event/2, terminate/2,
         code_change/3]).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

-record(state, {notebook, etsTabId}).

start_link() ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, [], []).

join(Channel) ->
    wx_object:cast(?MODULE, {join, Channel}).

leave(Channel) ->
    wx_object:cast(?MODULE, {leave, Channel}).

message(Channel, Message, From) ->
    wx_object:cast(?MODULE, {message, Channel, Message, From}).

init([]) ->
    wx:new(),
    TabId = ets:new(?MODULE, [set]),

    Frame = wxFrame:new(wx:null(), 1, "Chat client"),
    Panel = wxPanel:new(Frame),
    Notebook = wxNotebook:new(Panel, 2, [{style, ?wxBK_DEFAULT}]),
    MainMessagePage = message_page:start_link(main, Notebook, console),
    wxNotebook:addPage(Notebook, MainMessagePage, "Main", []),
    ets:insert(TabId, {main, main}),

    %% Sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxWindow:setSizer(Panel, MainSizer),

    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),

    {Frame, #state{notebook = Notebook, etsTabId = TabId}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({join, Channel}, #state{notebook = Notebook, etsTabId = TabId} = State) ->
    Name = list_to_atom(Channel),
    case ets:member(TabId, Name) of
        true ->
            ok;
        false ->
            Page = message_page:start_link(Name, Notebook, chat),
            wxNotebook:addPage(Notebook, Page, Channel),
            ets:insert(TabId, {Name, Channel})
    end,
    {noreply, State};
handle_cast({leave, Channel}, #state{notebook = Notebook, etsTabId = TabId} = State) ->
    Name = list_to_atom(Channel),
    case ets:lookup(TabId, Name) of
        [{Name, _}] ->
            SelectedIndex = wxNotebook:getSelection(Notebook),
            wxNotebook:removePage(Notebook, SelectedIndex),
            ets:delete(TabId, Name),
            wx_object:stop(Name);
        [] ->
            ok
    end,
    {noreply, State};
handle_cast({message, Channel, Message, From}, #state{etsTabId = TabId} = State) ->
    Name = list_to_atom(Channel),
    case ets:lookup(TabId, Name) of
        [{Name, _}] ->
            wx_object:cast(Name, {message, Message, From});
        [] ->
            ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_event(#wx{event = #wxClose{}}, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    wx:destroy(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
