%%%-------------------------------------------------------------------
%%% @author 姚 海涛 <>
%%% @copyright (C) 2012, 姚 海涛
%%% @doc
%%%
%%% @end
%%% Created :  8 Sep 2012 by 姚 海涛 <>
%%%-------------------------------------------------------------------
-module(elogserver_backend).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).
-export([do_start_writer/3, loop/1]).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% debug on the console
-define(DEBUG(String, Args), io:format(String ++ "~n", Args)).
%% -define(DEUBG(_String, _Args), ok).
-record(state, {
	  name :: string(),
	 category :: string(),
	 log_file_name :: string(),
	 address :: string(),
         level :: integer(),
	 logged_count :: integer(),
	 writer_pid :: pid(),
	 writer_monitor_ref :: reference()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @private
init(LogConfig) ->
    ?DEBUG("config: ~p~n", [LogConfig]),
    case parse_config(LogConfig) of
	{Name, Category, LogFileName, Address, Level} ->
	    Pid = start_writer(Category, LogFileName, Address),
	    {ok, #state{
	       name = Name,
	       category = Category,
		       log_file_name = LogFileName,
		       level = lager_util:level_to_num(Level),
		       address = Address,
	      writer_pid = Pid}};
	{error, Reason} ->
	    {error, Reason}
    end.

start_writer(Category, LogFileName, Address) ->
    ?DEBUG("start to spawn", []),
    process_flag(trap_exit, true),
    ?DEBUG("args: ~p, ~p, ~p ~n", [Category, LogFileName, Address]),
    Pid = erlang:spawn_link(?MODULE, do_start_writer, [Category, LogFileName, Address]),
    ?DEBUG("sender pid: ~p", [Pid]),
    Pid.
do_start_writer(Category, LogFileName, {Ip, Port}) ->
    ?DEBUG("staring the write", []),
    case gen_tcp:connect(Ip, Port, [binary, {packet, 0}]) of
	{ok, Socket} ->
	    Handshake = list_to_binary(io_lib:format("~s##~s", [Category, LogFileName])),
	    HeaderSize = erlang:byte_size(Handshake),
	    gen_tcp:send(Socket, <<1:32, HeaderSize:32>>),
	    gen_tcp:send(Socket, Handshake),
	    loop(Socket);
	{error, Reason} ->
	    ?DEBUG("failed to connect to ~p, reson: ~p", [{Ip, Port}, Reason]),
	    {error, Reason}
    end.

loop(Socket) ->
    receive
	{log, Message} ->
	    BMessage = list_to_binary(Message),
	    MLength = erlang:byte_size(BMessage),
	    ?DEBUG("Bmessage: ~p, length: ~p~n", [BMessage, MLength]),
	    gen_tcp:send(Socket, <<2:32, MLength:32>>),
	    case gen_tcp:send(Socket, BMessage) of
		ok ->
		    ?DEBUG("message sent, ~s,~p~n", [Message, Socket]),
		    ?MODULE:loop(Socket);
		{error, Reason} ->
		    gen_tcp:close(Socket),
		    ?DEBUG("failed to send data, socket: ~p, reson: ~p", [Socket, Reason])
	    end;
	{'EXIT', _} ->
	    gen_tcp:close(Socket),
	    ?DEBUG("sender exit, socket: ", [Socket])
    end.
	    
	    
    
parse_address(Address) ->
     case string:tokens(Address, ":") of
	 [Ip, Port] ->
             {P, _} = string:to_integer(Port),
	     case string:tokens(Ip, ".") of
		 [A, B, C, D] ->
		     {{A,B,C,D}, P};
		 [Host] ->
		     {Host, P}
	     end;
	 _ ->
	     error
     end.
parse_config([Name, Category, LogFileName, Address, Level]) when is_list(Name)
							   and is_list(Category)
							   and is_list(LogFileName)
							   and is_list(Address)
							   ->
    case parse_address(Address) of
	{Ip, Port} ->
	    {Name, Category, LogFileName, {Ip, Port}, Level};
	error ->
	    {error, "invalid address"}
    end;
parse_config(InvalidConfig) ->
    {error, "Invalid config " ++ InvalidConfig }.

log(_Level, {Date, Time}, Message, #state{writer_pid = WriterPid} = State) ->
    FMessage = io_lib:format("~s ~s ~s~n", [Date, Time, Message]),
    WriterPid ! {log, FMessage},
    State.

handle_event({log, Dest, Level, {Date, Time}, Message}, #state{name = Name, level = L} = State) when Level > L ->
    case lists:member({?MODULE, Name}, Dest) of
	true ->
	    {ok, log(Level, {Date, Time}, Message, State)};
	false ->
	    {ok, State}
    end;
handle_event({log, Level, {Date, Time}, Message}, #state{level = L} = State) when Level =< L ->
    {ok , log(Level, {Date, Time}, Message, State)};
%% @private
handle_event(_Event, State) ->
    {ok, State}.

handle_call({set_loglevel, Level}, State = #state{category = _Category}) ->
    {ok, ok, State#state{level = lager_util:level_to_num(Level)}};
handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};
%% @private
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%handle_info({'DOWN', MonitorRef, _Type, _Object, _Info}, #state{category = Category, log_file_name = LogFileName, address = Address} = State) ->
%%    erlang:demonitor(MonitorRef),
%%    
%%    {ok, State#state{writer_pid = Pid}};

handle_info({'EXIT', WPid, Reason}, #state{category = Category, log_file_name = LogFileName, address = Address} = State) ->
    ?DEBUG("writer pid exit, pid: ~p, reason: ~p ~n", [WPid, Reason]),
    Pid  = start_writer(Category, LogFileName, Address),
    {ok, State#state{writer_pid = Pid}};
    
%% @private
handle_info(Info, State) ->
    ?DEBUG("unknown info message: ~p~n", [Info]),
    {ok, State}.

%% @private
terminate(_Reason, #state{writer_pid = Pid} = _State) ->
    erlang:exit(Pid),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").



null_test() ->
    State = #state{},
    Category = State#state.category,
    ?assertEqual(undefined, Category).


parse_address_test() ->
    Address = "127.0.0.1:1234",
    Result = parse_address(Address),
   ?assertEqual({{"127", "0", "0", "1"}, 1234}, Result).
parse_address1_test() ->
    Address = "dctest:1234",
    Result = parse_address(Address),
   ?assertEqual({"dctest", 1234}, Result).

lager_test() ->
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend, debug},
					  {elogserver_backend,
					   ["log_name", "test_log", "erlang_test", "dctest:9512", info]}]),
    application:start(lager),
    lager:log(info, self(), "Test INFO message"),
    lager:log(info, self(), "test log").

length_test() ->
    Data = <<"test_log##lager_test.log">>,
    Length = erlang:byte_size(Data),
    ?assertEqual(24, Length).

-endif.
