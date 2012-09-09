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

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	 category :: string(),
	 log_file_name :: string(),
	 address :: string(),
         level :: integer(),
	 logged_count :: integer()}).

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
    case parse_config(LogConfig) of
	{Category, LogFileName, Address, Level} ->
	    {ok, #state{category = Category,
		       log_file_name = LogFileName,
		       level = Level,
		       address = Address}};
	{error, Reason} ->
	    {error, Reason}
    end.

parse_config({Category, LogFileName, Address, Level}) when is_list(Category)
							   and is_list(LogFileName)
							   and is_list(Address)
							   and is_atom(Level) ->
    {Category, LogFileName, Address, Level};
parse_config(InvalidConfig) ->
    {error, "Invalid config " ++ InvalidConfig }.


%% @private
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
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
-endif.
