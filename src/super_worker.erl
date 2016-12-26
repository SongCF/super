-module(super_worker).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("super.hrl").

-define(SERVER, ?MODULE).

-record(state, {sup_name}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------


start_link(SuperName, Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [SuperName], []).

%%%-------------------------------------------------------------------
%%% gen_server callbacks
%%%-------------------------------------------------------------------

init([SuperName]) ->
    Super = #super{id=node(), pid=self()},
    process_flag(trap_exit, true),
    cluster:set_val(super, Super),
    {ok, #state{sup_name=SuperName}}.

handle_call(_Msg = {start_child, Args}, _From, State =
                #state{sup_name=SuperName}) ->
    Reply =
        case start_child(SuperName, Args) of
            Pid when is_pid(Pid) -> Pid;
            _Error -> undefined
        end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% 异步调用。
handle_cast(_Msg = {start_child, Args, Data}, State =
                #state{sup_name=SuperName}) ->
    case start_child(SuperName, Args) of
        Pid when is_pid(Pid) -> Pid ! Data;
        _Error -> %% lager:info("start_child error ~p", [Error]),
            ignore
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    Super = #super{id=node(), pid=self()},
    lager:info("terminate"),
    cluster:del_object(super, Super),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------


%% 新启动一个child.
start_child(SuperName, Args) ->
    case catch supervisor:start_child(SuperName, Args) of
        {ok, Pid} -> Pid;
        E -> lager:error("start_child error ~p ~p ~p~n",
                         [SuperName, Args,E]), undefined
    end.
