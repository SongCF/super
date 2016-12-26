-module(super).
-compile([export_all]).

-include("super.hrl").


start() ->
    application:ensure_all_started(super).

%% 查找某一个Node上任意一个super.
get(Node) ->
    List = mnesia:dirty_read(super, Node),
    PidList = [Pid || #super{pid=Pid} <- List],
    %% lager:info("super get ~p", [Node]),
    lists:nth(rand:uniform(length(PidList)), PidList).

%% 同步启动，等待结果.
start_sync(Node, Spec) ->
    Pid = super:get(Node),
    gen_server:call(Pid, {start_child, Spec}).

%% 异步启动，传入回调函数.
start_async(Node, Spec, Msg) ->
    Pid = super:get(Node),
    gen_server:cast(Pid, {start_child, Spec, Msg}).

