-module(files_cleaner).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    Timer = erlang:send_after(1, self(), remove),
    {ok, Timer}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(remove, OldTimer) ->
    erlang:cancel_timer(OldTimer),
    {ok, StorageDir} = application:get_env(starter, storage_dir),
    Cmd = lists:concat(["find ", StorageDir, " -type f -mmin +1 -delete"]),
    exec:run(Cmd, [stdout, stderr, sync]),
    %% lager:debug("~p", [Result]),

    Timer = erlang:send_after(10000, self(), remove),
    {noreply, Timer};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

