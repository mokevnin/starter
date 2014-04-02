-module(starter_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {waiting}).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  process_flag(trap_exit, true),

  Waiting = queue:new(),
  {ok, #state{waiting=Waiting}}.

handle_call({evaluate, Lang, Code}, From, State) ->
  #state{waiting=Waiting} = State,

  {ok, DockerImage} = application:get_env(starter, docker_image),
  UUID = uuid:to_string(uuid:uuid1()),
  Runner = "docker run",
  LangCommand = langs:command(Lang, Code),
  DockerCommand = lists:concat([Runner, " ", DockerImage, " ", LangCommand, "; echo '", UUID, "'"]),

  lager:debug([DockerCommand]),
  exec:run(DockerCommand, [stdout, stderr]),
  NewWaiting = queue:in({From, UUID, [], []}, Waiting),
  {noreply, State#state{waiting=NewWaiting}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({Channel, _, Data}, #state{waiting=Waiting} = State)
  when Channel == stdout orelse Channel == stderr ->

  {{value, {From, UUID, Stdout, Stderr}} = Member, WaitingWithoutCurr} = queue:out(Waiting),

  %% lager:debug("~p", [Member]),
  %% lager:debug("~p", [Msg]),

  EofMarker =list_to_binary([UUID, "\n"]),
  NewWaiting = case {Channel, Data} of
                 {stdout, EofMarker} ->
                   MergedStderr = list_to_binary(lists:reverse(Stderr)),
                   MergedStdout = list_to_binary(lists:reverse(Stdout)),
                   Reply = [{stderr, MergedStderr}, {stdout, MergedStdout}],
                   gen_server:reply(From, Reply),
                   WaitingWithoutCurr;
                 {stdout, Data} ->
                   Item = {From, UUID, [Data | Stdout], Stderr},
                   queue:in_r(Item, WaitingWithoutCurr);
                 {stderr, Data} ->
                   Item = {From, UUID, Stdout, [Data | Stderr]},
                   queue:in_r(Item, WaitingWithoutCurr)
               end,
  {noreply, State#state{waiting=NewWaiting}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
