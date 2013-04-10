%% =====================================================================
%% @copyright 2011 Mark R. Greenstreet
%% @author Mark R. Greenstreet <mrg@cs.ubc.ca>
%% @end
%% =====================================================================
%%
%% @doc workers - create and manage a pool of worker processes.
%% Each worker process has a process state.  The process state is an
%% association list of tuples of the form <code>{Key, Value}</code>,
%% where <code>Key</code> and <code>Value</code> are arbitrary Erlang terms
%% When a process is created, this list is empty.
%%
%% Worker process wait to receive tasks.  The task is invoked on the
%% current state and returns a new state.
%%
%% @type worker_pool().  Abstract type for a worker pool.
%% @type worker_state().  Abstract type for a worker process state.
%%
%% @end

-module workers.
-export [create/1, create/0, default_n/0, alive/1, reap/1, nworkers/1].
-export [get/2, get/3, keys/1, put/2, put/3].
-export [broadcast/2, broadcast/3, retrieve/2, retrieve/3, update/3, update/4].
-export [rlist/3, rlist/4].

% set timeout to infinity for normal use.
% Set to an upperbound on receive delay for debugging.
debug_msg_timeout() -> 5000.

% ------------------------------------------------------------------------
% Basic processes for worker pools:
%   create(N) ->   a worker pool of N processes.
%   nworkers(W) -> the number of workers in pool W.
%   reap(W) ->     terminate the workers in pool W.
% ------------------------------------------------------------------------

% workerProc: a worker process
%   The state of a function is maintained in the parameter S.
%   The workerProc function waits to receive a function, Update.
%   Then, the process continues with S replaced by Update(S).
workerProc(S) ->
  receive
    Update when is_function(Update, 1) -> workerProc(Update(S));
    exit -> ok
  end.

%% @spec create(N::integer()) -> worker_pool()
%% @doc  Spawn <code>N</code> worker processes.
create(0) -> [];
create(N) -> [ spawn(fun() -> workerProc([]) end) | create(N-1)].

%% @spec create() -> worker_pool()
%% @equiv create(default_n())
create() -> create(default_n()).

%% @spec default_n() -> integer()
%% @doc  A default for the number of workers for a worker pool.
default_n() -> erlang:system_info(schedulers).

%% @spec nworkers(W::worker_pool()) -> integer()
%% @doc  Return the number of workers in <code>W</code>.
nworkers(W) -> length(W).

%% @spec alive(W::worker_pool()) -> true | false
%% @doc  Return true if all of the processes in worker_pool() are alive.
alive(W) -> lists:all(fun(P) -> is_process_alive(P) end, W).

%% @spec reap(W::worker_pool()) -> ok
%% @doc  Terminate the worker processes of <code>W</code>.
reap(W) -> broadcast(W, fun(_P) -> exit(normal) end), ok.



% ------------------------------------------------------------------------
% Functions that workers use to access their process state
%   get(S, Key) -> the value associated with Key or 'undefined'.
%   put(S1, Key, Value) -> S2 updated to associate Value with Key
%   put(S1, TupleList) -> S2 updated to associate element(2, X)
%                                with element(1,X) for each X of TupleList. 
%   keys(S) -> a list of all of the Keys in the association list.
% ------------------------------------------------------------------------

%% @spec get(S, Key, DefaultFn) -> term()
%%   S = worker_state(),
%%   Key = term(),
%%   DefaultFn = (   undefined
%%                 | fun(() -> term())
%%                 | fun((S::worker_state()) -> term())
%%                 | fun((S::worker_state(), Key::worker_state()) -> term())
%%   )
%% @end

%% @doc Get that value associated with <code>Key</code> in process state <code>S</code>.
%%   Parameters:
%%   <ul>
%%      <li> <code>S</code>: The state of the worker process.
%%      </li>
%%      <li> <code>Key</code>: The key associated with the desired value.
%%      </li>
%%      <li> <code>DefaultFn</code>: If <code>S</code> associates no value with
%%        <code>Key</code> then <code>DefaultFn</code> is used to determine the
%%        result of <code>get(...)</code>.
%%      </li>
%%   </ul>
%%   Result:
%%     if there is a value associated with <code>Key</code> in <code>S</code>,
%%     then that value is returned.  Otherwise, the return value is determined
%%     by <code>DefaultFn</code>:
%%     <ul>
%%       <li> If <code>DefaultFn</code> is the atom <code>undefined</code>,
%%         then return value is the atom <code>undefined</code>.
%%       </li>
%%       <li> If <code>DefaultFn</code> is a function with arity 0, then
%%          the return value is <code>DefaultFn()</code>.
%%       </li>
%%       <li> If <code>DefaultFn</code> is a function with arity 1, then
%%          the return value is <code>DefaultFn(S)</code>.
%%       </li>
%%       <li> If <code>DefaultFn</code> is a function with arity 3, then
%%          the return value is <code>DefaultFn(S, Key)</code>.</li>
%%       <li> If <code>DefaultFn</code> does not match any of these patterns,
%%          then an error is thrown.
%%       </li>
%%     </ul>
get(S, Key, DefaultFn) ->
  case lists:keyfind(Key, 1, S) of
    {Key, Value} -> Value;
    false ->
      if
        DefaultFn == undefined -> undefined;
	is_function(DefaultFn, 0) -> DefaultFn();
	is_function(DefaultFn, 1) -> DefaultFn(S);
	is_function(DefaultFn, 2) -> DefaultFn(S, Key)
      end
  end.

%% @spec get(S::worker_state(), Key::term()) -> term()
%% @equiv get(S, Key, undefined)
get(S, Key) -> workers:get(S, Key, undefined).


%% @spec put(S::worker_state(), Key::term(), Value::term()) -> worker_state()
%% @doc Update state <code>S</code> to associate <code>Value</code> with
%%   <code>Key</code>.
put(S, Key, Value) ->
  lists:keystore(Key, 1, S, {Key, Value}).

%% @spec put(S, TupleList) -> worker_state()
%%   S = worker_state(),
%%   TupleList = [ { Key::term(), Value::term() } ]
%% @doc Update state <code>S</code> so that each <code>Value</code> is
%%   associated with its corresponding <code>Key</code>.
put(S, []) -> S;
put(S, [{Key, Value} | Tail]) ->
  workers:put(workers:put(S, Key, Value), Tail).

%% @spec keys(S::worker_state()) -> [term()]
%% @doc return a list of all of the keys for association in <code>S</code>
keys(S) -> [ Key || {Key, _Value} <- S].



% ------------------------------------------------------------------------
% Functions that the root process uses to interact with worker processes
%   broadcast(W, Task, [Args]) ->  send the Task to each worker.
%   retrieve(W, Fun, [Args])   ->  evaluate Fun in each worker,
%                                  and collect the results
%   update(W, Key, Fun, [Args]) -> update the value associated with Key in
%                                  each process with the result of applying
%                                  Fun in that process.
% ------------------------------------------------------------------------

%% @spec broadcast(W::worker_pool(), Task) -> ok
%%    Task = (   fun((S::worker_state()) -> worker_state())
%%             | fun((S::worker_state(), N::integer) -> worker_state())
%%    )
%% @doc Each worker process performs Task.
%% If
%% <ul>
%%   <li><code>Task</code> is a function with arity 1,
%%     then it is called with the current process state.
%%     The process state is updated to the return value of <code>Task</code>.
%%   </li>
%%   <li><code>Task</code> is a function with arity 2,
%%     then the first parameter is the current process state,
%%     and the second parameter is the index of the process.
%%     The process state is updated to the return value of <code>Task</code>.
%%     Process indices range from 1 to
%%     <code>{@link nworkers/1. nworkers}(W)</code>.
%%   </li>
%% </ul>
broadcast(W, Task) when is_function(Task, 1) ->
  [ WW ! Task || WW <- W ];
broadcast(W, Task) when is_function(Task, 2) ->
  [    WW ! fun(S) -> Task(S, NN) end
    || {WW, NN} <- lists:zip(W, lists:seq(1, length(W)))
  ].

%% @spec broadcast(W::worker_pool(), Task, Args::List) -> ok
%%    Task = (   fun((S::worker_state(), Arg) -> worker_state())
%%            | fun((S::worker_state(), N::integer, Arg) -> worker_state())
%%    ),
%%    length(Args) = workers:nworkers(W)
%% @doc Each worker process performs <code>Task</code>, where <code>Task</code>
%% is called with a per-worker argument.
%% If
%% <ul>
%%   <li><code>Task</code> is a function with arity 2,
%%     then it is called with the current process state and
%%     <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>,
%%     where <code>N</code> is the index of the process in <code>W</code>.
%%     Process indices range from 1 to
%%     <code>{@link nworkers/1. nworkers}(W)</code>.
%%   </li>
%%   <li><code>Task</code> is a function with arity 3, then the first
%%     parameter is the current process state, the second parameter
%%     is the index of the process, and the third parameter is
%%     <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>.
%%   </li>
%% </ul>
broadcast(W, Fun, Args) when is_function(Fun, 2) ->
  broadcast(W, fun(S, _N, A) -> Fun(S, A) end, Args);
broadcast(W, Fun, Args) when is_function(Fun, 3) ->
  [    WW ! fun(S) -> Fun(S, NN, AA) end
    || {WW, NN, AA} <- lists:zip3(W, lists:seq(1, length(W)), Args)
  ],
  ok.

%% @spec retrieve(W::worker_pool(), Fun, Args::List) -> Values
%%    Fun = (   fun((S::worker_state(), Arg) -> term())
%%            | fun((S::worker_state(), N::integer(), Arg) -> term)
%%    ),
%%    Values = [ term() ]
%% @doc Each worker evaluates <code>Fun</code>.
%% The return value of <code>retrieve(W, Fun, Args)</code>
%% is a list whose <code>N</code>th element is the result of applying
%% <code>Fun</code> in the <code>N</code>th process.
%%
%% If <code>Fun</code> has arity 2, then it is called with the current process
%% state and
%% <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>,
%% where <code>N</code> is the index of the process in <code>W</code>.
%% If <code>Fun</code> has arity 3, then the first parameter is the current process state,
%% the second parameter is the index of the process, and the third parameter is
%% <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>.
retrieve(W, Fun, Args) ->
  MyPid = self(),
  F0 = if
    is_function(Fun, 2) -> fun(S, _N, Arg) -> Fun(S, Arg) end;
    is_function(Fun, 3) -> Fun
  end,
  F1 = fun(S, N, Arg) ->
    MyPid ! {retrieve, N, F0(S, N, Arg)},
    S
  end,
  broadcast(W, F1, Args),
  [    receive {retrieve, N, Value} -> Value end
    || N <- lists:seq(1, length(W))
  ].

%% @spec retrieve(W::worker_pool(), X) -> Values
%%    X = (   fun((S::worker_state()) -> term())
%%          | fun((S::worker_state(), N::integer()) -> term())
%%          | term()
%%    ),
%%    Values = [ term() ]
%% @doc
%% Each worker evaluates <code>Fun</code> and the results are returned.
%% The return value of retrieve(W, Fun, Args)
%% is a list whose <code>N</code>th element is the result of applying
%% <code>Fun</code> in the <code>N</code>th process.
%% If
%% <ul>
%%   <li><code>X</code> is a function with arity 1,
%%     then it is called with the current process state.
%%   </li>
%%   <li> <code>X</code> is a function with arity 2,
%%     then the first parameter is the current process state,
%%     and the second parameter is the index of the process.
%%   </li>
%%   <li>Otherwise, <code>X</code> is taken as the key for looking
%%     up a value in the state of each worker process.
%%     If no match is found, then the atom <code>'undefined'</code>
%%     is returned.
%%   </li>
%% </ul>
retrieve(W, X) ->
  F = if
    is_function(X, 1) -> fun(S, _N, _Arg) -> X(S) end;
    is_function(X, 2) -> fun(S, N, _Arg) -> X(S, N) end;
    true ->  % X is the key for looking up a value in the process state
      fun(S, _N, _Arg) -> get(S, X) end
    end,
  retrieve(W, F, W).

%% @spec update(W::worker_pool(), Key::term(), Fun, Args) -> ok
%%    Fun = (   fun((S::worker_state(), Arg) -> term())
%%            | fun((S::worker_state(), N::integer(), Arg) -> term())
%%          | term()
%%    ),
%%    Values = [ term() ]
%% @doc
%% Each worker updates the value in its state (<code>S</code>) associated
%% with <code>Key</code> to the result of applying Fun.  If there is not entry
%% in the current state for <code>Key</code>, then an entry is created.
%% If
%%  <ul>
%     <li><code>Fun</code> has arity 2,
%%      then it is called with the current process state and
%%       <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>,
%%      where <code>N</code> is the index of the process in <code>W</code>.
%%    </li>
%%    <li><code>Fun</code> has arity 3,
%%      then the first parameter is the current process state,
%%      the second parameter is the index of the process, <code>N</code>,
%%      and the third parameter is
%%      <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>.
%%   </li>
%% </ul>
update(W, Key, Fun, Args) ->
  F0 = if 
    is_function(Fun, 2) -> fun(S, _N, A) -> Fun(S, A) end;
    is_function(Fun, 3) -> fun(S, N, A) -> Fun(S, N, A) end
  end,
  F1 = fun(S, N, A) -> put(S, Key, F0(S, N, A)) end,
  broadcast(W, F1, Args).

%% @spec update(W::worker_pool(), Key::term(), X) -> ok
%%    Fun = (   fun((S::worker_state()) -> term())
%%            | fun((S::worker_state(), N::integer()) -> term())
%%            | [ term() ]
%%    ),
%%    Values = [ term() ]
%% @doc
%% Each worker updates the value in its state (<code>S</code>) associated with
%% <code>Key</code> to the result of applying Fun.  If there is no entry in
%% the current state for <code>Key</code>, then an entry is created.
%% If
%% <ul>
%%   <li><code>X</code> is a function with arity 1,
%%     then it is called with the current process state.
%%   </li>
%%   <li><code>X</code> is a function with arity 2,
%%     then it is called with the current process state and the index
%%     of the process.
%%   </li>
%%   <li><code>X</code> is a list, then
%%     <code><a href="http://www.erlang.org/doc/man/erlang.html#length-1">length</a>(X)</code>
%%     must be the same as the number of workers in <code>W</code>
%%     (i.e. <code>{@link nworkers/1. nworkers}(W)</code>).
%%     In this case, the value for <code>Key</code> in worker
%%     <code>N</code> is updated to the value of
%%     <code><a href="http://www.erlang.org/doc/man/lists.html#nth-2">lists:nth</a>(N, Args)</code>.
%%   </li>
%% </ul>
update(W, Key, X) ->
  F = if
    is_function(X, 0) -> fun(_S, _N, _A) -> X() end;
    is_function(X, 1) -> fun(S, _N, _A) -> X(S) end;
    is_function(X, 2) -> fun(S, N, _A) -> X(S, N) end;
    is_list(X) -> fun(_S, _N, A) -> A end
  end,
  update(W, Key, F, if is_list(X) -> X; true -> W end).


%% @spec rlist(W, N, M, Key) -> ok
%%   W = worker_pool(),
%%   N = integer(),
%%   M = number(),
%%   Key = term()
%% @doc  Generate a pseudo-random list distributed amongst the workers of
%%   worker pool W.  N is the total numbr of elements in the list.
%%   <ul>
%%     <li> if M is an integer, then each element is uniformly distributed
%%	      in 1..M.</li>
%%     <li> if M is a float, then each element is uniformly distributed
%%	      in [0, M].</li>
%%   </ul>
rlist(W, N, M, Key) -> rlist1({W, length(W)}, N, M, Key).

%% @spec rlist(W, N, Key) -> ok
%%   W = worker_pool(),
%%   N = integer(),
%%   Key = term()
%% @doc  Generate a pseudo-random list distributed amongst the workers of
%%   worker pool W.  N is the total numbr of elements in the list.
%%   Each random value is a float, uniformly distributed in [0, 1].
rlist(W, N, Key) -> rlist1({W, length(W)}, N, 1.0, Key).

% rlist1(W, N, M, Key):
%   Like rlist, we figure out how many elements the first worker should
%   get, send it the appropriate task, and then make a recursive call to
%   handle the other workers.
%   NW is # of workers
rlist1({[], 0}, _N, _M, _Key) -> ok;
rlist1({[W_head | W_tail], NW}, N, M, Key) ->
  N1 = round(N/NW),
  N2 = N - N1,
  W_head ! (fun(ProcState) -> rlist2(N1, M, ProcState, Key) end),
  rlist1({W_tail, NW-1}, N2, M, Key).

% rlist2(N, M, ProcState, Key)
%   The worker task for generating a random list.
%   It the worker already has a random-number generator state, use it.
%   Otherwise, create one based on our PID.  That gives each worker a
%   different random sequence.
%   We associate the random list that we create with Key in ProcState.
rlist2(N, M, ProcState, Key) ->
  R0 = fun() ->  % generate a process-specific random seed if no seed in ProcState
    X = erlang:phash2(self()),
    {X, X*(X+17), X*(X-42)*(X+18780101)}
  end,
  V = rlist3(N, M, [], workers:get(ProcState, randomState, R0)), % make a list
  workers:put(ProcState, lists:zip([Key, randomState], V)). % update ProcState

% rlist3(N, M, List, RandState0)
%   Generate a random list of N elements uniformly distributed in 1..M.
%   Prepend this randome list to List.  Use RandState 0 as the seed for
%   the random number generator.  Return the random list and the updated
%   random number generator state.
rlist3(0, _M, List, RandState0) -> [List, RandState0];
rlist3(N, M, List, RandState0) ->
  {RandNum, RandState1} = if
    is_integer(M) -> random:uniform_s(M, RandState0);
    is_float(M) -> M*random:uniform_s(RandState0)
  end,
  rlist3(N-1, M, [RandNum | List], RandState1).
