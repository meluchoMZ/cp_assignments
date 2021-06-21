-module(utils).
-export([for/7, send_message/2, length/1, map/2, fulfill/2, test/0, print_list/1, delete/2]).


%%% for loop implementation.

for(Init, End, Mod, Function, Arg1, Arg2, Caller) -> 
	for(Init, End, Mod, Function, Arg1, Arg2, Caller, []).

for(Init, End, Mod, Function, Arg1, Arg2, Caller, Proc_list) ->
	if (Init < End) -> 
		Pid = spawn_link(fun() -> Mod:Function(Arg1,Arg2,Caller) end), 
		for(Init+1, End, Mod, Function, Arg1, Arg2, Caller, [Pid | Proc_list]);
		true -> {ok, Proc_list}
	end.


%% sends a message to a list of processes
send_message([],_) -> {ok, sent};
send_message([H | T], Message) -> H  ! Message, send_message(T, Message).


%% calculates the length of a list
length(N) -> length_tail(0,N).

length_tail(L, []) -> L;
length_tail(L, [_ | T]) -> length_tail(L + 1, T).

%% applyes the given function to a list
map(F, L) -> map_tail(F, L, []).

map_tail(_,[],R) -> lists:reverse(R);
map_tail(F,[H | T], R) -> map_tail(F, T, [F(H) | R]).

%% returns true if all the elements fulfill the predicate P
fulfill(_, []) -> true;
fulfill(P, [H | T]) -> 
	case P(H) of
		true -> 
		fulfill(P, T);
		false -> 
		false
	end.

%% status test

test() -> 
	receive
		{Caller, status} ->
			Caller ! status;
		{error, Reason} ->
			io:format("Error: ~w~n", [Reason])
	end.


%% prints the content of the given list

print_list([]) ->
	io:format("~n");
print_list([H | T]) -> 
	io:format("~w ", [H]),
	print_list(T).


%% deletes the specified element from the list

delete([],_) -> [];
delete(List, Element) ->
	delete_tail(List, Element, []).

delete_tail([], _, Dev) ->
	Dev;
delete_tail([H | T], Element, Dev) ->
	case H of
		Element ->
			Dev++T;
		_ -> delete_tail(T, Element, [H | Dev])
	end.

