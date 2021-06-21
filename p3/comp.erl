-module(comp).

-export([comp/1, comp/2, comp_proc/2, comp_proc/3, comp_proc_loop/3, decomp/1, decomp/2, decomp_proc/2, decomp_proc/3, decomp_proc_loop/3]).


-define(DEFAULT_CHUNK_SIZE, 1024*1024).

%%% File Compression

comp(File) -> %% Compress file to file.ch
    comp(File, ?DEFAULT_CHUNK_SIZE).

comp(File, Chunk_Size) ->  %% Starts a reader and a writer which run in separate processes
    case file_service:start_file_reader(File, Chunk_Size) of
        {ok, Reader} ->
            case archive:start_archive_writer(File++".ch") of
                {ok, Writer} ->
                    comp_loop(Reader, Writer);
                {error, Reason} ->
                    io:format("Could not open output file: ~w~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Could not open input file: ~w~n", [Reason])
    end.

comp_loop(Reader, Writer) ->  %% Compression loop => get a chunk, compress it, send to writer
    Reader ! {get_chunk, self()},  %% request a chunk from the file reader
    receive
        {chunk, Num, Offset, Data} ->   %% got one, compress and send to writer
            Comp_Data = compress:compress(Data),
            Writer ! {add_chunk, Num, Offset, Comp_Data},
            comp_loop(Reader, Writer);
        eof ->  %% end of file, stop reader and writer
            Reader ! stop,
            Writer ! stop;
        {error, Reason} ->
            io:format("Error reading input file: ~w~n",[Reason]),
            Reader ! stop,
            Writer ! abort
    end.

%%% Multiprocess compression implementation

comp_proc(File, Procs) ->
	comp_proc(File, ?DEFAULT_CHUNK_SIZE, Procs).
	
comp_proc(File, Chunk_Size, Procs) ->
	case file_service:start_file_reader(File, Chunk_Size) of
		{ok, Reader} -> 
			case archive:start_archive_writer(File++".ch") of
				{ok, Writer} ->
					{ok, Proc_list}  = utils:for(0, Procs, comp, comp_proc_loop, Reader, Writer, self()), %% Proc_list has the list of the created processes running comp_proc_loop
						process_flag(trap_exit, true),
						manage_procs(Proc_list),
						Reader ! stop,
						Writer ! stop,
						{ok, compression_finished};
				{error, Reason} ->
					io:format("Could not open output file: ~w~n", [Reason])
			end;
		{error, Reason} ->
			io:format("Could not open input file: ~w~n", [Reason])
	end.

comp_proc_loop(Reader, Writer, Main) ->  %% Compression loop => get a chunk, compress it, send to writer
    Reader ! {get_chunk, self()},  %% request a chunk from the file reader
	receive
	    {chunk, Num, Offset, Data} ->   %% got one, compress and send to writer
	        Comp_Data = compress:compress(Data),
	        Writer ! {add_chunk, Num, Offset, Comp_Data},
	        comp_proc_loop(Reader, Writer, Main);
	    eof ->  %% end of file, advises main process that it has finished his work
			exit(normal);
	    {error, Reason} ->
	        io:format("Error reading input file: ~w~n",[Reason]),
	        Reader ! stop,
	        Writer ! abort
	end.
%% File Decompression

decomp(Archive) ->
    decomp(Archive, string:replace(Archive, ".ch", "", trailing)).

decomp(Archive, Output_File) ->
    case archive:start_archive_reader(Archive) of
        {ok, Reader} ->
            case file_service:start_file_writer(Output_File) of
                {ok, Writer} ->
                    decomp_loop(Reader, Writer);
                {error, Reason} ->
                    io:format("Could not open output file: ~w~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Could not open input file: ~w~n", [Reason])
    end.

decomp_loop(Reader, Writer) ->
    Reader ! {get_chunk, self()},  %% request a chunk from the reader
    receive
        {chunk, _Num, Offset, Comp_Data} ->  %% got one
            Data = compress:decompress(Comp_Data),
            Writer ! {write_chunk, Offset, Data},
            decomp_loop(Reader, Writer);
        eof ->    %% end of file => exit decompression
            Reader ! stop,
            Writer ! stop;
        {error, Reason} ->
            io:format("Error reading input file: ~w~n", [Reason]),
            Writer ! abort,
            Reader ! stop
    end.


%% Multiprocess file decompression

decomp_proc(Archive, Procs) ->
	decomp_proc(Archive, string:replace(Archive, ".ch", "", trailing), Procs).

decomp_proc(Archive, Output_File, Procs) ->
	case archive:start_archive_reader(Archive) of
	{ok, Reader} ->
		case file_service:start_file_writer(Output_File) of
			{ok, Writer} ->
				%% Proc_list has the list of the created processes running decomp_proc_loop
				{ok, Proc_list} = utils:for(0, Procs, comp, decomp_proc_loop, Reader, Writer, self()),
				process_flag(trap_exit, true),
				manage_procs(Proc_list),
				Reader ! stop,
				Writer ! stop,
				{ok, decompression, finished};
			{error, Reason} ->
				io:format("Could not open output file: ~w~n", [Reason])
		end;
	{error, Reason} ->
		io:format("Could not open input file: ~w~n", [Reason])
	end.

decomp_proc_loop(Reader, Writer, Main) -> %% Decompression loop => get comp_Data, decompress it, send to writer
	Reader ! {get_chunk, self()},
	receive
		{chunk, _Num, Offset, Comp_Data} ->
			Data = compress:decompress(Comp_Data),
			Writer ! {write_chunk, Offset, Data},
			decomp_proc_loop(Reader, Writer,Main);
		eof ->
			exit(normal);
		{error, Reason} ->
			io:format("Error reading input file: ~w~n", [Reason]),
			Reader ! stop,
			Writer ! abort
	end.



%%% Manages the comp_loop_proc workers
manage_procs([]) -> {ok, all_procs_stopped};
manage_procs(List) ->
	receive 
		{'EXIT', Pid, _} ->
			io:format("~w exited~n", [Pid]),
			manage_procs(utils:delete(List,Pid));
		_ -> 
			manage_procs(List)
	end.
