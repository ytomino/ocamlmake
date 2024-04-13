(* "gnatmake" like make tool for Objective Caml by YT *)

let usage = "Usage: ocamlmake <switches...> <source...>\n\
\n\
\  source is one or more file name from which you can omit the .ml suffix\n\
\n\
ocamlmake switches:\n\
\  -a                   Build a library\n\
\  -c                   Compile only (do not link)\n\
\  -ccopt <opt>         Pass option <opt> to the C compiler and linker\n\
\  -D <dir>             Specify dir as the object directory\n\
\  -f                   Force recompilations\n\
\  -h --help            Display this list of options\n\
\  -interact            Interactive mode\n\
\  -I <dir>             Add <dir> to the list of include directories\n\
\  -l <lib>             Link library\n\
\  -L <dir>             Look for program libraries also in dir\n\
\  -m                   Minimal recompilation\n\
\  -M                   List file dependencies saved in .ocamlmake\n\
\  -o <file>            Set output file name to <file>\n\
\  --ocamlc <command>   Set the OCaml bytecode compiler\n\
\  --ocamldep <command> Set the OCaml dependency tool\n\
\  --ocamlopt <command> Set the OCaml native compiler\n\
\  -O                   Optimization with ocamlopt instead of ocamlc\n\
\  -p                   Compile and link with profiling support\n\
\  -run                 Execute directly\n\
\  -S                   Keep intermediate assembly file\n\
\  -v --version         Print compiler version and exit\n\
\  -verbose             Print calls to external commands and file operations\n\
\n\
compiler switches (passed to the compiler by ocamlmake):\n\
\  -g           Save debugging information\n\
\  -noassert    Do not compile assertion checks\n\
\  -rectypes    Allow arbitrary recursive types\n\
\  -safe-string (was set when configuring the compiler)\n\
\  -thread      (deprecated) same as -I +threads\n\
\  -unsafe      Do not compile bounds checking on array and string access\n\
\  -w <list>    Enable or disable warnings according to:\n\
\     See ocamlc --warn-help.\n\
";;

module Queue = struct
	include Queue;;
	
	let mem item queue = (
		let module Local = struct exception Break end in
		try
			Queue.iter (fun it ->
				if it = item then raise Local.Break
			) queue;
			false
		with
		| Local.Break -> true
	);;
	
end;;

type target = Default | Optimized | CMI | CMO | CMX | CMA | CMXA | ByteExe | NativeExe | AsmSrc | Run | Interact;;

type compiler_switches = {
	mutable profiling: bool;
	mutable debug: bool;
	mutable noassert: bool;
	mutable rectypes: bool;
	mutable safe_string: bool;
	mutable thread: bool;
	mutable unsafe: bool;
	mutable outside: bool;
};;

type options = {
	mutable target: target;
	mutable target_name: string;
	mutable ocaml: string;
	mutable ocamlc: string;
	mutable ocamlcp: string;
	mutable ocamlopt: string;
	mutable ocamloptp: string;
	mutable ocamldep: string;
	mutable force: bool;
	mutable minimum: bool;
	mutable compiler: compiler_switches;
	mutable warnings: string;
	mutable source_files: string list;
	mutable libraries: string list;
	mutable reference_dirs: string list;
	mutable build_dir: string;
	mutable library_dirs: string list;
	mutable args: string;
	mutable largs: string;
	mutable print_dependency: bool;
	mutable help: bool;
	mutable version: bool;
	mutable verbose: bool;
	mutable error: bool};;

let rindex_extension_suffix s = (
	(* Remove .extension and -suffix. *)
	let p =
		try String.rindex s '-' with
		| Not_found -> String.length s
	in
	try String.rindex_from s (p - 1) '.' with
	| Not_found -> p
);;

let rindex_extension s = (
	try String.rindex s '.' with
	| Not_found -> String.length s
);;

let change_ocamlc_suffix suffix ocamlc = (
	let length = String.length ocamlc in
	(* e.g. ocamlc.opt-4.13 *)
	let p = rindex_extension_suffix ocamlc in
	if p > 0 && ocamlc.[p - 1] = 'c' then (
		String.sub ocamlc 0 (p - 1) ^ suffix ^ String.sub ocamlc p (length - p)
	) else
	(* e.g. x86-64-linux-gnu-ocamlc.opt *)
	let p = rindex_extension ocamlc in
	if p > 0 && ocamlc.[p - 1] = 'c' then (
		String.sub ocamlc 0 (p - 1) ^ suffix ^ String.sub ocamlc p (length - p)
	) else "ocaml" ^ suffix (* use default name *)
);;

let change_ocamlopt_suffix suffix ocamlopt = (
	let isopt s p = (
		p > 3 && s.[p - 3] = 'o' && s.[p - 2] = 'p' && s.[p - 1] = 't'
	) in
	let length = String.length ocamlopt in
	(* e.g. ocamlopt.opt-4.13 *)
	let p = rindex_extension_suffix ocamlopt in
	if isopt ocamlopt p then (
		String.sub ocamlopt 0 (p - 3) ^ suffix ^ String.sub ocamlopt p (length - p)
	) else
	(* e.g. x86-64-linux-gnu-ocamlopt.opt *)
	let p = rindex_extension ocamlopt in
	if isopt ocamlopt p then (
		String.sub ocamlopt 0 (p - 3) ^ suffix ^ String.sub ocamlopt p (length - p)
	) else "ocaml" ^ suffix (* use default name *)
);;

let options = (
	let options: options = {
		target = Default;
		target_name = "";
		ocaml = "ocaml";
		ocamlc = "ocamlc.opt";
		ocamlcp = "ocamlcp";
		ocamlopt = "ocamlopt.opt";
		ocamloptp = "ocamloptp";
		ocamldep = "ocamldep.opt";
		force = false;
		minimum = false;
		compiler = {profiling = false; debug = false; noassert = false; rectypes = false;
			safe_string = false; thread = false; unsafe = false; outside = false};
		warnings = "";
		source_files = [];
		libraries = [];
		reference_dirs = [];
		build_dir = "";
		library_dirs = [];
		args = "";
		largs = "";
		print_dependency = false;
		help = false;
		version = false;
		verbose = false;
		error = false
	} in
	let length = Array.length Sys.argv in
	let i = ref 1 in
	while !i < length do
		let arg = Sys.argv.(!i) in
		if options.target = Run && options.source_files <> [] then (
			options.args <- options.args ^ " " ^ arg
		) else if arg = "-a" then (
			begin match options.target with
			| Default -> options.target <- CMA
			| Optimized -> options.target <- CMXA
			| CMA | CMXA -> ()
			| CMI | CMO | CMX | ByteExe | NativeExe | AsmSrc | Run | Interact ->
				prerr_string "mismatched option: -a\n";
				options.error <- true
			end
		) else if arg = "-c" then (
			begin match options.target with
			| Default -> options.target <- CMO
			| Optimized -> options.target <- CMX
			| CMI | CMO | CMX -> ()
			| CMA | CMXA | ByteExe | NativeExe | AsmSrc | Run | Interact ->
				prerr_string "mismatched option: -c\n";
				options.error <- true
			end
		) else if arg = "-ccopt" || arg = "--ccopt" then (
			incr i;
			let escaped = String.escaped Sys.argv.(!i) in
			options.largs <- options.largs ^ " -ccopt \"" ^ escaped ^ "\""
		) else if String.length arg >= 2 && arg.[0] = '-' && arg.[1] = 'D' then (
			let dir = (
				if String.length arg > 2 then (
					String.sub arg 2 (String.length arg - 2)
				) else (
					incr i;
					Sys.argv.(!i)
				)
			) in
			options.build_dir <- dir
		) else if arg = "-f" then (
			options.force <- true
		) else if arg = "-g" then (
			options.compiler.debug <- true
		) else if arg = "-h" || arg = "--help" then (
			options.help <- true
		) else if arg = "-interact" || arg = "--interact" then (
			begin match options.target with
			| Default -> options.target <- Interact
			| Interact -> ()
			| Optimized | CMI | CMO | CMA | CMX | CMXA | ByteExe | NativeExe | AsmSrc | Run ->
				prerr_string "mismatched option: -interact\n";
				options.error <- true
			end
		) else if String.length arg >= 2 && arg.[0] = '-' && arg.[1] = 'I' then (
			let dir = (
				if String.length arg > 2 then (
					String.sub arg 2 (String.length arg - 2)
				) else (
					incr i;
					Sys.argv.(!i)
				)
			) in
			if not (List.mem dir options.reference_dirs) then (
				options.reference_dirs <- dir :: options.reference_dirs
			)
		) else if String.length arg >= 2 && arg.[0] = '-' && arg.[1] = 'l' then (
			let lib = (
				if String.length arg > 2 then (
					String.sub arg 2 (String.length arg - 2)
				) else (
					incr i;
					Sys.argv.(!i)
				)
			) in
			options.libraries <- lib :: options.libraries
		) else if String.length arg >= 2 && arg.[0] = '-' && arg.[1] = 'L' then (
			let dir = (
				if String.length arg > 2 then (
					String.sub arg 2 (String.length arg - 2)
				) else (
					incr i;
					Sys.argv.(!i)
				)
			) in
			options.library_dirs <- dir :: options.library_dirs
		) else if arg = "-m" then (
			options.minimum <- true
		) else if arg = "-M" then (
			options.print_dependency <- true
		) else if arg = "-noassert" || arg = "--noassert" then (
			options.compiler.noassert <- true
		) else if (String.length arg >= 2 && arg.[0] = '-' && arg.[1] = 'o') || arg = "--output" then (
			let name = (
				if String.length arg > 2 && arg.[1] = 'o' then (
					String.sub arg 2 (String.length arg - 2)
				) else (
					incr i;
					Sys.argv.(!i)
				)
			) in
			options.target_name <- name;
			let target_by_filename = (
				if Filename.check_suffix name ".S" || Filename.check_suffix name ".s" || Filename.check_suffix name ".asm" then (
					AsmSrc
				) else if Filename.check_suffix name ".cmi" then (
					CMI
				) else if Filename.check_suffix name ".cmo" then (
					CMO
				) else if Filename.check_suffix name ".cma" then (
					CMA
				) else if Filename.check_suffix name ".cmx" then (
					CMX
				) else if Filename.check_suffix name ".cmxa" then (
					CMXA
				) else (
					Default
				)
			) in
			begin match options.target, target_by_filename with
			| Run, _ ->
				prerr_string "mismatched option: -o\n";
				options.error <- true
			| Default, Default -> options.target <- ByteExe
			| Default, _ -> options.target <- target_by_filename
			| _, Default -> ()
			| x, y when x = y -> ()
			| CMO, CMI -> options.target <- CMI
			| CMX, CMI -> options.target <- CMI
			| CMO, CMX -> options.target <- CMX
			| CMA, CMXA -> options.target <- CMXA
			| _ ->
				prerr_string "mismatched output filename: ";
				prerr_string name;
				prerr_newline ();
				options.error <- true
			end
		) else if arg = "--ocamlc" then (
			incr i;
			let ocamlc = Sys.argv.(!i) in
			options.ocaml <- change_ocamlc_suffix "" ocamlc;
			options.ocamlc <- ocamlc;
			options.ocamlcp <- change_ocamlc_suffix "cp" ocamlc;
			options.ocamlopt <- change_ocamlc_suffix "opt" ocamlc;
			options.ocamloptp <- change_ocamlc_suffix "optp" ocamlc
		) else if arg = "--ocamldep" then (
			incr i;
			let ocamldep = Sys.argv.(!i) in
			options.ocamldep <- ocamldep
		) else if arg = "--ocamlopt" then (
			incr i;
			let ocamlopt = Sys.argv.(!i) in
			options.ocamlopt <- ocamlopt;
			options.ocamloptp <- change_ocamlopt_suffix "optp" ocamlopt
		) else if arg = "-O" then (
			begin match options.target with
			| Default -> options.target <- Optimized
			| CMO -> options.target <- CMX
			| CMA -> options.target <- CMXA
			| ByteExe -> options.target <- NativeExe
			| Optimized | CMI | CMX | CMXA | NativeExe | AsmSrc -> ()
			| Run | Interact ->
				prerr_string "mismatched option: -O\n";
				options.error <- true
			end
		) else if arg = "-p" then (
			options.compiler.profiling <- true
		) else if arg = "-rectypes" || arg = "--rectypes" then (
			options.compiler.rectypes <- true
		) else if arg = "-run" || arg = "--run" then (
			begin match options.target with
			| Default -> options.target <- Run
			| Run -> ()
			| Optimized | CMI | CMO | CMA | CMX | CMXA | ByteExe | NativeExe | AsmSrc | Interact ->
				prerr_string "mismatched option: -run\n";
				options.error <- true
			end
		) else if arg = "-safe-string" || arg = "--safe-string" then (
			options.compiler.safe_string <- true
		) else if arg = "-S" then (
			begin match options.target with
			| Default | Optimized -> options.target <- AsmSrc
			| AsmSrc -> ()
			| CMI | CMO | CMX | CMA | CMXA | ByteExe | NativeExe | Run | Interact ->
				prerr_string "mismatched option: -c\n";
				options.error <- true
			end
		) else if arg = "-thread" || arg = "--thread" then (
			options.compiler.thread <- true
		) else if arg = "-unsafe" || arg = "--unsafe" then (
			options.compiler.unsafe <- true
		) else if arg = "-v" || arg = "--version" then (
			options.version <- true
		) else if arg = "-verbose" || arg = "--verbose" then (
			options.verbose <- true
		) else if String.length arg >= 2 && arg.[0] = '-' && arg.[1] = 'w' then (
			let opt = (
				if String.length arg > 2 then (
					String.sub arg 2 (String.length arg - 2)
				) else (
					incr i;
					Sys.argv.(!i)
				)
			) in
			options.warnings <- options.warnings ^ opt
		) else if arg.[0] = '-' then (
			prerr_string "unknown option: ";
			prerr_string arg;
			prerr_newline ();
			options.error <- true
		) else (
			let f = if String.contains arg '.' || Sys.file_exists arg then arg else arg ^ ".ml" in
			let dir = Filename.dirname f in
			let f =
				if dir = Filename.current_dir_name then f else (
					if not (List.mem dir options.reference_dirs) then (
						options.reference_dirs <- dir :: options.reference_dirs
					);
					Filename.basename f
				)
			in
			options.source_files <- f :: options.source_files
		);
		incr i
	done;
	if List.length options.source_files > 1 then (
		begin match options.target with
		| CMI | CMO | CMX | AsmSrc ->
			if options.target_name <> "" then (
				prerr_string "mismatched option: -o\n";
				options.error <- true
			)
		| Run ->
			prerr_string "mismatched option: -run\n";
			options.error <- true
		| NativeExe | ByteExe | CMXA | CMA | Optimized | Default | Interact ->
			()
		end
	);
	options.source_files <- List.rev options.source_files;
	options.libraries <- List.rev options.libraries;
	options.reference_dirs <- List.rev options.reference_dirs;
	options
);;

let usage (): int = (
	print_string usage;
	0
);;

let version (): int = (
	print_string "ocamlmake for ";
	flush stdout;
	let cmd = options.ocamlc ^ " -version" in
	Sys.command cmd
);;

let prerr_error (): unit = (
	prerr_string Sys.argv.(0);
	prerr_string ": error: ";
);;

let prerr_info (): unit = (
	prerr_string Sys.argv.(0);
	prerr_string ": info: ";
);;

let prerr_command (command: string): unit = (
	prerr_string command;
	prerr_newline ()
);;

if options.error then exit 1;;
if options.version then exit (version ());;
if options.help || (options.source_files = [] && not options.print_dependency) then exit (usage ());;

if options.build_dir <> "" && not (try Sys.is_directory options.build_dir with Sys_error _ -> false) then (
	if options.verbose then (
		prerr_info ();
		prerr_string "mkdir ";
		prerr_endline options.build_dir
	);
	Unix.mkdir options.build_dir 0o755
);;

let known_library = [
	("dynlink", ([], ["dynlink"]));
	("event", (["+threads"], ["unix"; "threads"]));
	("raw_spacetime_lib", ([], ["raw_spacetime_lib"]));
	("runtime_events", ([], ["runtime_events"]));
	("str", ([], ["str"]));
	("thread", (["+threads"], ["unix"; "threads"]));
	("unix", ([], ["unix"]));
	("unixLabels", ([], ["unix"]))
];;

let find_source (name: string): string option = (
	if Sys.file_exists name then (
		Some name
	) else (
		let rec loop rs = (
			begin match rs with
			| [] -> None
			| r :: rr ->
				let path = Filename.concat r name in
				if Sys.file_exists path then (
					Some path
				) else (
					loop rr
				)
			end
		) in
		loop options.reference_dirs
	)
);;

type source_kind = ML | MLI;;

type source_info = {
	kind: source_kind;
	switches: compiler_switches;
	depends: string list;
	depending_library_dirs: string list;
	depending_libraries: string list};;
	
let ocamldep (source_file: string): string list * string list * string list = (
	let result_mls = ref [] in
	let result_dir = ref [] in
	let result_lib = ref [] in
	let command = (
		let r = Buffer.create 0 in
		Buffer.add_string r options.ocamldep;
		List.iter (fun dir ->
			Buffer.add_string r " -I ";
			Buffer.add_string r dir;
		) options.reference_dirs;
		Buffer.add_string r " ";
		Buffer.add_string r source_file;
		Buffer.contents r
	) in
	if options.verbose then (
		prerr_command command
	);
	let p_in = Unix.open_process_in command in
	let dep = (
		let r = Buffer.create 0 in
		begin try
			let next = ref true in
			while !next do
				let line = input_line p_in in
				let line = (if line.[String.length line - 1] = '\r' then String.sub line 0 (String.length line - 1) else line) in
				Buffer.add_string r line;
				next := line.[String.length line - 1] = '\\'
			done
		with
		| End_of_file -> ()
		end;
		Buffer.contents r
	) in
	begin match Unix.close_process_in p_in with
	| Unix.WEXITED 0 ->
		let start = (try String.index dep ':' + 1 with Not_found -> -1) in
		if start >= 0 then (
			let rec loop s i = (
				if s >= String.length dep then (
					()
				) else if dep.[s] <= ' ' then (
					loop (s + 1) i
				) else if i >= String.length dep || dep.[i] <= ' ' then (
					if i > s then (
						let f = String.sub dep s (i - s) in
						if f <> "\\" then (
							(* print_string f; print_newline (); *)
							let b = Filename.basename f in
							let c = Filename.chop_extension b in
							let mli_filename = c ^ ".mli" in
							let d = (
								if find_source mli_filename <> None then (
									mli_filename
								) else (
									c ^ ".ml"
								)
							) in
							if d <> source_file then (
								result_mls := d :: !result_mls
							)
						)
					);
					loop (i + 1) (i + 1)
				) else (
					loop s (i + 1)
				)
			) in
			loop start start
		)
	| _ ->
		prerr_error ();
		prerr_endline command;
		exit 1
	end;
	let command = options.ocamldep ^ " -modules " ^ source_file in
	if options.verbose then (
		prerr_command command
	);
	let p_in = Unix.open_process_in command in
	let dep = (try input_line p_in with End_of_file -> "") in
	begin match Unix.close_process_in p_in with
	| Unix.WEXITED 0 ->
		let start = (try String.index dep ':' + 1 with Not_found -> -1) in
		if start >= 0 && start < String.length dep then (
			let start = (if dep.[start] = ' ' then start + 1 else start) in
			let rec loop s i = (
				if i >= String.length dep || dep.[i] <= ' ' then (
					if i > s then (
						let f = Bytes.sub (Bytes.unsafe_of_string dep) s (i - s) in
						Bytes.set f 0 (Char.lowercase_ascii (Bytes.get f 0));
						let f = Bytes.unsafe_to_string f in
						if List.mem_assoc f known_library then (
							let dirs, libs = List.assoc f known_library in
							result_dir := List.rev_append dirs !result_dir;
							result_lib := List.rev_append libs !result_lib
						)
					);
					if i < String.length dep then (
						loop (i + 1) (i + 1)
					)
				) else (
					loop s (i + 1)
				)
			) in
			loop start start
		)
	| _ ->
		prerr_error ();
		prerr_endline command;
		exit 1
	end;
	!result_mls, List.rev (!result_dir), List.rev (!result_lib)
);;

let execute (command: string): unit = (
	prerr_command command;
	if Sys.command command <> 0 then exit 1
);;

let build_info_revision = 0x102;; (* increment when binary format is changed *)

let build_info_filename = Filename.concat options.build_dir ".ocamlmake";;

type build_info = {
	source_info: (string, source_info) Hashtbl.t;
	mutable ext_exe: string option;
};;

let build_info: build_info = (
	let default_build_info () = (
		{
			source_info = Hashtbl.create 13;
			ext_exe = None
		}
	) in
	match open_in_bin build_info_filename with
	| _ as f ->
		let revision = input_binary_int f in
		let r =
			if revision = build_info_revision then Marshal.from_channel f
			else default_build_info ()
		in
		close_in f;
		if options.verbose then (
			prerr_info ();
			prerr_string "read ";
			prerr_endline build_info_filename
		);
		r
	| exception Sys_error _ ->
		default_build_info ()
);;

if options.print_dependency then (
	Hashtbl.iter (fun s {depends = deps; _} ->
		print_string s;
		print_string ":";
		List.iter (fun d ->
			print_string " ";
			print_string d;
		) deps;
		print_newline ()
	) build_info.source_info;
	exit 0
);;

let save_build_info () = (
	let f = open_out_bin build_info_filename in
	output_binary_int f build_info_revision;
	Marshal.to_channel f build_info [];
	close_out f;
	if options.verbose then (
		prerr_info ();
		prerr_string "write ";
		prerr_endline build_info_filename
	);
);;
at_exit save_build_info;;

let mem_info (source_file: string): bool = (
	Hashtbl.mem build_info.source_info source_file
);;

let get_info source_file source_file_with_path kind remake = (
	if not remake && Hashtbl.mem build_info.source_info source_file then (
		(Hashtbl.find build_info.source_info source_file, false)
	) else (
		let depends, depending_library_dirs, depending_libraries =
			ocamldep source_file_with_path
		in
		{
			kind;
			switches = {options.compiler with outside = true};
			depends;
			depending_library_dirs;
			depending_libraries
		}, true
	)
);;

let set_info source_file (info: source_info): unit = (
	Hashtbl.replace build_info.source_info source_file info
);;

let the_compiler = (
	begin match options.target with
	| Default | CMO | CMA | ByteExe | CMI | Run | Interact ->
		if options.compiler.profiling then options.ocamlcp else options.ocamlc
	| Optimized | CMX | CMXA | NativeExe | AsmSrc ->
		if options.compiler.profiling then options.ocamloptp else options.ocamlopt
	end
);;

let compiled_extension = (
	begin match options.target with
	| Default | CMO | CMA | ByteExe | CMI | Run | Interact -> ".cmo"
	| Optimized | CMX | CMXA | NativeExe | AsmSrc -> ".cmx"
	end
);;

let library_extension = (
	begin match options.target with
	| Default | CMO | CMA | ByteExe | CMI | Run | Interact -> ".cma"
	| Optimized | CMX | CMXA | NativeExe | AsmSrc -> ".cmxa"
	end
);;

let buffer_add_compiler_switches buffer compiler = (
	if compiler.debug then Buffer.add_string buffer " -g";
	if compiler.noassert then Buffer.add_string buffer " -noassert";
	if compiler.rectypes then Buffer.add_string buffer " -rectypes";
	if compiler.safe_string then Buffer.add_string buffer " -safe-string";
	if compiler.thread then Buffer.add_string buffer " -thread";
	if compiler.unsafe then Buffer.add_string buffer " -unsafe"
);;

let library_dirs: string Queue.t = Queue.create ();;
let link_files: string Queue.t = Queue.create ();;

let latest: float ref = ref 0.0;;

let build_files: (string, unit) Hashtbl.t = Hashtbl.create 13 in
let rec loop source_files = (
	let rec build source_file = (
		if not (Hashtbl.mem build_files source_file) then (
			Hashtbl.add build_files source_file ();
			let compile kind = (
				let source_file_with_path = (
					match find_source source_file with
					| Some x -> x
					| None -> source_file
				) in
				let run = options.target = Run && List.mem source_file options.source_files in
				let asm = options.target = AsmSrc && List.mem source_file options.source_files in
				let compiled_filename = (
					if run then (
						source_file_with_path
					) else (
						Filename.concat options.build_dir (
							Filename.chop_extension source_file ^ (
								if kind = MLI then ".cmi" else compiled_extension
							)
						)
					)
				) in
				let (info, new_info) = get_info source_file source_file_with_path kind false in
				let be_compiling = (
					if run then (
						false
					) else if options.force || asm then (
						true
					) else if (
						if kind = ML && not new_info && not run then (
							if (
								let mli_name = Filename.chop_extension source_file ^ ".mli" in
								if not (List.mem mli_name info.depends) then (
									find_source mli_name <> None
								) else (
									false
								)
							) then (
								true
							) else (
								List.exists (fun d ->
									if Filename.check_suffix d ".ml" then (
										let mli_name = Filename.chop_extension d ^ ".mli" in
										if not (List.mem mli_name info.depends) then (
											find_source mli_name <> None
										) else (
											false
										)
									) else (
										false
									)
								) info.depends
							)
						) else (
							false
						)
					) then (
						true
					) else (
						begin try
							let source_stat = Unix.stat source_file_with_path in
							begin try
								let compiled_stat = Unix.stat compiled_filename in
								if compiled_stat.Unix.st_mtime > !latest then latest := compiled_stat.Unix.st_mtime;
								source_stat.Unix.st_mtime > compiled_stat.Unix.st_mtime || (info.switches <> options.compiler && not options.minimum)
							with
							| Unix.Unix_error (Unix.ENOENT, _, _) -> true
							end
						with
						| Unix.Unix_error (Unix.ENOENT, _, _) -> false
						end
					)
				) in
				(* Printf.printf "be compiling %s %b\n" source_file be_compiling; *)
				let info = (
					if be_compiling && not new_info then (
						let (i, _) = get_info source_file source_file_with_path kind true in i
					) else (
						info
					)
				) in
				List.iter build info.depends;
				let be_compiling = be_compiling || not run && (
					begin try
						let compiled_stat = Unix.stat compiled_filename in
						List.exists (fun ref ->
							if Filename.check_suffix ref ".ml"
								&& mem_info (Filename.chop_extension source_file ^ ".mli")
							then (
								true
							) else (
								let cmi_filename source_file = (
									Filename.chop_extension (
										Filename.concat options.build_dir (Filename.basename source_file)
									) ^ ".cmi"
								) in
								begin try
									let depends_stat = Unix.stat (cmi_filename ref) in
									depends_stat.Unix.st_mtime > compiled_stat.Unix.st_mtime
								with
								| Unix.Unix_error (Unix.ENOENT, _, _) -> true
								end
							)
						) info.depends
					with
					| Unix.Unix_error (Unix.ENOENT, _, _) -> false
					end
				) in
				(* Printf.printf "be compiling %s %d\n" source_file (Obj.magic be_compiling); *)
				if be_compiling then (
					let compile_command = (
						let command = Buffer.create 0 in
						Buffer.add_string command the_compiler;
						Buffer.add_string command " -c";
						if asm then Buffer.add_string command " -S";
						buffer_add_compiler_switches command options.compiler;
						if options.warnings <> "" then (
							Buffer.add_string command " -w ";
							Buffer.add_string command options.warnings
						);
						if options.verbose then (
							Buffer.add_string command " -verbose";
						);
						let add_I dir = (
							Buffer.add_string command " -I ";
							Buffer.add_string command dir
						) in
						List.iter add_I info.depending_library_dirs;
						List.iter add_I options.library_dirs;
						if options.build_dir <> "" then (
							add_I options.build_dir
						);
						Buffer.add_string command " -o ";
						Buffer.add_string command compiled_filename;
						Buffer.add_string command " ";
						Buffer.add_string command source_file_with_path;
						Buffer.contents command;
					) in
					execute compile_command;
					set_info source_file {info with switches = options.compiler};
					if asm then (
						let oldpath =
							let s = Filename.chop_extension compiled_filename ^ ".s" in
							if Sys.file_exists s then s
							else Filename.chop_extension compiled_filename ^ ".asm"
						in
						let newpath =
							if options.target_name = "" then Filename.basename oldpath
							else options.target_name
						in
						if oldpath <> newpath then (
							if options.verbose then (
								prerr_info ();
								prerr_string "rename ";
								prerr_string oldpath;
								prerr_string " to ";
								prerr_endline newpath;
							);
							Sys.rename oldpath newpath
						)
					);
					latest := Unix.time ()
				);
				List.iter (fun dir ->
					if not (Queue.mem dir library_dirs) then Queue.add dir library_dirs
				) info.depending_library_dirs;
				List.iter (fun lib ->
					let lib = lib ^ library_extension in
					if not (Queue.mem lib link_files) then Queue.add lib link_files
				) info.depending_libraries;
				if kind = MLI then (
					let ml = Filename.chop_extension source_file ^ ".ml" in
					build ml
				) else if not (Queue.mem compiled_filename link_files) then (
					Queue.add compiled_filename link_files
				)
			) in
			if Filename.check_suffix source_file ".ml" then (
				compile ML
			) else if Filename.check_suffix source_file ".mli" then (
				compile MLI
			) else (
				Queue.add source_file link_files
			)
		)
	) in
	begin match source_files with
	| [] -> ()
	| s :: sr ->
		build s;
		loop sr
	end
) in
loop options.source_files;;

let guess_ext_exe () = (
	match build_info.ext_exe with
	| None ->
		let command = the_compiler ^ " -config-var ext_exe" in
		if options.verbose then (
			prerr_command command
		);
		let p_in = Unix.open_process_in command in
		let ext_exe = (try input_line p_in with End_of_file -> "") in
		begin match Unix.close_process_in p_in with
		| Unix.WEXITED 0 ->
			let result = Some ext_exe in
			build_info.ext_exe <- result;
			result
		| _ ->
			prerr_error ();
			prerr_endline command;
			exit 1;
		end;
	| Some _ as result ->
		result
);;

begin match options.target with
| CMI | CMO | CMX | AsmSrc -> ()
| Run | Interact ->
	let run_command = (
		let result = Buffer.create 0 in
		Buffer.add_string result options.ocaml;
		let add_I dir = (
			Buffer.add_string result " -I ";
			Buffer.add_string result dir
		) in
		Queue.iter add_I library_dirs;
		List.iter add_I options.library_dirs;
		if options.build_dir <> "" then (
			add_I options.build_dir
		);
		List.iter (fun i ->
			Buffer.add_string result " ";
			Buffer.add_string result i;
			Buffer.add_string result library_extension
		) options.libraries;
		Queue.iter (fun i ->
			Buffer.add_string result " ";
			Buffer.add_string result i
		) link_files;
		Buffer.add_string result options.args;
		Buffer.contents result
	) in
	execute run_command
| Default | Optimized | CMA | CMXA | ByteExe | NativeExe ->
	let target_name = (
		if options.target_name <> "" then (
			options.target_name
		) else (
			begin match options.source_files with
			| s :: _ ->
				let r = Filename.chop_extension (Filename.basename s) in
				begin match options.target with
				| CMA -> r ^ ".cma"
				| CMXA -> r ^ ".cmxa"
				| Default | Optimized | ByteExe | NativeExe ->
					begin match guess_ext_exe () with
					| None -> r
					| Some ext_exe -> r ^ ext_exe
					end
				| _ -> assert false
				end
			| [] -> assert false
			end
		)
	) in
	if options.force || (
		try
			let target_stat = Unix.stat target_name in
			!latest > target_stat.Unix.st_mtime
		with Unix.Unix_error (Unix.ENOENT, _, _) -> true
	) then (
		let link_command = (
			let result = Buffer.create 0 in
			Buffer.add_string result the_compiler;
			buffer_add_compiler_switches result options.compiler;
			if options.verbose then (
				Buffer.add_string result " -verbose";
			);
			Buffer.add_string result " -o ";
			Buffer.add_string result target_name;
			begin match options.target with
			| CMA | CMXA ->
				Buffer.add_string result " -a";
			| _ ->
				()
			end;
			let add_I dir = (
				Buffer.add_string result " -I ";
				Buffer.add_string result dir
			) in
			Queue.iter add_I library_dirs;
			List.iter add_I options.library_dirs;
			List.iter (fun i ->
				Buffer.add_string result " ";
				Buffer.add_string result i;
				Buffer.add_string result library_extension
			) options.libraries;
			Queue.iter (fun i ->
				Buffer.add_string result " ";
				Buffer.add_string result i
			) link_files;
			Buffer.add_string result options.largs;
			Buffer.contents result
		) in
		execute link_command
	)
end;;
