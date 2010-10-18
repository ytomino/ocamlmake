(* "gnatmake" like make tool for Objective Caml by YT *)

let usage = "Usage: ocamlmake <switche...> <source...>\n\
\n\
\  source is one or more file name from which you can omit the .ml suffix\n\
\n\
ocamlmake switches:\n\
\  -a              Build a library\n\
\  -c              Compile only (do not link)\n\
\  -D <dir>        Specify dir as the object directory\n\
\  -f              Force recompilations\n\
\  --gcaml         Use G'Caml instead of O'Caml\n\
\  -h --help       Display this list of options\n\
\  -I <dir>        Add <dir> to the list of include directories\n\
\  -l <lib>        Link library\n\
\  -L <dir>        Look for program libraries also in dir\n\
\  -M              List file dependences saved in .ocamlmake\n\
\  -m              Minimal recompilation\n\
\  -mwindows       Create window mode application (Windows only)\n\
\  -O              Optimization with ocamlopt instead of ocamlc\n\
\  -o <file>       Set output file name to <file>\n\
\  -run            Execute directly\n\
\  -interact       Interacive mode\n\
\  -S              Keep intermediate assembly file\n\
\  -v --version    Print compiler version and exit\n\
\n\
compiler switches (passed to the compiler by ocamlmake):\n\
\  -g         Save debugging information\n\
\  -noassert  Don't compile assertion checks\n\
\  -p         Compile and link with profiling support\n\
\  -rectypes  Allow arbitrary recursive types\n\
\  -thread    Generate code that supports the system threads library\n\
\  -unsafe    No bounds checking on array and string access\n\
\  -w <flags> Enable or disable warnings according to <flags>:\n\
\     C/c enable/disable suspicious comment\n\
\     D/d enable/disable deprecated features\n\
\     E/e enable/disable fragile match\n\
\     F/f enable/disable partially applied function\n\
\     L/l enable/disable labels omitted in application\n\
\     M/m enable/disable overriden methods\n\
\     P/p enable/disable partial match\n\
\     S/s enable/disable non-unit statement\n\
\     U/u enable/disable unused match case\n\
\     V/v enable/disable overriden instance variables\n\
\     Y/y enable/disable suspicious unused variables\n\
\     Z/z enable/disable all other unused variables\n\
\     X/x enable/disable all other warnings\n\
\     A/a enable/disable all warnings\n\
\     default setting is \"Aelz\"\n\
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
	mutable ocamlprof: string;
	mutable ocamlopt: string;
	mutable ocamldep: string;
	mutable force: bool;
	mutable minimum: bool;
	mutable compiler: compiler_switches;
	mutable warnings: string;
	mutable source_files: string list;
	mutable libraries: string list;
	mutable reference_dirs: string list;
	mutable build_dir: string;
	mutable lib_dir: string;
	mutable library_dirs: string list;
	mutable args: string;
	mutable largs: string;
	mutable print_dependency: bool;
	mutable help: bool;
	mutable version: bool;
	mutable error: bool};;
	
let options = (
	let options: options = {
		target = Default;
		target_name = "";
		ocaml = "ocaml";
		ocamlc = "ocamlc.opt";
		ocamlcp = "ocamlcp";
		ocamlprof = "ocamlprof";
		ocamlopt = "ocamlopt.opt";
		ocamldep = "ocamldep";
		force = false;
		minimum = false;
		compiler = {profiling = false; debug = false; noassert = false; rectypes = false;
			thread = false; unsafe = false; outside = false};
		warnings = "";
		source_files = [];
		libraries = [];
		reference_dirs = [];
		build_dir = "";
		lib_dir = (try Sys.getenv "OCAMLLIB" with Not_found -> "/usr/local/lib/ocaml");
		library_dirs = [];
		args = "";
		largs = "";
		print_dependency = false;
		help = false;
		version = false;
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
		) else if arg = "-f" then (
			options.force <- true
		) else if arg = "-m" then (
			options.minimum <- true
		) else if arg = "-g" then (
			options.compiler.debug <- true
		) else if arg = "--gcaml" then (
			options.ocaml <- "gcaml";
			options.ocamlc <- "gcamlc";
			options.ocamlcp <- "gcamlcp";
			options.ocamlprof <- "gcamlprof";
			options.ocamlopt <- "gcamlopt";
			options.ocamldep <- "gcamldep";
			options.lib_dir <- (try Sys.getenv "GCAMLLIB" with Not_found -> "/usr/local/lib/gcaml")
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
		) else if arg = "-M" then (
			options.print_dependency <- true
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
		) else if arg = "-thread" || arg = "--thread" then (
			options.compiler.thread <- true
		) else if arg = "-unsafe" || arg = "--unsafe" then (
			options.compiler.unsafe <- true
		) else if arg = "-v" || arg = "--version" then (
			options.version <- true
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
		) else if String.length arg >= 2 && arg.[0] = '-' && arg.[1] = 'I' then (
			let dir = (
				if String.length arg > 2 then (
					String.sub arg 2 (String.length arg - 2)
				) else (
					incr i;
					Sys.argv.(!i)
				)
			) in
			options.reference_dirs <- dir :: options.reference_dirs
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
		) else if arg = "-S" then (
			begin match options.target with
			| Default | Optimized -> options.target <- AsmSrc
			| AsmSrc -> ()
			| CMI | CMO | CMX | CMA | CMXA | ByteExe | NativeExe | Run | Interact ->
				prerr_string "mismatched option: -c\n";
				options.error <- true
			end
		) else if arg = "-mwindows" || arg = "--mwindows" then (
			options.largs <- options.largs ^ " -cclib -mwindows"
		) else if arg.[0] = '-' then (
			prerr_string "unknown option: ";
			prerr_string arg;
			prerr_newline ();
			options.error <- true
		) else (
			let f = if String.contains arg '.' || Sys.file_exists arg then arg else arg ^ ".ml" in
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

if options.error then exit 1;;
if options.version then exit (version ());;
if options.help || (options.source_files = [] && not options.print_dependency) then exit (usage ());;

if options.build_dir <> "" && not (try Sys.is_directory options.build_dir with Sys_error _ -> false) then (
	Unix.mkdir options.build_dir 0o755
);;

let known_library = [
	("bigarray", ["bigarray"]);
	("big_int", ["nums"]);
	("bz", ["bz"]); (* bzip2 (Ocamlplot) *)
	("curl", ["curl"]); (* ocurl *)
	("dfm", ["bigarray"; "unicode"; "dfm"]); (* dfm-ocaml *)
	("dfm_text", ["bigarray"; "unicode"; "dfm"]); (* dfm-ocaml *)
	("dynlink", ["dynlink"]);
	("glcaml", ["bigarray"; "glcaml"]); (* GLCaml *)
	("gmp", ["gmp"]); (* OCaml-GMP or gmp-ocaml *)
	("gcaml", ["gcamllib"]); (* G'Caml *)
	("gprint", ["gcamllib"]); (* G'Caml *)
	("graphics", ["graphics"]);
	("gz", ["gz"]); (* zlib (Ocamlplot) *)
	("iconv", ["iconv"]); (* iconv-ocaml *)
	("mpc", ["mpc"; "mpfr"; "gmp"]); (* gmp-ocaml *)
	("mpfr", ["mpfr"; "gmp"]); (* gmp-ocaml *)
	("num", ["nums"]);
	("screen", ["bigarray"; "unicode"; "glcaml"; "sdl"; "sdl_image"; "sdl_mixer"; "sdl_ttf"; "screen"]); (* gl-helper *)
	("serialize", ["bigarray"; "unicode"; "dfm"]); (* dfm-ocaml *)
	("serialize_pp", ["bigarray"; "unicode"; "dfm"]); (* dfm-ocaml *)
	("serialize_yaml", ["bigarray"; "unicode"; "dfm"; "yaml"]); (* yaml-ocaml *)
	("sdl", ["bigarray"; "sdl"]); (* SDL *)
	("sdl_image", ["bigarray"; "sdl"; "sdl_image"]); (* SDL_image *)
	("sdl_mixer", ["bigarray"; "sdl"; "sdl_mixer"]); (* SDL_mixer *)
	("sdl_ttf", ["bigarray"; "sdl"; "sdl_ttf"]); (* SDL_ttf *)
	("str", ["str"]);
	("unicode", ["bigarray"; "unicode"]); (* unicode-ocaml *)
	("unix", ["unix"]);
	("yaml", ["bigarray"; "unicode"; "dfm"; "yaml"]); (* yaml-ocaml *)
	("zlib", ["zlib"]) (* zlib-ocaml *)
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
	depending_libraries: string list};;
	
let ocamldep (source_file: string): (string list * string list) = (
	let result_mls = ref [] in
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
	| _ -> prerr_string "error: "; prerr_string command; prerr_newline (); exit 1
	end;
	let command = options.ocamldep ^ " -modules " ^ source_file in
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
						let f = String.sub dep s (i - s) in
						f.[0] <- Char.lowercase f.[0];
						if List.mem_assoc f known_library then (
							result_lib := List.rev_append (List.assoc f known_library) !result_lib
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
	| _ -> prerr_string "error: "; prerr_string command; exit 1
	end;
	(!result_mls, List.rev (!result_lib))
);;

let execute (command: string): unit = (
	prerr_string command;
	prerr_newline ();
	if Sys.command command <> 0 then exit 1
);;

let build_info_filename = Filename.concat options.build_dir ".ocamlmake";;

let build_info: (string, source_info) Hashtbl.t = (
	begin try
		let f = open_in_bin build_info_filename in
		let r = Marshal.from_channel f in
		close_in f;
		r
	with
	| _ -> Hashtbl.create 13
	end
);;

if options.print_dependency then (
	Hashtbl.iter (fun s {depends = deps} ->
		print_string s;
		print_string ":";
		List.iter (fun d ->
			print_string " ";
			print_string d;
		) deps;
		print_newline ()
	) build_info;
	exit 0
);;

let save_build_info () = (
	let f = open_out_bin build_info_filename in
	Marshal.to_channel f build_info [];
	close_out f
);;
at_exit save_build_info;;

let mem_info (source_file: string): bool = (
	Hashtbl.mem build_info source_file
);;

let get_info source_file source_file_with_path kind remake = (
	if not remake && Hashtbl.mem build_info source_file then (
		(Hashtbl.find build_info source_file, false)
	) else (
		let (dm, dl) = ocamldep source_file_with_path in
		({kind = kind; switches = {options.compiler with outside = true}; depends = dm; depending_libraries = dl}, true)
	)
);;

let set_info source_file (info: source_info): unit = (
	Hashtbl.replace build_info source_file info
);;

let the_compiler = (
	begin match options.target with
	| Default | CMO | CMA | ByteExe | CMI | Run | Interact ->
		if options.compiler.profiling then options.ocamlcp ^ " -p a" else options.ocamlc
	| Optimized | CMX | CMXA | NativeExe | AsmSrc ->
		if options.compiler.profiling then options.ocamlopt ^ " -p" else options.ocamlopt
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
	if compiler.thread then Buffer.add_string buffer " -thread";
	if compiler.unsafe then Buffer.add_string buffer " -unsafe"
);;

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
						if options.build_dir <> "" then (
							Buffer.add_string command " -I ";
							Buffer.add_string command options.build_dir
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
						let command = Buffer.create 0 in
						Buffer.add_string command (if Sys.os_type = "Win32" then "move " else "mv ");
						Buffer.add_string command (
							let s = Filename.chop_extension compiled_filename ^ ".s" in
							if Sys.file_exists s then (
								s ^ " "
							) else (
								Filename.chop_extension compiled_filename ^ ".asm "
							)
						);
						Buffer.add_string command (if options.target_name = "" then "." else options.target_name);
						execute (Buffer.contents command)
					);
					latest := Unix.time ()
				);
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

let win_path_to_unix_path s = (
	begin match options.target with
	| Optimized | NativeExe ->
		let result = String.copy s in
		for i = 0 to String.length result - 1 do
			if result.[i] = '\\' then (
				result.[i] <- '/'
			)
		done;
		result
	| _ -> s
	end
);;

begin match options.target with
| CMI | CMO | CMX | AsmSrc -> ()
| Run | Interact ->
	let run_command = (
		let result = Buffer.create 0 in
		Buffer.add_string result options.ocaml;
		List.iter (fun i ->
			Buffer.add_string result " ";
			Buffer.add_string result i;
			Buffer.add_string result library_extension
		) options.libraries;
		if options.build_dir <> "" then (
			Buffer.add_string result " -I ";
			Buffer.add_string result options.build_dir
		);
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
					if Sys.os_type = "Win32" then r ^ ".exe" else r
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
			Buffer.add_string result " -o ";
			Buffer.add_string result target_name;
			List.iter (fun i ->
				Buffer.add_string result " -I ";
				Buffer.add_string result i
			) options.library_dirs;
			List.iter (fun i ->
				Buffer.add_string result " ";
				Buffer.add_string result i;
				Buffer.add_string result library_extension
			) options.libraries;
			Queue.iter (fun i ->
				Buffer.add_string result " ";
				Buffer.add_string result (win_path_to_unix_path i)
			) link_files;
			Buffer.add_string result options.largs;
			Buffer.contents result
		) in
		execute link_command
	)
end;;
