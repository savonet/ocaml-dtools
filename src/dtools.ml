
  (**************************************************************************)
  (*  ocaml-dtools                                                          *)
  (*  Copyright (C) 2003-2006  The Savonet Team                             *)
  (**************************************************************************)
  (*  This program is free software; you can redistribute it and/or modify  *)
  (*  it under the terms of the GNU General Public License as published by  *)
  (*  the Free Software Foundation; either version 2 of the License, or     *)
  (*  any later version.                                                    *)
  (**************************************************************************)
  (*  Contact: dev@gim.name                                                 *)
  (**************************************************************************)

(* $Id$ *)

(**
  ocaml-dtools
  @author Stephane Gimenez
*)

module Conf =
struct

  type link = string
  type path = link list

  and ut =
      <
	kind: string option;
	descr: string;
	comments: string list;
 	plug: string -> ut -> unit;
        subs: string list;
        path: string list -> ut;
        routes: ut -> path list;
	ut: ut;
     >

  type 'a t =
      <
	kind: string option;
	descr: string;
	comments: string list;
	plug: string -> ut -> unit;
        subs: string list;
        path: string list -> ut;
        routes: ut -> path list;
	ut: ut;
	set_d: 'a option -> unit;
	get_d: 'a option;
	set: 'a -> unit;
	get: 'a;
      >

  type links = (string * ut) list

  type 'a builder =
      ?d:'a ->
      ?p:(ut -> unit) ->
      ?l:links ->
      ?comments:string list ->
      string -> 'a t

  exception Undefined of ut
  exception Invalid of string
  exception Unbound of ut * string
  exception Bound of ut * string
  exception Mismatch of ut
  exception Wrong_Conf of string * string
  exception File_Wrong_Conf of string * int * string
  exception Cyclic of ut * ut

  let path_sep_regexp =
    Str.regexp "\\."
  let list_sep_regexp =
    Str.regexp ":"
  let line_regexp =
    Str.regexp
      "^[ \t]*\\([a-zA-Z]+\\)[ \t]+\\([a-zA-Z0-9._-]+\\)[ \t]*:\\(.*\\)$"
  let comment_regexp =
    Str.regexp "^[ ]*\\(#.*\\)?$"

  let check s =
    if Str.string_match path_sep_regexp s 0 then raise (Invalid (s))

  let make kind
      ?(d : 'a option)
      ?(p : ut -> unit = fun _ -> ())
      ?(l : links = [])
      ?(comments : string list = [])
      descr
      : 'a t =
  object (self)

    val kind : string option = kind
    val descr : string = descr
    val comments : string list = comments

    val mutable links : links = []

    val mutable value_d : 'a option = d
    val mutable value : 'a option = None

    initializer
      p self#ut;
      List.iter (fun (s, t) -> self#plug s t) l

    method subs =
      List.sort compare (List.map fst links)

    method private sub (s : string) : ut =
      check s;
      begin try
	  List.assoc s links
	with
	| Not_found -> raise (Unbound (self#ut, s))
      end

    method path (l : string list) : ut =
      begin match l with
      | [] -> self#ut
      | s :: q -> (self#sub s)#path q
      end

    method routes (st : ut) =
      (* todo: cache already accessed nodes *)
      let rec aux l t =
	begin match t = st with
	| true -> [List.rev l]
	| false ->
	    List.concat (List.map (fun s -> aux (s :: l) (t#path [s])) t#subs)
	end
      in
      aux [] self#ut

    method kind = kind

    method descr = descr
    method comments = comments

    method plug s t =
      if t#routes self#ut <> [] then raise (Cyclic (self#ut, t));
      if List.mem_assoc s links then raise (Bound (self#ut, s));
      links <- (s, t) :: links

    method ut = (self :> ut)

    method get_d : 'a option = value_d

    method set_d (v : 'a option) : unit = value_d <- v

    method get : 'a =
      begin match value with
      | None ->
	  begin match value_d with
	  | None -> raise (Undefined (self#ut))
	  | Some v -> v
	  end
      | Some v -> v
      end

    method set (v : 'a) : unit = value <- Some v

  end

  let void ?p ?l ?comments descr =
    (make None ?p ?l ~d:None ?comments descr)#ut

  let unit ?d = make (Some "unit") ?d
  let int ?d = make (Some "int") ?d
  let float ?d = make (Some "float") ?d
  let bool ?d = make (Some "bool") ?d
  let string ?d = make (Some "string") ?d
  let list ?d = make (Some "list") ?d

  (* Harmful function, do not use *)
  let force_type c (t : ut) : 'a t =
    begin match t#kind with
    | Some x when x = c -> (Obj.magic t : 'a t)
    | _ -> raise (Mismatch (t))
    end

  let as_unit t : unit t = force_type "unit" t
  let as_int t : int t = force_type "int" t
  let as_float t : float t = force_type "float" t
  let as_bool t : bool t = force_type "bool" t
  let as_string t : string t = force_type "string" t
  let as_list t : string list t = force_type "list" t

  let path_of_string p =
    Str.split path_sep_regexp p
  let string_of_path p =
    String.concat "." p

  let routes (t : ut) (st : ut) =
    let rec aux l t =
      begin match t = st with
      | true -> [List.rev l]
      | false ->
	  List.concat (List.map (fun s -> aux (s :: l) (t#path [s])) t#subs)
      end
    in
    aux [] t

  let get_string (t : ut) =
    begin try
       	begin match t#kind with
	| None -> None
	| Some "unit" -> Some ("")
	| Some "int" -> Some (string_of_int (as_int t)#get)
	| Some "float" -> Some (string_of_float (as_float t)#get)
	| Some "bool" -> Some (string_of_bool (as_bool t)#get)
	| Some "string" -> Some ((as_string t)#get)
	| Some "list" -> Some (String.concat ":" (as_list t)#get)
	| _ -> assert false
	end
      with
      | Undefined _ -> None
    end

  let get_d_string (t : ut) =
    let mapopt f = (function None -> None | Some x -> Some (f x)) in
    begin try
	begin match t#kind with
	| None -> None
	| Some "unit" -> mapopt (fun () -> "") (as_unit t)#get_d
	| Some "int" -> mapopt string_of_int (as_int t)#get_d
	| Some "float" -> mapopt string_of_float (as_float t)#get_d
	| Some "bool" -> mapopt string_of_bool (as_bool t)#get_d
	| Some "string" -> (as_string t)#get_d
	| Some "list" -> mapopt (String.concat ":") (as_list t)#get_d
	| _ -> assert false
	end
      with
      | Undefined _ -> None
    end

  let descr ?(prefix=[]) (t : ut) =
    let rec aux prefix t =
      let p s = if prefix = "" then s else prefix ^ "." ^ s in
      let subs =
	List.map (function s -> aux (p s) (t#path [s])) t#subs
      in
      Printf.sprintf "## %s\n" t#descr ^
	begin match get_d_string t with
	| None -> ""
	| Some d -> Printf.sprintf "# default :%s\n" d
	end ^
	begin match t#kind, get_string t with
	| Some k, None ->
	    Printf.sprintf "#%s\t%-30s\n" k prefix
	| Some k, Some p ->
	    Printf.sprintf "%s\t%-30s :%s\n" k prefix p
	| _ -> ""
	end ^
	begin match t#comments with
	| [] -> ""
	| l ->
	    "# comments:\n" ^
	    String.concat "" (List.map (fun s -> Printf.sprintf "#  %s\n" s) l)
	end ^
	"\n" ^ String.concat "" subs
    in
    aux (string_of_path prefix) (t#path prefix)

  let dump ?(prefix=[]) (t : ut) =
    let rec aux prefix t =
      let p s = if prefix = "" then s else prefix ^ "." ^ s in
      let subs =
	List.map (function s -> aux (p s) (t#path [s])) t#subs
      in
      begin match t#kind with
      | Some k ->
	  begin match get_d_string t, get_string t with
	  | None, None ->
	      Printf.sprintf "#%s\t%-30s\n" k prefix
	  | Some p, None ->
	      Printf.sprintf "#%s\t%-30s :%s\n" k prefix p
	  | Some p, Some p' when p' = p ->
	      Printf.sprintf "#%s\t%-30s :%s\n" k prefix p
	  | _, Some p ->
	      Printf.sprintf "%s\t%-30s :%s\n" k prefix p
	  end
      | _ -> ""
      end ^
	String.concat "" subs
    in
    aux (string_of_path prefix) (t#path prefix)

  let conf_set (t: ut) s =
    if Str.string_match line_regexp s 0
    then
      let val0 = Str.matched_group 1 s in
      let val1 = Str.matched_group 2 s in
      let val2 = Str.matched_group 3 s in
      let st = t#path (path_of_string val1) in
      begin match val0 with
      | "unit" ->
	  begin match val2 = "" with
	  | false -> raise (Wrong_Conf (s, "unit expected"))
	  | true -> (as_unit st)#set ()
	  end
      | "int" ->
	  let i =
	    begin try int_of_string val2  with
	    | Invalid_argument _ ->
		raise (Wrong_Conf (s, "integer expected"))
	    end
	  in
	  (as_int st)#set i
      | "float" ->
	  let f =
	    begin try float_of_string val2 with
	    | Invalid_argument _ ->
		raise (Wrong_Conf (s, "float expected"))
	    end
	  in
	  (as_float st)#set f
      | "bool" ->
	  let b =
	    begin try bool_of_string val2 with
	    | Invalid_argument _ ->
		raise (Wrong_Conf (s, "boolean expected"))
	    end
	  in
	  (as_bool st)#set b
      | "string" ->
	  let s = val2 in
	  (as_string st)#set s
      | "list" ->
	  let l = Str.split list_sep_regexp val2 in
	  (as_list st)#set l
      | _ -> raise (Wrong_Conf (s, "unknown type"))
      end
    else raise (Wrong_Conf (s, "syntax error"))

  let conf_file t s =
    let nb = Pervasives.ref 0 in
    let f = open_in s in
    begin try
	while true do
	  nb := !nb + 1;
	  let l = input_line f in
	  if Str.string_match comment_regexp l 0
	  then ()
	  else
	    begin try conf_set t l with
	    | Wrong_Conf (x,y) ->
		raise (File_Wrong_Conf (s,!nb,y))
	    end
	done
      with
      | End_of_file -> ()
    end

  let args t =
    [
      ["--conf-file";"-f"],
      Arg.String (conf_file t),
      "read the given configuration file";
      ["--conf-set";"-s"],
      Arg.String (conf_set t),
      "apply the given configuration assignation";
      ["--conf-descr-key"],
      Arg.String (fun p ->
	Printf.printf "%s" (descr ~prefix:(path_of_string p) t); exit 0),
      "describe a configuration key";
      ["--conf-descr"],
      Arg.Unit (fun () ->
	Printf.printf "%s" (descr t); exit 0),
      "display a described table of the configuration keys";
      ["--conf-dump"],
      Arg.Unit (fun () ->
	Printf.printf "%s" (dump t); exit 0),
      "dump the configuration state";
    ]

end

module Init =
struct

  let conf =
    Conf.void "initialization configuration"
  let conf_daemon =
    Conf.bool ~p:(conf#plug "daemon") ~d:false
      "run in daemon mode"
  let conf_daemon_pidfile =
    Conf.bool ~p:(conf_daemon#plug "pidfile") ~d:false
      "support for pidfile generation"
  let conf_daemon_pidfile_path =
    Conf.string ~p:(conf_daemon_pidfile#plug "path")
      "path to pidfile"
  let conf_trace =
    Conf.bool ~p:(conf#plug "trace") ~d:false
      "dump an initialization trace"
  let conf_concurrent =
    Conf.bool ~p:(conf#plug "concurrent") ~d:false
      "run initialization using concurrent threads"
  let conf_catch_exn =
    Conf.bool ~p:(conf#plug "catch_exn") ~d:true
      "catch exceptions, use false to backtrace exceptions"

  type t =
    {
      name: string;
      mutable launched: bool;
      mutable depends: t list;
      mutable triggers: t list;
      mutable mutex: Mutex.t;
      f: unit -> unit;
    }

  let make ?(name="") ?(depends=[]) ?(triggers=[]) ?(after=[]) ?(before=[]) f =
    let na =
      {
	name = name;
	launched = false;
	depends = depends;
	triggers = triggers;
	mutex = Mutex.create ();
	f = f;
      }
    in
    List.iter (fun a -> a.triggers <- na :: a.triggers) after;
    List.iter (fun a -> a.depends <- na :: a.depends) before;
    na

  let start = make ~name:"init-start" flush_all

  let stop = make ~name:"init-stop" flush_all

  let at_start ?name ?depends ?triggers ?after ?before f =
    let a = make ?name ?depends ?triggers ?after ?before f in
    start.triggers <- a :: start.triggers;
    a

  let at_stop ?name ?depends ?triggers ?after ?before f =
    let a = make ?name ?depends ?triggers ?after ?before f in
    stop.depends <- a :: stop.depends;
    a

  let rec exec a =
    let log =
      if conf_trace#get then
	begin fun s ->
	  let id = Thread.id (Thread.self ()) in
	  Printf.printf "init(%i):%-35s@%s\n%!" id a.name s
	end
      else
	begin fun s -> () end
    in
    let rec exec a =
      log "called";
      Mutex.lock a.mutex;
      try
        if not a.launched
        then begin
          a.launched <- true;
	  log "start";
	  log "start-depends";
	  mult_exec a.depends;
	  log "stop-depends";
	  log "start-atom";
	  a.f ();
	  log "stop-atom";
	  log "start-triggers";
	  mult_exec a.triggers;
	  log "stop-triggers";
	  log "stop";
	end;
        Mutex.unlock a.mutex;
        log "return"
      with e -> Mutex.unlock a.mutex; raise e
    and mult_exec l =
      begin match conf_concurrent#get with
      | true ->
	  let ask x =
	    log (Printf.sprintf "exec %s" x.name);
	    Thread.create exec x
	  in
	  let threads = List.map ask l in
	  List.iter Thread.join threads
      | false ->
	  List.iter exec l
      end
    in
    exec a

  let rec wait_signal () =
    begin try
      ignore (Thread.wait_signal [Sys.sigterm; Sys.sigint]);
    with
      | Unix.Unix_error (Unix.EINTR,_,_) -> ()
      | Sys_error("Thread.wait_signal: Interrupted system call") ->
          wait_signal ()
    end

  exception StartError of exn
  exception StopError of exn

  let main f () =
    begin try exec start with e -> raise (StartError e) end;
    let quit pid = Unix.kill pid Sys.sigterm in
    let thread pid =
      begin try f (); quit pid with
      | e ->
	  let se = Printexc.to_string e in
	  Printf.eprintf
	    "init: exception encountered during main phase:\n  %s\n%!" se;
	  Printf.eprintf "exception: %s\n%!" se;
	  if conf_catch_exn#get then quit pid else raise e
      end
    in
    ignore (Thread.create thread (Unix.getpid ()));
    wait_signal ();
    begin try exec stop with e -> raise (StopError e) end

  let catch f clean =
    begin try
	f (); clean ()
      with
      | StartError (e) ->
	  Printf.eprintf
	    "init: exception encountered during start phase:\n  %s\n%!"
	    (Printexc.to_string e);
	  clean ();
	  exit (-1)
      | StopError (e) ->
	  Printf.eprintf
	    "init: exception encountered during stop phase:\n  %s\n%!"
	    (Printexc.to_string e);
	  clean ();
	  exit (-1)
    end

  (** A function to reopen a file descriptor
    * Thanks to Xavier Leroy!
    * Ref: http://caml.inria.fr/pub/ml-archives/caml-list/2000/01/
    *      a7e3bbdfaab33603320d75dbdcd40c37.en.html
    *)
  let reopen_out outchan filename =
    flush outchan;
    let fd1 = Unix.descr_of_out_channel outchan in
    let fd2 =
      Unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666
    in
    Unix.dup2 fd2 fd1;
    Unix.close fd2

  (** The same for inchan *)
  let reopen_in inchan filename =
    let fd1 = Unix.descr_of_in_channel inchan in
    let fd2 =
      Unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666
    in
    Unix.dup2 fd2 fd1;
    Unix.close fd2

  let daemonize fn =
    begin match Unix.fork () with
    | 0 ->
        (* Change umask to 0 *)
        ignore(Unix.umask 0) ;
	let f =  
          match conf_daemon_pidfile#get with
	    | false ->
	       fun () -> catch fn (fun () -> ())
	    | true ->
	       (* Write PID to file *)
	       let filename = conf_daemon_pidfile_path#get in
	       let f = open_out filename in
	       let pid = Unix.getpid () in
	       output_string f (string_of_int pid);
	       output_char f '\n';
	       close_out f ;
               fun () -> catch fn (fun () -> Unix.unlink filename)
	in
        (* Dettach from the console *)
        if (Unix.setsid () < 0) then
               exit 1;
        (* chdir to / *)
        Unix.chdir "/" ;
        (* Reopen usual file descriptor *)
        flush_all ();
        reopen_in stdin "/dev/null";
        reopen_out stdout "/dev/null";
        reopen_out stderr "/dev/null";
        (* Main loop *)
        f () ;
        exit 0
    | _ -> exit 0
    end

  let exit_when_root () =
    let security s = Printf.eprintf "init: security exit, %s\n%!" s in
    if Unix.geteuid () = 0 then
      begin security "root euid (user)."; exit (-1) end;
    if Unix.getegid () = 0 then
      begin security "root egid (group)."; exit (-1) end

  let init ?(prohibit_root=false) f =
    if prohibit_root then exit_when_root ();
    (* We want to block those signals. (not blocked by default 
     * on freebsd for instance) *)
    ignore(Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigterm; Sys.sigint]);
    let signal_h i = () in
    Sys.set_signal Sys.sigterm (Sys.Signal_handle signal_h);
    Sys.set_signal Sys.sigint (Sys.Signal_handle signal_h);
    if conf_daemon#get
    then daemonize (main f)
    else catch (main f) (fun () -> ())

  let args =
    [
      ["--daemon";"-d"],
      Arg.Unit (fun () -> conf_daemon#set true),
      "run in daemon mode";
    ]

end

module Log =
struct

  type t =
      <
	active: int -> bool;
	f: 'a. int -> ('a, unit, string, unit) format4 -> 'a;
      >

  let log_ch = ref None

  (* Mutex to avoid interlacing logs *)
  let log_mutex = Mutex.create ()

  let conf =
    Conf.void "log configuration"

  let conf_level =
    Conf.int ~p:(conf#plug "level") ~d:3
      "general log level"
  let conf_unix_timestamps =
    Conf.bool ~p:(conf#plug "unix_timestamps") ~d:false
      "display unix timestamps (subsecond accuracy, timezone independant)"
  let conf_file  =
    Conf.bool ~p:(conf#plug "file") ~d:true
      "log to file"
  let conf_file_path =
    Conf.string ~p:(conf_file#plug "path")
      "path to log file"
  let conf_file_append =
    Conf.bool ~p:(conf_file#plug "append") ~d:true
      "append log to the file"
  let conf_file_perms =
    Conf.int ~p:(conf_file#plug "perms") ~d:0o600
      "log file permissions"
  let conf_stdout =
    Conf.bool ~p:(conf#plug "stdout") ~d:false
      "log to stdout"

  let state : [ `Direct | `Buffer of (float * string) list ] ref =
    ref (`Buffer [])

  let timestamp time =
    begin match conf_unix_timestamps#get with
    | true ->
	Printf.sprintf "%f" time
    | false ->
	let date = Unix.localtime time in
	Printf.sprintf "%d/%02d/%02d %02d:%02d:%02d"
          (date.Unix.tm_year+1900)
          (date.Unix.tm_mon+1)
          date.Unix.tm_mday
          date.Unix.tm_hour
          date.Unix.tm_min
          date.Unix.tm_sec
    end

  let mutexify f x =
    Mutex.lock log_mutex;
    begin try f x; Mutex.unlock log_mutex with
    | e -> Mutex.unlock log_mutex; raise e
    end

  let print (time, str) =
    let to_stdout = conf_stdout#get in
    let to_file = !log_ch <> None in
    begin match to_stdout || to_file with
    | true ->
        let timestamp = timestamp time in
	let do_stdout () =
	  Printf.printf "%s %s\n%!" timestamp str;
	in
	let do_file () =
	  begin match !log_ch with
	  | None -> ()
	  | Some ch -> Printf.fprintf ch "%s %s\n%!" timestamp str;
	  end
	in
	if to_stdout then do_stdout ();
	if to_file then do_file ();
    | false -> ()
    end

  let proceed entry =
    mutexify (fun () ->
      begin match !state with
      | `Buffer l -> state := `Buffer (entry :: l)
      | `Direct -> print entry
      end
    ) ()

  let build path =
    let rec aux p l (t : Conf.ut) =
      begin match p with
      | [] -> t :: l
      | s :: q ->
	  let st =
	    begin try t#path [s] with
	    | Conf.Unbound _ ->
		let c = Conf.int ~p:(t#plug s) "subordinate log level" in
		c#ut
	    end
	  in
	  aux q (t :: l) st
      end
    in
    aux path [] conf_level#ut

  let make path : t =
    let confs = build path in
    let path_str = Conf.string_of_path path in
    object (self : t)
      val label =
	(fun lvl -> "[" ^ path_str ^ ":" ^ (string_of_int lvl) ^ "]")
      method active lvl =
	let rec aux l =
	  begin match l with
	  | [] -> None
	  | t :: q ->
	      begin match aux q with
	      | Some i -> Some i
	      | None ->
		  begin try Some ((Conf.as_int t)#get)
		  with
		  | Conf.Undefined _ -> None
		  end
	      end
	  end
	in
	begin match aux confs with
	| None -> false
	| Some i -> i >= lvl
	end
      method f lvl =
	begin match self#active lvl with
	| true ->
	    let time = Unix.gettimeofday () in
	    Printf.ksprintf (fun s -> proceed (time, label lvl ^ " " ^ s))
	| false ->
	    Printf.ksprintf (fun s -> ())
	end
    end

  let init () =
    let time = Unix.gettimeofday () in
    if conf_file#get then
      begin
	let opts =
	  [Open_wronly; Open_creat; Open_nonblock]
	  @ (if conf_file_append#get then [Open_append] else [Open_trunc])
	in
	let log_file_path = conf_file_path#get in
	let log_file_perms = conf_file_perms#get in
	(* Re-open log file on SIGUSR1 -- for logrotate *)
	Sys.set_signal Sys.sigusr1
	  (Sys.Signal_handle
              begin fun _ ->
		begin match !log_ch with
		| None -> ()
		| Some ch -> log_ch := None; close_out ch;
		end;
		log_ch := Some (open_out_gen opts log_file_perms log_file_path)
	      end
	  );
	log_ch := Some (open_out_gen opts log_file_perms log_file_path);
      end;
    mutexify (fun () ->
      print (time, ">>> LOG START");
      begin match !state with
      | `Buffer l -> List.iter (fun entry -> print entry) (List.rev l)
      | `Direct -> ()
      end;
      state := `Direct
    ) ()

  let start = Init.make ~name:"init-log-start" ~before:[Init.start] init

  let close () =
    let time = Unix.gettimeofday () in
    mutexify (fun () ->
      print (time, ">>> LOG END");
      state := `Buffer [];
      begin match !log_ch with
      | None -> ()
      | Some ch -> log_ch := None; close_out ch;
      end
    ) ()

  let stop = Init.make ~name:"init-log-stop" ~after:[Init.stop] close

  let args =
    [
      ["--log-stdout"],
      Arg.Unit (fun () -> conf_stdout#set true),
      "log also to stdout";
      ["--log-file";"-l"],
      Arg.String (fun s -> conf_file#set true; conf_file_path#set s),
      "log file";
    ]

end
