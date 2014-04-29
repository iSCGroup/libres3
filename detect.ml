open Printf;;
printf "OCaml compiler version ... %s\n%!" Sys.ocaml_version;;

#load "str.cma"
#load "unix.cma"
(* Version number handling based on odb.ml which is under the WTFPL license*)
module Ver = struct
  (* A version number is a list of (string or number) *)
  type ver_comp = Num of int | Str of string
  type ver = ver_comp list

  let rec cmp : ver -> ver -> int = fun a b ->
    match a,b with
    | [],[] -> 0 (* each component was equal *)
    (* ignore trailing .0's *)
    | Str"."::Num 0::t, [] -> cmp t [] | [], Str"."::Num 0::t -> cmp [] t
    (* longer version numbers are before shorter ones *)
    | _::_,[] -> 1 | [], _::_ -> -1
    (* compare tails when heads are equal *)
    | (x::xt), (y::yt) when x=y -> cmp xt yt
    (* just compare numbers *)
    | (Num x::_), (Num y::_) -> compare (x:int) y
    (* extend with name ordering? *)
    | (Str x::_), (Str y::_) -> compare (x:string) y
    | (Num x::_), (Str y::_) -> -1 (* a number is always before a string *)
    | (Str x::_), (Num y::_) -> 1 (* a string is always after a number *)
  let to_ver = function
    | Str.Delim s -> Str s
    | Str.Text s -> Num (int_of_string s)
  let parse_ver v =
    try
      List.map to_ver (Str.full_split (Str.regexp "[^0-9]+") v)
    with Failure _ -> eprintf "Could not parse version: %s" v; []
  let comp_to_string = function Str s -> s | Num n -> string_of_int n
  let to_string v = String.concat "" (List.map comp_to_string v)

  let (<?) a b = (cmp a (parse_ver b)) < 0
  let (>?) a b = (cmp a (parse_ver b)) > 0
  let (<=?) a b = (cmp a (parse_ver b)) <= 0
  let (>=?) a b = (cmp a (parse_ver b)) >= 0
  let (==?) a b = (cmp a (parse_ver b)) == 0
  let ver_any _ = true
  let ver_none _ = false
end;;

open Ver;;
if (parse_ver Sys.ocaml_version) <? "3.12.1" then begin
  eprintf "OCaml version 3.12.1 or later is required";
  exit 3;
end;;

(* Above this must be compatible with OCaml 3.09 *)

type clib = {
  header: string;
  lib: string;
  fn: string;
}

type tool = {
  tool: string;
}

type ocaml = {
  findlibnames: string list;
  version: ver -> bool;
}

type pkg_spec = CLib of clib | Tool of tool | OCaml of ocaml

type build = {
  source : string;
  findlibnames: string list;
  configure: string list;
  make: string list;
  install: string list;
  uninstall: string list;
}

type install = {
  os_pkg_names: string list;
}

module Builds = Set.Make(struct
  type t = string * build * string list
  let compare = Pervasives.compare
end);;

module Installs = Set.Make(struct
  type t = install
  let compare = Pervasives.compare
end);;

let prefix = Filename.concat (Sys.getcwd ()) "_build";;
let prefix_bin = Filename.concat prefix "bin";;
let prefix_data = Filename.concat prefix "share";;
let prefix_var = Filename.concat prefix "var";;
let prefix_etc = Filename.concat prefix "etc";;
let destdir = Filename.concat prefix "lib/ocaml/site-lib";;
let ocamlpath = destdir;;
let ld_library_path = Filename.concat destdir "stublibs";;
(* libres3 features *)
let want_nethttpd = false;;
let want_ocsigen = (parse_ver Sys.ocaml_version) >=? "3.12.1";;

printf "Cleaning build directory\n";;
Sys.command "ocamlbuild -clean -quiet";;
Sys.command "cd libres3 && ocaml setup.ml -distclean -quiet";;

let append l1 l2 =
  List.rev_append (List.rev l1) l2;;

let get_env_opt env =
  try
    Sys.getenv env
    with Not_found ->
      ""
;;

module Packages : sig
  type have = private No | MissingDev | Yes
  type dep
  type generate_build = dep list -> build
  type action =
  | Build of generate_build
  | Install of install
  type opt_flag = {
    dep: dep;
    enabled: string;
    disabled: string;
  }

  val check_clib : name:string -> header:string -> lib:string list -> fn: string -> have
  val check_tool : string -> commands:string list -> string -> (string -> bool) -> have * string
  val check_findlib_package : (ver -> bool) -> string -> have
  val check_ocaml :
    name:string -> findlibnames:string list -> cmi:string*string -> version:(ver -> bool) -> have

  val dependency :
    string -> ?deps_opt:dep list -> ?deps:dep list ->
    have -> action -> dep

  (* (dep,flag) => if dep is required then add flag to configure flags *)
  val flags : opt_flag list -> string list -> string list

  val rules: dep list -> Builds.t * Installs.t
  val print_summary : Builds.t -> Installs.t -> unit
  val generate : string -> Builds.t -> unit
end = struct
  type have = No | MissingDev | Yes

  let run_command cmd =
    Printf.eprintf "+ %s\n%!" cmd;
    let rc = Sys.command cmd in
    if rc <> 0 then
      eprintf "Command '%s' exited with code '%d'\n%!" cmd rc;
    rc = 0;;

  let checking what check default =
    printf "Checking for %s ... %!" what;
    try
      let result = check () in
      begin match result with
      | Yes -> printf "OK\n%!"
      | MissingDev -> printf "No (-dev package missing\n)%!"
      | No -> printf "No\n%!"
      end;
      result
    with Not_found -> default
  ;;

  let check_version verfn version =
    printf "found %s ... %!" version;
    if verfn (Ver.parse_ver version) then Yes else No
  ;;

  let try_finally fn finally value =
    let result =
      begin try
        fn value
      with e ->
        begin try finally value with _ -> () end;
        raise e
      end in
      finally value;
      result
  ;;

  let remove name =
    try
      Sys.remove name
    with Sys_error _ -> ();;

  let with_tmpfile prefix suffix write_fn use_fn =
    let name, ch = Filename.open_temp_file prefix suffix in
    try_finally (fun name ->
      try_finally write_fn close_out ch;
      use_fn name;
    ) remove name
  ;;

  let noop _ = ();;

  let rec consume p =
    try
      ignore (input_line p);
      consume p
    with End_of_file -> ()
  ;;

  let get_command_output cmd =
    let pipe_out, pipe_in, pipe_err = Unix.open_process_full cmd (Unix.environment ()) in
    let result =
      try input_line pipe_out
      with End_of_file -> "" in
    consume pipe_out;
    consume pipe_err;
    if (Unix.close_process_full (pipe_out, pipe_in, pipe_err)) = (Unix.WEXITED 0) then
      result
    else
      ""
  ;;

  let trim_start_re = Str.regexp "^ *"
  let trim_end_re = Str.regexp " *$"

  let trim str =
    Str.replace_first trim_start_re "" (
      Str.replace_first trim_end_re "" str)
  ;;

  let append_env env value =
    let v = trim (value) in
    let old = trim (get_env_opt env) in
    if v <> "" then
    Unix.putenv env (
      if old = "" || old = v then v
      else (old ^ ":" ^ v)
    )
  ;;

  let pkgconfig name =
    let cppflags = get_command_output (sprintf "pkg-config %s --cflags-only-I" name)
    and ldflags = get_command_output (sprintf "pkg-config %s --libs-only-L" name)
    in
    append_env "CPPFLAGS" cppflags;
    append_env "LDFLAGS" ldflags;
    cppflags, ldflags
  ;;

  let check_clib ~name ~header ~lib ~fn =
    checking (sprintf "C library '%s', lib%s, header '%s'" name
      (String.concat "," lib) header) (fun () ->
      let cppflags, ldflags = pkgconfig name in
      printf "\n\tCPPFLAGS=%s LDFLAGS=%s\n%!" cppflags ldflags;
      let test_name = sprintf "test_%s" name in
      with_tmpfile test_name ".c" (fun tmp_out ->
        fprintf tmp_out "#include <%s>\nint main(void)\n{ %s; return 0; }\n"
          header fn
      ) (fun c_name ->
        with_tmpfile test_name ".cma" noop (fun cma_name ->
          let o_name = Filename.basename (Filename.chop_suffix c_name ".c") ^ ".o" in
          let libs = String.concat " " (List.map (fun s -> "-l" ^ s) lib) in
          let result = with_tmpfile test_name ".run" noop (fun run_name ->
            run_command (
              sprintf "ocamlc -verbose -a -o %s -ccopt \"%s\" %s -cclib \"%s %s\""
                cma_name cppflags c_name ldflags libs
            ) &&
            run_command (
              sprintf "ocamlc -verbose -make-runtime -o %s %s" run_name cma_name
            )
          ) in
          remove o_name;
          if result then Yes else No
        )
      )
    ) No
  ;;

  let check_cmd verflag verfn cmd =
    if cmd = "" then No
    else checking cmd (fun () ->
      match get_command_output (cmd ^ " " ^ verflag) with
      | "" -> No
      | out ->
        printf "found %s ... %!" out;
        if verfn out then Yes else No
    ) No;;

  let check_tool name ~commands verflag verfn =
    printf "Checking for %s ...\n%!" name;
    match List.filter (fun c -> (check_cmd verflag verfn c) = Yes) commands with
    | [] ->
        No, ""
    | first :: _ ->
        Yes, first
  ;;

  (* Can't use 'topfind' because Centos5 has incompatible ocaml and ocaml-findlib
   * versions (3.09 and 3.11) when using EPEL5+Dag *)

  let query_findlib fmt pkg =
    (* TODO: check for .cmi! if not only runtime pkg is installed! *)
    let cmd = "ocamlfind query -format " ^ fmt ^ " " ^ pkg in
    let pipe = Unix.open_process_in cmd in
    let result =
      try input_line pipe
      with End_of_file -> "" in
    match Unix.close_process_in pipe with
    | Unix.WEXITED 0 -> result
    | _ ->
        raise Not_found
    ;;

  let get_version pkg =
    query_findlib "%v" pkg;;

  let check_findlib_package ver_fn pkg =
    checking (sprintf "findlib package '%s'" pkg) (fun () ->
      check_version ver_fn (get_version pkg)
    ) No
  ;;

  let check_findlib_cmi (pkg,cmi) =
    checking cmi (fun () ->
      let cmi = Filename.concat (query_findlib "%d" pkg) cmi in
      if Sys.file_exists cmi then Yes else No
    ) No
  ;;

  let id x = x = Yes;;

  let check_ocaml ~name ~findlibnames ~cmi ~version =
    checking (sprintf "ocaml library '%s'" name) (fun () ->
      printf "\n";
      let have = List.rev_map (fun pkg ->
        printf "\t";
        (check_findlib_package version pkg),
        (check_findlib_cmi cmi)
      ) findlibnames in
      let have_pkgs, have_cmis = List.split have in
      match (List.for_all id have_pkgs), (List.for_all id have_cmis) with
      | true, true -> Yes
      | true, false -> MissingDev
      | false, _ -> No
    ) No
  ;;

  type dep = {
    name: string;
    dep_findlibnames: string list;
    have: bool;
    deps: dep list;
    deps_opt : dep list;
    mutable required: bool;
    action: action option;
  }

  and generate_build = dep list -> build
  and action =
  | Build of generate_build
  | Install of install

  type opt_flag = {
    dep: dep;
    enabled: string;
    disabled: string;
  }

  let dependency name ?(deps_opt = []) ?(deps = []) have action =
    let findlibnames, action_opt = match action with
    | Build b ->
        (* if the package is installed and none of its deps
         * needs to be built then we don't have to build it.
         * otherwise we do *)
        if have = Yes && List.for_all (fun d -> d.action = None) deps then
          [], None
        else
          (b []).findlibnames, Some action
    | Install _ ->
        [], if have = Yes then None else Some action in
    {
      name = name;
      dep_findlibnames = findlibnames;
      have = (have = Yes);
      deps = deps;
      deps_opt = deps_opt;
      required = false;
      action = action_opt
    }
  ;;

  let rec require dep =
    if not dep.required then begin
      dep.required <- true;
      if dep.action <> None then
        List.iter require dep.deps
    end
  ;;

  let is_required d = d.required;;

  let get_deps deps =
    List.fold_left (fun accum d ->
      match d.action with
      | Some (Build _) ->
        List.rev_append d.dep_findlibnames accum
      | _ ->
        accum
    ) [] deps;;

  let rec compute_rules deps =
    List.fold_left (fun (builds,installs) d ->
      match d.action with
      | None -> builds, installs
      | Some (Build get_build) ->
          let deps = List.rev_append
            (List.filter is_required d.deps_opt)
            d.deps in
          let dep_builds, dep_installs = compute_rules deps in
          Builds.union builds (Builds.add (d.name, get_build deps, get_deps deps) dep_builds),
          Installs.union installs dep_installs
      | Some (Install i) ->
          builds, Installs.add i installs
    ) (Builds.empty,Installs.empty) deps
  ;;

  let rules deps =
    List.iter require deps;
    compute_rules deps;;

  let print_install i =
    printf "\t%s\n" (String.concat " or " i.os_pkg_names);;

  let print_build (name,_,_) =
    printf " %s" name;;

  let print_summary builds installs =
    printf "\n>>> Configure summary:\n";
    if not (Installs.is_empty installs) then begin
      printf "\n*** Please install the following packages:\n";
      Installs.iter print_install installs;
      exit 1
    end;
    if not (Builds.is_empty builds) then begin
      printf "The following packages will be built:";
      Builds.iter print_build builds;
      printf "\n"
    end
  ;;

  let flags opt_flags flags =
    append flags (List.rev_map (fun flag ->
        if is_required flag.dep then
          flag.enabled
        else
          flag.disabled
      ) opt_flags
    )
  ;;

  let string_of_list lst =
    let buf = Buffer.create 128 in
    Buffer.add_char buf '[';
    List.iter (fun e ->
      Buffer.add_char buf '"';
      Buffer.add_string buf e;
      Buffer.add_char buf '"';
      Buffer.add_char buf ';';
    ) lst;
    Buffer.add_char buf ']';
    Buffer.contents buf;;

  let print_cmd_cpr f dir =
    fprintf f "\tlet cmd_cpr = Cmd (S [";
    fprintf f "A \"cp\";A \"-R\";A \"-P\";P \"../%s\";A \"%s\"]) in\n"
      dir (Filename.basename dir);;

  let print_cmd ?(opt=false) f name lst =
    fprintf f "\tlet cmd_%s = " name;
    if lst = [] then
      fprintf f "Nop"
    else begin
      fprintf f "Cmd (S [cmd_env; Sh \"&&\";\n\t\t";
      List.iter (fun e ->
        fprintf f "A \"%s\";" e
      ) lst;
      if opt then
        fprintf f "Sh \"||\";A \"true\"";
      fprintf f "])";
    end;
    fprintf f " in\n";;

  let print_cmd_env f dir =
    fprintf f "\tlet cmd_env = S [A \"cd\"; Px \"%s\"] in\n" dir
    ;;

  let name_re = Str.regexp "[-.]"

  let gname s =
    Str.global_replace name_re "_" s;;

  let generate_build f (name,build,_) =
    fprintf f "let build_%s _ _ =\n" (gname name);
    let dir = build.source in
    print_cmd_env f (Filename.basename dir);
    print_cmd_cpr f dir;
    print_cmd f "configure" build.configure;
    print_cmd f "build" build.make;
    print_cmd f "install" build.install;
    print_cmd ~opt:true f "uninstall" build.uninstall;
    fprintf f "\tSeq [cmd_cpr; cmd_configure; cmd_build; cmd_uninstall; cmd_install];;\n\n"
  ;;

  (* TODO: dependencies should be on META.
   * also handle uninstall; and install failures! *)
  let depname name =
    Filename.concat (Filename.concat "lib/ocaml/site-lib" name) "META";;

  let generate_rule f name prods deps =
    let sdeps = List.fast_sort String.compare deps in
    let udeps = List.fold_left (fun accum d ->
      match accum with
      | hd :: _ when hd = d ->
          accum
      | _ -> d :: accum) [] sdeps in
    let rdeps = if udeps = prods then [] else udeps in
    let name = gname name in
    fprintf f "\trule \"build %s\" ~prods:%s ~deps:%s build_%s;\n"
      name (string_of_list prods) (string_of_list rdeps) name;;

  let generate_rule_meta f (name,b,deps) =
    generate_rule f name
      (List.rev_map depname b.findlibnames)
      (List.rev_map depname deps)
  ;;

  let generate_main f main builds =
    let args = List.tl (Array.to_list Sys.argv) in
    let dir = Filename.concat ".." main in

    fprintf f "let build_all _ _ =\n";
    print_cmd_env f dir;
    print_cmd f "configure"
      (append
        ["./configure";"--override";"ocamlbuildflags";"-j 0";
          if want_ocsigen then "--enable-ocsigen" else "--disable-ocsigen";
          "--disable-docs";
          "--enable-tests"
        ]
        args
      );
    print_cmd f "build" ["ocaml";"setup.ml";"-build"];
    fprintf f "\tSeq [cmd_configure; cmd_build ];;\n";

    List.iter (fun target ->
      fprintf f "let build_%s _ _ =\n" target;
      print_cmd_env f dir;
      print_cmd f target ["ocaml";"setup.ml";"-" ^ target];
      fprintf f "\tSeq [cmd_%s];;\n" target
    ) ["test";"install";"reinstall";"uninstall";"clean"]
  ;;

  let write_env name =
    let file = "config." ^ (String.lowercase name) in
    let f = open_out_gen [Open_trunc;Open_creat;Open_wronly] 0o600 file in
    begin try
      output_string f (Unix.getenv name);
    with
      Not_found-> ()
    end;
    close_out f
  ;;

  let generate main builds =
    write_env "CPPFLAGS";
    write_env "LDFLAGS";
    let f = open_out_gen [Open_trunc;Open_creat;Open_wronly] 0o600 "myocamlbuild.ml" in
    fprintf f "open Ocamlbuild_plugin\n\n";
    Builds.iter (generate_build f) builds;
    fprintf f "\n";
    generate_main f main builds;
    fprintf f "\ndispatch begin function\n";
    fprintf f "| After_rules ->\n";
    Builds.iter (generate_rule_meta f) builds;
    fprintf f "\n";
    let deps = Builds.fold (fun (_,b,_) accum ->
      List.rev_append (List.rev_map depname b.findlibnames) accum) builds [] in
    generate_rule f "all" ["all.target"] deps;
    generate_rule f "install" ["install.target"] ["all.target"];
    generate_rule f "uninstall" ["uninstall.target"] [];
    generate_rule f "test" ["test.target"] ["all.target"];
    generate_rule f "clean" ["clean.target"] [];
    fprintf f "| _ -> ()\n";
    fprintf f "end;;\n";
    close_out f;;

end;;
open Packages;;

module CommonRules : sig
  val build_oasis : string -> findlibnames:string list -> flags:string list -> build
  val build_pkgopkg : string -> findlibname:string -> flags:string list -> build
  val build_confgnumake : ?flags: string list -> findlibnames:string list -> string -> build

  val pkg_findlib: dep
  val ocaml_dependency :
    string -> ?cmi:string*string -> ?deps_opt:dep list -> ?deps:dep list ->
      ?findlibnames:string list -> ?version:(ver->bool) -> action -> dep
  val clib_dependency :
    string -> header:string -> lib:string list -> fn:string -> install -> dep
  val gnu_make: string
  val gnu_make_dep: dep
end = struct
  (* GNU Make *)
  let gnumake_re = Str.regexp_string "GNU Make"

  let have_gnu_make, gnu_make = check_tool "GNU make" ~commands:[
    (get_env_opt "MAKE"); "gmake";"make"
  ] "--version" (fun ver ->
    Str.string_match gnumake_re ver 0
  );;

  let build_confgnumake ?(flags=[]) ~findlibnames source = {
    source = source;
    findlibnames = findlibnames;
    configure = append ["sh";"configure";"--prefix";prefix] flags;
    make = [gnu_make];
    install = [gnu_make; "install"];
    uninstall = [gnu_make; "uninstall"]
  };;

  let install_gnumake = Install { os_pkg_names = ["make";"gmake"] };;
  let gnu_make_dep = dependency "GNU Make" have_gnu_make install_gnumake;;

  let build_oasis source ~findlibnames ~flags = {
    source = source;
    findlibnames = findlibnames;
    configure =
      append
        ["ocaml";"setup.ml";"-configure";"--disable-docs";"--prefix";prefix;
        "--override";"native_dynlink";"false";
        "--override";"ocamlbuildflags";"-j 0"]
        flags;
    make = ["ocaml";"setup.ml";"-build"];
    install = ["ocaml";"setup.ml";"-install"];
    uninstall = ["ocaml";"setup.ml";"-uninstall"];
  }
  ;;

  let build_pkgopkg source ~findlibname ~flags = {
    source = source;
    findlibnames = [findlibname];
    configure = ["./pkg/pkg-git"];
    make = ["./pkg/build";"true"];
    install = ["ocamlfind";"install";findlibname;"pkg/META";
      "_build/src/" ^ findlibname ^ ".a";
      "_build/src/" ^ findlibname ^ ".cmi";
      "_build/src/" ^ findlibname ^ ".cma";
      "_build/src/" ^ findlibname ^ ".cmx";
      "_build/src/" ^ findlibname ^ ".cmxa"
    ];
    uninstall = ["ocamlfind";"remove";findlibname]
  }
  ;;

  let build_ocamlfind = Build (fun _ ->
    {
      source = "3rdparty/libs/findlib";
      findlibnames = ["findlib"];
      configure = ["sh";"configure";
        "-bindir";prefix_bin;
        "-mandir";Filename.concat prefix_data "man";
        "-sitelib";destdir;
        "-config";Filename.concat prefix_etc "findlib.conf"; 
        "-no-topfind";
      ];
      make = [gnu_make;"all";"opt"];
      install = [gnu_make;"install"];
      uninstall = [gnu_make;"uninstall"];
    }
  );;

  let dep_ocamlfind =
    (* oasis uses ocamlfind ocamldep -modules,
    which is not supported on versions earlier than 1.2.0
     some 3rdparty libs also use 'ocamlfind ocamlmklib' which was not available
     prior to 1.2.8
    *)
    let have = check_findlib_package (fun v -> v >=? "1.2.8") "findlib" in
    dependency "ocamlfind" have build_ocamlfind ~deps:[gnu_make_dep]
  ;;

  let any _ = true

  let ocaml_dependency
    name ?(cmi=(name,name^".cmi")) ?deps_opt ?(deps=[]) ?(findlibnames = [name]) ?(version=any) action =
      let have = check_ocaml ~name ~findlibnames ~cmi ~version in
      (* if user has installed the correct runtime package,
       * but not the dev package tell him to install the dev package,
       * instead of building the package ourselves *)
      let a =
        match have with
        | MissingDev ->
          Install { os_pkg_names = ["lib"^name^"-ocaml-dev"] }
        | Yes | No ->
          action in
      dependency name ?deps_opt ~deps:(dep_ocamlfind :: deps) have a
    ;;

  let pkg_findlib =
    ocaml_dependency "findlib" build_ocamlfind;;

  let clib_dependency name ~header ~lib ~fn install =
    dependency name (check_clib ~name ~header ~lib ~fn) (Install install);;


end;;
open CommonRules

(* ocamlbuild *)
let have_ocamlbuild, _ = check_tool "ocamlbuild" ~commands:["ocamlbuild"]
  "-version" (fun _ -> true)
;;
let install_ocamlbuild = Install { os_pkg_names = ["ocaml"] };;
let dep_ocamlbuild = dependency "ocamlbuild" have_ocamlbuild install_ocamlbuild;;

(* Camlp4 *)
let install_camlp4of = Install { os_pkg_names = ["camlp4-extra";"ocaml-camlp4-devel"] };;
let have_camlp4of, _ = check_tool "camlp4of" ~commands:["camlp4of.opt";"camlp4of"]
  "-version" (fun ver -> ver = Sys.ocaml_version);;
let camlp4of_dep = dependency "camlp4of" have_camlp4of install_camlp4of

let install_camlp4rf = Install { os_pkg_names = ["camlp4-extra";"ocaml-camlp4-devel"] };;
let have_camlp4rf, _ = check_tool "camlp4rf" ~commands:["camlp4rf.opt";"camlp4rf"]
  "-version" (fun ver -> ver = Sys.ocaml_version);;
let camlp4rf_dep = dependency "camlp4rf" have_camlp4rf install_camlp4rf

let install_camlp4 = Install { os_pkg_names = ["camlp4";"ocaml-camlp4-devel"] };;
let have_camlp4, _ = check_tool "camlp4" ~commands:["camlp4.opt";"camlp4"]
  "-version" (fun ver -> ver = Sys.ocaml_version);;
let camlp4_dep = dependency "camlp4" have_camlp4 install_camlp4

(* Pcre *)

let install_pcre = { os_pkg_names = ["libpcre3-dev"] };;

let pkg_pcre = ocaml_dependency "pcre" (Build (fun _ ->
  build_oasis "3rdparty/libs/pcre-ocaml" ~findlibnames:["pcre"] ~flags:[]))
  ~deps:[
    clib_dependency "libpcre" ~header:"pcre.h" ~lib:["pcre"] ~fn:"pcre_version()"
      install_pcre;
  ] ~version:(fun ver -> ver >=? "6.0.1");;

(* OpenSSL *)
let install_ssl =  { os_pkg_names = ["libssl-dev"] };;
let pkg_ssl =
  ocaml_dependency "ssl"
    (Build (fun _ ->
      build_confgnumake "3rdparty/libs/ocaml-ssl" ~findlibnames:["ssl"]
    ))
    ~deps:[clib_dependency "libssl"
      ~header:"openssl/ssl.h" ~lib:["ssl";"crypto"] ~fn:"(void)SSL_library_init ()" install_ssl;
      gnu_make_dep
    ]
    ~version:(fun v -> v >=? "0.4.4")
;;

(* OCamlnet *)

let pkg_ocamlnet =
  ocaml_dependency "ocamlnet" ~findlibnames:[
    "netstring";"netstring-pcre";"netsys";"netcgi2";"netclient";"equeue.ssl"
  ] ~version:(fun ver -> ver >? "3.7.3") ~cmi:("netstring","netstring_str.cmi")
  (Build (fun _ -> {
    source = "3rdparty/libs/ocamlnet";
    findlibnames = ["netstring";"netstring-pcre";"netsys";"netcgi2";"netclient"];
    configure =
      flags [
        {dep = pkg_pcre; enabled = "-enable-pcre"; disabled = "-disable-pcre"};
        {dep = pkg_ssl; enabled = "-enable-ssl"; disabled = "-disable-ssl"};
      ] ["sh"; "configure";"-bindir";prefix_bin;"-datadir";prefix_data;
        "-disable-gtk";"-disable-gtk2";"-disable-tcl";"-disable-zip";
        "-disable-crypto";"-disable-apache";
        if want_nethttpd then "-with-nethttpd" else "-without-nethttpd";
        "-without-rpc-auth-dh"
      ];
    (* apparently this requires GNU make too, or at least it doesn't build with
     * pmake *)
    make = [gnu_make;"all";"opt"];
    install = [gnu_make;"install"];
    uninstall = [gnu_make;"uninstall"];
  })) ~deps_opt:[gnu_make_dep; pkg_pcre; pkg_ssl];;

(* xmlm *)
let pkg_xmlm = ocaml_dependency "xmlm" (Build (fun _ ->
  build_pkgopkg "3rdparty/libs/xmlm" ~findlibname:"xmlm" ~flags:[])) ~deps:[dep_ocamlbuild];;

(* uutf *)
let pkg_uutf = ocaml_dependency "uutf" (Build (fun _ ->
  build_pkgopkg "3rdparty/libs/uutf" ~findlibname:"uutf" ~flags:[])) ~deps:[dep_ocamlbuild];;

(* jsonm *)
let pkg_jsonm = ocaml_dependency "jsonm" (Build (fun _ ->
  build_oasis "3rdparty/libs/jsonm" ~findlibnames:["jsonm"] ~flags:[]))
  ~deps:[dep_ocamlbuild; pkg_uutf];;

(* cryptokit *)
let pkg_cryptokit = ocaml_dependency "cryptokit" (Build (fun _ ->
  build_oasis "3rdparty/libs/cryptokit" ~flags:[] ~findlibnames:["cryptokit"]
)) ~deps:[dep_ocamlbuild] ~version:(fun ver -> ver >=? "1.3");;

(* oUnit *)
let pkg_ounit = ocaml_dependency "oUnit" (Build (fun _ ->
  build_oasis "3rdparty/libs/ounit" ~flags:[] ~findlibnames:["oUnit"]
)) ~deps:[dep_ocamlbuild; camlp4_dep];;

(* React *)
let pkg_react = ocaml_dependency "react" (Build (fun _ ->
  build_oasis "3rdparty/libs/react" ~flags:[] ~findlibnames:["react"]
)) ~deps:[dep_ocamlbuild] ~version:(fun v -> v >=? "0.9.2");;

(* Lwt *)
let pkg_lwt = ocaml_dependency "lwt" ~findlibnames:["lwt";"lwt.unix";"lwt.ssl"]
  (Build (fun _ ->
    let flags = flags [{
      dep = pkg_ssl;
      enabled = "--enable-ssl";
      disabled = "--disable-ssl";
    }] ["--disable-libev";"--enable-react"] in
    build_oasis "3rdparty/libs/lwt" ~flags ~findlibnames:["lwt"]
  ))
  ~version:(fun v -> (v >=? "2.4.2"))
  ~deps:[pkg_react; camlp4_dep; dep_ocamlbuild]
  ~deps_opt:[pkg_ssl]
;;

(* Tyxml *)
let pkg_tyxml = ocaml_dependency "tyxml" ~cmi:("tyxml","html5.cmi") (Build (fun _ ->
  build_oasis "3rdparty/libs/tyxml" ~findlibnames:["tyxml"] ~flags:[]))
    ~version:(fun v -> v >=? "2.0.1")
  ~deps:[pkg_ocamlnet; camlp4of_dep; camlp4rf_dep; camlp4_dep]
;;

(* Ocsigenserver *)
let pkg_ocsigenserver = ocaml_dependency "ocsigenserver"
  ~cmi:("ocsigenserver","ocsigen_server.cmi") (Build (fun _ ->
  build_confgnumake "3rdparty/libs/ocsigenserver" ~flags:[
    "--without-dbm";"--enable-debug";
    "--disable-natdynlink";
    "--commandpipe";Filename.concat prefix_var "lib/ocsigenserver/commandpipe";
    "--ocsigen-user";string_of_int (Unix.getuid ());
    "--ocsigen-group";string_of_int (Unix.getgid ());
    "--sysconfdir";Filename.concat prefix_etc "ocsigenserver";
    "--logdir";Filename.concat prefix_var "log/ocsigenserver";
    "--staticpagesdir";Filename.concat prefix_var "www/ocsigenserver";
    "--datadir";Filename.concat prefix_var "lib/ocsigenserver"
  ] ~findlibnames:["ocsigenserver"]
)) ~version:(fun _ -> false) (* always build *)
  ~deps:[
  gnu_make_dep; camlp4_dep; pkg_findlib; pkg_pcre; pkg_ocamlnet; pkg_react; pkg_ssl; pkg_lwt; pkg_cryptokit; pkg_tyxml
]

(* Main dependencies of libres3 *)
let deps_default = [ pkg_ocamlnet; pkg_jsonm; pkg_xmlm; pkg_cryptokit; pkg_ounit; pkg_ssl ];;
let deps = if want_ocsigen then
  pkg_ocsigenserver :: deps_default
else
  deps_default;;

let builds, installs = rules deps;;
print_summary builds installs;;
if not want_ocsigen then
  eprintf "WARNING: disabling Ocsigen build due to old OCaml version\n%!";
generate "libres3" builds;;

List.iter (fun dir ->
  try Unix.mkdir dir 0o700
  with Unix.Unix_error(Unix.EEXIST,_,_) -> ())
[
  prefix;
  prefix_bin;
  prefix_data;
  prefix_var;
  prefix_etc;
  Filename.concat prefix "3rdparty";
  Filename.concat prefix "3rdparty/libs";
  Filename.concat prefix "lib";
  Filename.concat prefix "lib/ocaml";
  destdir;
  ld_library_path
];;

printf "\n>>> Ready\nNow run 'make' and 'make install'\n";;
