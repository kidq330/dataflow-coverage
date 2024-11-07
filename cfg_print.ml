open Cil_types

let print_stmt out = function
  | Instr i -> Printer.pp_instr out i
  | Return _ -> Format.pp_print_string out "<return>"
  | Goto _ -> Format.pp_print_string out "<goto>"
  | Break _ -> Format.pp_print_string out "<break>"
  | Continue _ -> Format.pp_print_string out "<continue>"
  | If (e,_,_,_) -> Format.fprintf out "if %a" Printer.pp_exp e
  | Switch(e,_,_,_) -> Format.fprintf out "switch %a" Printer.pp_exp e
  | Loop _ -> Format.fprintf out "<loop>"
  | Block _ -> Format.fprintf out "<block>"
  | UnspecifiedSequence _ -> Format.fprintf out "<unspecified sequence>"
  | TryFinally _ | TryExcept _ | TryCatch _ -> Format.fprintf out "<try>"
  | Throw _ -> Format.fprintf out "<throw>"

class print_cfg out = object
  inherit Visitor.frama_c_inplace

  method! vfile _ =
    Format.fprintf out "@[<hov 2>digraph cfg {@ ";
    Cil.DoChildrenPost (fun f -> Format.fprintf out "}@]@."; f)

  method! vglob_aux g =
    match g with
    | GFun (f, _) ->
      Format.fprintf out "@[<hov 2>subgraph cluster_%a {@ \
                          @[<hv 2> graph@ [label=\"%a\"];@]@ "
        Printer.pp_varinfo f.svar
        Printer.pp_varinfo f.svar;
      Cil.DoChildrenPost (fun g -> Format.fprintf out "}@]@ "; g)
    | _ -> Cil.SkipChildren

  method! vstmt_aux s =
    let color = if Eva.Analysis.is_computed () then
        let reachable = Eva.Results.is_reachable s in
        if reachable then "fillcolo=\"#CCFFCC\" style=filled"
        else "fillcolor=pink style=filled"
      else ""
    in
    Format.fprintf out "@[s%d@ [label=%S %s]@];@ "
      s.sid (Pretty_utils.to_string print_stmt s.skind) color;
    List.iter
      (fun succ -> Format.fprintf out "@[s%d -> s%d;@]@ " s.sid succ.sid)
      s.succs;
    Format.fprintf out "@]";
    Cil.DoChildren
end

module Self = Plugin.Register(struct
    let name = "control flow graph"
    let shortname = "viewcfg"
    let help = "control flow graph computation and display"
  end)

module Enabled = Self.False(struct
    let option_name = "-cfg"
    let help = "when on (off by default), computes and CFG of all functions."
  end)

module OutputFile = Self.String(struct
    let option_name = "-cfg-output"
    let default = "cfg.dot"
    let arg_name = "output-file"
    let help = "file where the graph is output, in dot format."
  end)

let run () =
  if Enabled.get () then
    let filename = OutputFile.get () in
    let chan = open_out filename in
    let fmt = Format.formatter_of_out_channel chan in
    Visitor.visitFramacFileSameGlobals (new print_cfg fmt) (Ast.get ());
    close_out chan

let () = Boot.Main.extend run
