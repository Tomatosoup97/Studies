open Xi_lib.Iface
open Xi_lib.Plugin
open Xi_lib.Plugin_register


module LexerAndParser = struct

  type token = Parser.token

  module Lexer = Lexer

  module Parser = Parser

end

module Plugin : PLUGIN = struct

  let version = "na"

  let make_live_variables_analysis = None

  let make_dominators_analysis = None

  let make_scheduler = None

  let make_natural_loops_analysis = None

  let make_spill_costs_analysis = None

  (* Set to None to use built-in lexer and parser *)
  let lexer_and_parser = Some (module LexerAndParser : LEXER_AND_PARSER)

  (* Set to None to use built-in typechecker *)
  let make_typechecker = Some (module Typechecker.Make : MAKE_TYPECHECKER)

  let make_translator = Some (module Translator.Make : MAKE_TRANSLATOR)

  let make_jump_threading = None

  let make_constant_folding = None

  let make_hilower = None

  let make_callconv = None

  let make_mipslower = None

  let make_register_allocator = None

  let make_constant_folding_analysis = None

  let make_codegen = None

  let make_dead_code_elimination = None

  let make_interference_graph_analysis = None

  let make_spilling = None

  let make_reachability_analysis = None
end

module RegisterMyPlugin = RegisterPlugin(Plugin)
