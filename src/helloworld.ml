open Sast
open Ast

let lift input =
  let lexbuf = Lexing.from_string input in
  let _ = Parser.program Scanner.token lexbuf in
  let sast = [
    SVDecl(Func({ param_typs = []; return_typ = Func({ param_typs = []; return_typ = String }) }), 
    "foo", 
    Some((Func({ param_typs = []; return_typ = Func({ param_typs = []; return_typ = String }) }), SFExpr({
      sname = "foo";
      srecursive = false;
      styp = Func({ param_typs = []; return_typ = String });
      sparams = [];
      sbody = [
        SVDecl(String, "hello", Some((String, SStrLit("Hello World"))));
        SVDecl(Func({ param_typs = []; return_typ = String }), "bar", Some((Func({ param_typs = []; return_typ = String }), SFExpr({
          sname = "bar";
          srecursive = false;
          styp = String;
          sparams = [];
          sbody = [
            SReturn((String, SId("hello")))
          ]
        }))));
        SReturn((Func({ param_typs = []; return_typ = String }), SId("bar")))
      ];
    }))));
    SVDecl(Func({ param_typs = []; return_typ = String }), "baz", Some((String, SFCall("foo", []))));
    SExpr((String, SFCall("println", [(String, SFCall("baz", []))])))
  ] in
  let lsast = Lift.lift sast in
  let m = Codegen.translate lsast in
  Llvm_analysis.assert_valid_module m;
  print_string (Llvm.string_of_llmodule m)

let () =
  lift ""