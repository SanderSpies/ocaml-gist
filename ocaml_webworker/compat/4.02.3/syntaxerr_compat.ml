let prepare_error err =
  let msg = match err with
  | Syntaxerr.Unclosed(opening_loc, opening, closing_loc, closing) ->
      Format.sprintf
        "Syntax error: '%s' expected" closing

  | Syntaxerr.Expecting (loc, nonterm) ->
      "Syntax error: " ^ nonterm ^ " expected."
  | Syntaxerr.Not_expecting (loc, nonterm) ->
      "Syntax error: " ^ nonterm ^ " not expected."
  | Syntaxerr.Applicative_path loc ->
        "Syntax error: applicative paths of the form F(X).t \
         are not supported when the option -no-app-func is set."
  | Syntaxerr.Variable_in_scope (loc, var) ->
      Format.sprintf
        "In this scoped type, variable '%s \
         is reserved for the local type %s."
         var var
  | Syntaxerr.Other loc ->
      "Syntax error"
  | Syntaxerr.Ill_formed_ast (loc, s) ->
      "broken invariant in parsetree:" ^ s
  in
  match err with
  | Unclosed (loc, s, loc2, s2) -> Some ("SyntaxError", "Unclosed", [loc; loc2], msg)
  | Expecting (loc, s) ->  Some ("SyntaxError", "Expecting", [loc], msg)
  | Not_expecting (loc, s) -> Some ("SyntaxError", "Not_expecting", [loc], msg)
  | Applicative_path loc -> Some ("SyntaxError", "Applicative_path", [loc], msg)
  | Variable_in_scope (loc, s) -> Some ("SyntaxError", "Variable_in_scope", [loc], msg)
  | Other loc -> Some ("SyntaxError", "Other", [loc], msg)
  | Ill_formed_ast (loc, s) -> Some ("SyntaxError", "Ill_formed_ast", [loc], msg)
