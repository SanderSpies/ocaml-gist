open Std

type command =
Command : string * Marg.docstring *
          ([`Mandatory|`Optional|`Many] * 'args Marg.spec) list * 'args *
          (Mpipeline.t -> 'args -> json) -> command

let command name ?(doc="") ~spec ~default f =
  Command (name, doc, spec, default, f)

let arg ?(kind=`Mandatory) name doc action = (kind, (name, doc, action))
let optional x = arg ~kind:`Optional x
let many x = arg ~kind:`Many x

let marg_position f = Marg.param "position"
    (function
      | "start" -> f `Start
      | "end" -> f `End
      | str -> match int_of_string str with
        | n -> f (`Offset n)
        | exception _ ->
          match
            let offset = String.index str ':' in
            let line = String.sub str ~pos:0 ~len:offset in
            let col = String.sub str ~pos:(offset+1)
                ~len:(String.length str - offset - 1) in
            `Logical (int_of_string line, int_of_string col)
          with
          | pos -> f pos
          | exception _ ->
            failwithf "expecting position, got %S. \
                       position can be start|end|<offset>|<line>:<col>, \
                       where offset, line and col are numbers, \
                       lines are indexed from 1."
              str
    )

let rec find_command name = function
  | [] -> raise Not_found
  | (Command (name', _, _, _, _) as command) :: xs ->
    if name = name' then
      command
    else find_command name xs

let run pipeline query =
  Logger.logj "New_commands.run" "query" (fun () -> Query_json.dump query);
  Mpipeline.with_reader pipeline @@ fun () ->
  let result = Query_commands.dispatch pipeline query in
  let json = Query_json.json_of_response query result in
  Logger.logj "New_commands.run" "result" (fun () -> json);
  json

let all_commands = [

  command "case-analysis"
    ~spec: [
      arg "-start" "<position> Where analysis starts"
        (marg_position (fun startp (_startp,endp) -> (startp,endp)));
      arg "-end" "<position> Where analysis ends"
        (marg_position (fun endp (startp,_endp) -> (startp,endp)));
    ]
~doc:"When the range determined by (-start, -end) positions is an expression,
this command replaces it with [match expr with _] expression where a branch \
is introduced for each immediate value constructor of the type that was \
determined for expr.
When it is a variable pattern, it is further expanded and new branches are \
introduced for each possible immediate constructor of this variable.
The return value has the shape \
`[{'start': position, 'end': position}, content]`, where content is string.
"
    ~default:(`Offset (-1), `Offset (-1))
    begin fun buffer -> function
      | (`Offset (-1), _) -> failwith "-start <pos> is mandatory"
      | (_, `Offset (-1)) -> failwith "-end <pos> is mandatory"
      | (startp, endp) ->
        run buffer (Query_protocol.Case_analysis (startp,endp))
    end
  ;

  command "complete-prefix"
    ~spec: [
      arg "-position" "<position> Position to complete"
          (marg_position (fun pos (txt,_pos,doc,typ) -> (txt,pos,doc,typ)));
      optional "-doc" "<bool> Add docstring to entries (default is false)"
          (Marg.bool (fun doc (txt,pos,_doc,typ) -> (txt,pos,doc,typ)));
      arg "-prefix" "<string> Prefix to complete"
          (Marg.param "string" (fun txt (_,pos,doc,typ) -> (txt,pos,doc,typ)));
      optional "-types" "<bool> Report type information (default is true)"
          (Marg.bool (fun typ (txt,pos,doc,_typ) -> (txt,pos,doc,typ)));
    ]
~doc:"This functions completes an identifier that the user started to type.
It returns a list of possible completions.
With '-types y' (default), each completion comes with type information.
With '-doc y' it tries to lookup OCamldoc, which is slightly more time consuming.

The result has the form:
```javascript
{
  'context': (null | ['application',{'argument_type': string, 'labels': [{'name':string,'type':string}]}]),
  'entries': [{'name':string,'kind':string,'desc':string,'info':string}]
}
```

Context describe where completion is occurring. Only application is distinguished now: that's when one is completing the arguments to a function call. In this case, one gets the type expected at the cursor as well as the other labels.

Entries is the list of possible completion. Each entry is made of:
- a name, the text that should be put in the buffer if selected
- a kind, one of `'value'`, `'variant'`, `'constructor'`, `'label'`, `'module'`, `'signature'`, `'type'`, `'method'`, `'#'` (for method calls), `'exn'`, `'class'`
- a description, most of the time a type or a definition line, to be put next to the name in completion box
- optional informations which might not fit in the completion box, like signatures for modules or documentation string."
    ~default:("",`None,false,true)
    begin fun buffer (txt,pos,doc,typ) ->
      match pos with
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        run buffer (Query_protocol.Complete_prefix (txt,pos,doc,typ))
    end
  ;

  command "document"
~doc:"Returns OCamldoc documentation as a string.
If `-identifier ident` is specified, documentation for this ident is looked \
up from environment at `-position`.
Otherwise, Merlin looks for the documentation for the entity under the cursor (at `-position`)."
    ~spec: [
      arg "-position" "<position> Position to complete"
          (marg_position (fun pos (ident,_pos) -> (ident,pos)));
      optional "-identifier" "<string> Identifier"
          (Marg.param "string" (fun ident (_ident,pos) -> (Some ident,pos)));
    ]
    ~default:(None,`None)
    begin fun buffer (ident,pos) ->
      match pos with
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        run buffer (Query_protocol.Document (ident, pos))
    end
  ;

  command "enclosing"
    ~spec: [
      arg "-position" "<position> Position to complete"
          (marg_position (fun pos _pos -> pos));
    ]
~doc:"Returns a list of locations `{'start': position, 'end': position}` in \
increasing size of all entities surrounding the position.
(In a lisp, this would be the locations of all s-exps that contain the cursor.)"
    ~default:`None
    begin fun buffer pos ->
      match pos with
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        run buffer (Query_protocol.Enclosing pos)
    end
  ;

  command "errors"
    ~doc:"Returns a list of errors in current buffer.
The value is a list where each item as the shape:

```javascript
{
'start' : position,
'end'   : position,
'valid' : bool,
'message' : string,
'type'  : ('type'|'parser'|'env'|'warning'|'unkown')
}
```

`start` and `end` are omitted if error has no location \
(e.g. wrong file format), otherwise the editor should probably highlight / \
mark this range.
`type` is an attempt to classify the error.
`valid` is here mostly for informative purpose. \
It reflects whether Merlin was expecting such an error to be possible or not, \
and is useful for debugging purposes.
`message` is the error description to be shown to the user."
    ~spec:[]
    ~default:()
    begin fun buffer () ->
      run buffer (Query_protocol.Errors)
    end
  ;

  command "expand-prefix"
~doc:"
The function behaves like `complete-prefix`, but it also handles partial, \
incorrect, or wrongly spelled prefixes (as determined by some heuristic).
For instance, `L.ma` can get expanded to `List.map`. This function is a \
useful fallback if normal completion gave no results.
Be careful that it always return fully qualified paths, whereas normal \
completion only completes an identifier (last part of a module path)."
    ~spec: [
      arg "-position" "<position> Position to complete"
        (marg_position (fun pos (txt,_pos,typ) -> (txt,pos,typ)));
      arg "-prefix" "<string> Prefix to complete"
        (Marg.param "string" (fun txt (_prefix,pos,typ) -> (txt,pos,typ)));
      optional "-types" "<bool> Report type information (default is false)"
        (Marg.bool (fun typ (txt,pos,_typ) -> (txt,pos,typ)));
    ]
    ~default:("",`None,false)
    begin fun buffer (txt,pos,typ) ->
      match pos with
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        run buffer (Query_protocol.Expand_prefix (txt,pos,typ))
    end
  ;

  command "extension-list"
    ~spec: [
      optional "-status" "<all|enabled|disabled> Filter extensions"
        (Marg.param "<all|enabled|disabled>"
         (fun status _status -> match status with
            | "all" -> `All
            | "enabled" -> `Enabled
            | "disabled" -> `Disabled
            | _ -> failwith "-status should be one of all, disabled or enabled"
         ));
    ]
    ~doc:"List all known / currently enabled / currently disabled extensions \
          as a list of strings."
    ~default:`All
    begin fun buffer status ->
      run buffer (Query_protocol.Extension_list status)
    end
  ;

  command "findlib-list"
    ~doc:"Returns all known findlib packages as a list of string."
    ~spec:[]
    ~default:()
    begin fun buffer () ->
      run buffer (Query_protocol.Findlib_list)
    end
  ;

  command "flags-list"
    ~spec:[]
~doc:"Returns supported compiler flags.\
The purpose of this command is to implement interactive completion of \
compiler settings in an IDE."
    ~default:()
    begin fun buffer () ->
      `List (List.map Json.string (Mconfig.flags_for_completion ()))
    end
  ;

  command "jump"
    ~spec: [
      arg "-target" "<string> Entity to jump to"
        (Marg.param "string" (fun target (_,pos) -> (target,pos)));
      arg "-position" "<position> Position to complete"
        (marg_position (fun pos (target,_pos) -> (target,pos)));
    ]
~doc:"This command can be used to assist navigation in a source code buffer.
Target is a string that can contain one or more of the 'fun', 'let', 'module' \
and 'match' words.
It returns the starting position of the function, let definition, module or \
match expression that contains the cursor
"
    ~default:("",`None)
    begin fun buffer (target,pos) ->
      match pos with
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        run buffer (Query_protocol.Jump (target,pos))
    end
  ;

  command "phrase"
    ~spec: [
      arg "-target" "<next|prev> Entity to jump to"
        (Marg.param "string" (fun target (_,pos) ->
           match target with
           | "next" -> (`Next,pos)
           | "prev" -> (`Prev,pos)
           | _ -> failwith "-target should be one of 'next' or 'prev'"
         ));
      arg "-position" "<position> Position to complete"
        (marg_position (fun pos (target,_pos) -> (target,pos)));
    ]
    ~doc:"Returns the position of the next or previous phrase \
          (top-level definition or module definition)."
    ~default:(`Next,`None)
    begin fun buffer (target,pos) ->
      match pos with
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        run buffer (Query_protocol.Phrase (target,pos))
    end
  ;

  command "list-modules"
    ~spec:[
      many "-ext" "<extension> file extensions to look for"
        (Marg.param "extension" (fun ext exts -> ext :: exts));
    ]
~doc:"Looks into project source paths for files with an extension \
matching and prints the corresponding module name."
    ~default:[]

    begin fun buffer extensions ->
      run buffer (Query_protocol.List_modules (List.rev extensions))
    end
  ;

  command "locate"
    ~spec: [
      optional "-prefix" "<string> Prefix to complete"
        (Marg.param "string" (fun txt (_,pos,kind) -> (Some txt,pos,kind)));
      arg "-position" "<position> Position to complete"
        (marg_position (fun pos (prefix,_pos,kind) -> (prefix,pos,kind)));
      optional "-look-for" "<interface|implementation> Prefer opening interface or implementation"
        (Marg.param "<interface|implementation>"
          (fun kind (prefix,pos,_) -> match kind with
            | "mli" | "interface" -> (prefix,pos,`MLI)
            | "ml"  | "implementation" -> (prefix,pos,`ML)
            | str ->
              failwithf "expecting interface or implementation, got %S." str));
    ]
~doc:"Finds the declaration of entity at the specified position, \
Or referred to by specified string.
Returns either:
- if location failed, a `string` describing the reason to the user,
- `{'pos': position}` if the location is in the current buffer,
- `{'file': string, 'pos': position}` if definition is located in a \
different file."
    ~default:(None,`None,`MLI)
    begin fun buffer (prefix,pos,lookfor) ->
      match pos with
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        run buffer (Query_protocol.Locate (prefix,lookfor,pos))
    end
  ;

  command "occurrences"
    ~spec: [
      arg "-identifier-at" "<position> Position to complete"
        (marg_position (fun pos _pos -> (`Ident_at pos)));
    ]
~doc:"Returns a list of locations `{'start': position, 'end': position}` \
of all occurrences in current buffer of the entity at the specified position."
    ~default:`None
    begin fun buffer -> function
      | `None -> failwith "-identifier-at <pos> is mandatory"
      | `Ident_at pos ->
        run buffer (Query_protocol.Occurrences (`Ident_at pos))
    end
  ;

  command "outline"
    ~spec:[]
~doc:"Returns a tree of objects `{'start': position, 'end': position, \
'name': string, 'kind': string, 'children': subnodes}` describing the content \
of the buffer."
    ~default:()
    begin fun buffer () ->
      run buffer (Query_protocol.Outline)
    end
  ;

  command "path-of-source"
    ~doc:"Looks for first file with a matching name in the project source \
          and build paths"
    ~spec: [
      arg "-file" "<filename> filename to look for in project paths"
        (Marg.param "filename" (fun file files -> file :: files));
    ]
    ~default:[]

    begin fun buffer filenames ->
      run buffer (Query_protocol.Path_of_source (List.rev filenames))
    end
  ;

  command "shape"
~doc:"This command can be used to assist navigation in a source code buffer.
It returns a tree of all relevant locations around the cursor.
It is similar to outline without telling any information about the entity \
at a given location.
```javascript
shape =
{
  'start' : position,
  'end'   : position,
  'children' : [shape]
}
```
"
    ~spec: [
      arg "-position" "<position> Position "
        (marg_position (fun pos _pos -> pos));
    ]
    ~default:`None
    begin fun buffer -> function
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        run buffer (Query_protocol.Shape pos)
    end
  ;

  command "type-enclosing"
~doc:"Returns a list of type information for all expressions at given \
position, sorted by increasing size.
That is asking for type enlosing around `2` in `string_of_int 2` will return \
the types of `2 : int` and `string_of_int 2 : string`.

If `-expression` and `-cursor` are specified, the first result will be the type
relevant to the prefix ending at the `cursor` offset.

`-index` can be used to print only one type information. This is useful to
query the types lazily: normally, Merlin would return the signature of all
enclosing modules, which can be very expensive.

The result is returned as a list of:
```javascript
{
  'start': position,
  'end': position,
  'type': string,
  // is this expression not in tail position, in tail position, \
or even a tail call?
  'tail': ('no' | 'position' | 'call')
}
```"
    ~spec: [
      arg "-position" "<position> Position to complete"
        (marg_position (fun pos (expr,cursor,_pos,index) -> (expr,cursor,pos,index)));
      optional "-expression" "<string> Expression to type"
        (Marg.param "string" (fun expr (_expr,cursor,pos,index) -> (expr,cursor,pos,index)));
      optional "-cursor" "<int> Position of the cursor inside expression"
        (Marg.param "int" (fun cursor (expr,_cursor,pos,index) ->
            match int_of_string cursor with
            | cursor -> (expr,cursor,pos,index)
            | exception exn ->
              failwith "cursor should be an integer"
          ));
      optional "-index" "<int> Only print type of <index>'th result"
        (Marg.param "int" (fun index (expr,cursor,pos,_index) ->
            match int_of_string index with
            | index -> (expr,cursor,pos,Some index)
            | exception exn ->
              failwith "index should be an integer"
          ));
    ]
    ~default:("",-1,`None,None)
    begin fun buffer (expr,cursor,pos,index) ->
      match pos with
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        let expr =
          if expr = "" then None
          else
            let cursor = if cursor = -1 then String.length expr else cursor in
            Some (expr, cursor)
        in
        run buffer (Query_protocol.Type_enclosing (expr,pos,index))
    end
  ;

  command "type-expression"
~doc:"Returns the type of the expression when typechecked in the environment \
around the specified position."
    ~spec: [
      arg "-position" "<position> Position to complete"
        (marg_position (fun pos (expr,_pos) -> (expr,pos)));
      arg "-expression" "<string> Expression to type"
        (Marg.param "string" (fun expr (_expr,pos) -> (expr,pos)));
    ]
    ~default:("",`None)
    begin fun buffer (expr,pos) ->
      match pos with
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        run buffer (Query_protocol.Type_expr (expr,pos))
    end
  ;

  (* Implemented without support from Query_protocol.  This command might be
     refactored if it proves useful for old protocol too.  *)
  command "check-configuration"
    ~spec:[]
~doc:"This command checks that merlin project and options are correct.
The return value has the shape:
```javascript
{
  'dot_merlins': [path], // a list of string
  'failures': [message]  // a list of string
}
```"
    ~default:()
    begin fun pipeline () ->
      let config = Mpipeline.final_config pipeline in
      `Assoc [
        "dot_merlins", `List (List.rev_map Json.string
                                Mconfig.(config.merlin.dotmerlin_loaded));
        "failures", `List (List.map Json.string
                             Mconfig.(config.merlin.failures));
      ]
    end
  ;

]
