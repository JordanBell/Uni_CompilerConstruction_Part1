
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | TRUE
  | TIMES
  | STRUCT
  | STRING_VALUE of (string)
  | SEQ
  | REF
  | READINT
  | PRINT
  | PLUS
  | PARENTHESIS_OPEN
  | PARENTHESIS_CLOSE
  | OR
  | NOTEQ
  | NOT
  | NEW
  | MINUS
  | LET
  | LEQ
  | INCLUDE
  | IN
  | IF
  | IDENTIFIER of (string)
  | GEQ
  | FALSE
  | EQUAL
  | EOF
  | ELSE
  | DOT
  | DIVIDE
  | DEREF
  | CURLY_OPEN
  | CURLY_CLOSE
  | CONST_INT of (int)
  | COMMA
  | ASG
  | AND

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState110
  | MenhirState107
  | MenhirState105
  | MenhirState100
  | MenhirState98
  | MenhirState96
  | MenhirState93
  | MenhirState87
  | MenhirState80
  | MenhirState77
  | MenhirState74
  | MenhirState72
  | MenhirState66
  | MenhirState57
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState21
  | MenhirState18
  | MenhirState17
  | MenhirState12
  | MenhirState7
  | MenhirState5
  | MenhirState3
  | MenhirState0
  
open Parser_types

let rec _menhir_goto_memlist : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parser_types.memlist) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CURLY_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _2), _, ms) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Parser_types.structdef) =                                                                    ( (_2, ms) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState107 in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, s) = _menhir_stack in
                let _2 = () in
                let _v : (Parser_types.program * string list) =                                           (                                (([], [s]), []) ) in
                _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
            | IDENTIFIER _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | INCLUDE ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | STRUCT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, md), _, ml) = _menhir_stack in
        let _2 = () in
        let _v : (Parser_types.memlist) =                                   (	md :: ml ) in
        _menhir_goto_memlist _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parser_types.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Parser_types.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : (Parser_types.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parser_types.program * string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, s), _, p) = _menhir_stack in
        let _v : (Parser_types.program * string list) =                                           ( match p with ((fs, ss), is) -> ((fs, s::ss), is) ) in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, f), _, p) = _menhir_stack in
        let _v : (Parser_types.program * string list) =                                           ( match p with ((fs, ss), is) -> ((f::fs, ss), is) ) in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), i), _, p) = _menhir_stack in
        let _1 = () in
        let _v : (Parser_types.program * string list) =                                       ( match p with ((fs, ss), is) -> ((fs, ss), i::is)) in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, p) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.program * (string list)) =                  ( p ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parser_types.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARENTHESIS_CLOSE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, xs0) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Parser_types.expression list) = let args =
          let xs = xs0 in
              ( xs )
        in
                                                                                         ( args ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let args = _v in
        let (_menhir_stack, _menhir_s, str) = _menhir_stack in
        let _v : (Parser_types.expression) =                                      ( Application (Identifier(str), args) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | CURLY_CLOSE | IN | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _v : (Parser_types.expression) =                               ( Seq (e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | MINUS | NOTEQ | OR | PARENTHESIS_CLOSE | PLUS | SEQ | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                                ( Operator (Times, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
        let _2 = () in
        let _v : (Parser_types.expression) =                                ( Operator (Divide, e, f) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | CURLY_CLOSE | IN | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                              ( Seq (e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | PLUS | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                                ( Operator (Plus, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | MINUS | NOTEQ | OR | PARENTHESIS_CLOSE | PLUS | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                                ( Operator (Minus, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                              ( Operator (Or, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | NOTEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                                ( Operator (Noteq, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | NOTEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                               ( Operator (And, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | ASG | COMMA | CURLY_CLOSE | IN | LEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                                ( Operator (Leq, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | ASG | COMMA | CURLY_CLOSE | GEQ | IN | LEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                               ( Operator (Geq, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                                ( Operator (Equal, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | ASG | COMMA | CURLY_CLOSE | IN | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                              ( Asg (e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CURLY_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                                                 ( Scope (e) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState5 | MenhirState7 | MenhirState12 | MenhirState17 | MenhirState18 | MenhirState80 | MenhirState21 | MenhirState77 | MenhirState24 | MenhirState26 | MenhirState66 | MenhirState28 | MenhirState30 | MenhirState57 | MenhirState55 | MenhirState53 | MenhirState51 | MenhirState49 | MenhirState47 | MenhirState45 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState33 | MenhirState31 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CONST_INT _v ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                | CURLY_OPEN ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | DEREF ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | FALSE ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | IDENTIFIER _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                | IF ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | LET ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | NEW ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | NOT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | PRINT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | READINT ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | REF ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | STRING_VALUE _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                | STRUCT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | TRUE ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | WHILE ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
            | MenhirState72 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | CURLY_OPEN ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState74 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, b), _, e), _, f) = _menhir_stack in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Parser_types.expression) =                         ( If (b, e, f) ) in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | MenhirState87 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, b), _, e) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Parser_types.expression) =                         ( While (b, e) ) in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | MenhirState105 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, funcid), _, args), _, e) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (Parser_types.fundef) =                     ( Myfunc (funcid, args, e) ) in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EOF ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState110 in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, f) = _menhir_stack in
                    let _2 = () in
                    let _v : (Parser_types.program * string list) =                                           (                                (([f], []), []) ) in
                    _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
                | IDENTIFIER _v ->
                    _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
                | INCLUDE ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | STRUCT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
            | _ ->
                _menhir_fail ())
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
        let _1 = () in
        let _v : (Parser_types.expression) =                           ( Deref (e) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState66 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | CURLY_OPEN ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | DEREF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | FALSE ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | REF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | STRING_VALUE _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | STRUCT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Parser_types.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CURLY_OPEN ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | CURLY_OPEN ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | DEREF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | FALSE ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | REF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | STRING_VALUE _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | STRUCT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | CURLY_CLOSE | IN | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), id), _, e), _, f) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                        ( Let (id, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | CURLY_OPEN ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | DEREF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | FALSE ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | REF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | STRING_VALUE _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | STRUCT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | CURLY_CLOSE | IN | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), id), _, e), _, f) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                        ( New (id, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _1 = () in
            let _v : (Parser_types.expression) =                           ( Operator_unary (Not, e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                           ( Printint (e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
        let _1 = () in
        let _v : (Parser_types.expression) =                          ( Ref (e) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CURLY_OPEN ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | CURLY_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, e) = _menhir_stack in
            let _2 = () in
            let _v : (string * Parser_types.expression) =                                    ( (_1, e) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENTIFIER _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
            | CURLY_CLOSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, md) = _menhir_stack in
                let _v : (Parser_types.memlist) =                          ( [md] ) in
                _menhir_goto_memlist _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARENTHESIS_OPEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | CURLY_OPEN ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | DEREF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | FALSE ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | LET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | NEW ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | PRINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | READINT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | REF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | STRING_VALUE _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | STRUCT ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Parser_types.expression) =                      ( Const_bool (true) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Parser_types.expression) =                            ( Const_struct (_2) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Parser_types.expression) =                         ( Const_string (_1) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARENTHESIS_OPEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                                                 ( Readint ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARENTHESIS_OPEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | CURLY_OPEN ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | DEREF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | FALSE ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | LET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | NEW ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | PRINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | READINT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | REF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | STRING_VALUE _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | STRUCT ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | CURLY_OPEN ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | DEREF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | FALSE ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | REF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | STRING_VALUE _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | STRUCT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | CURLY_OPEN ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | DEREF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | FALSE ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | REF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | STRING_VALUE _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | STRUCT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARENTHESIS_OPEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | CURLY_OPEN ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | DEREF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | FALSE ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | NEW ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | PRINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | READINT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | REF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | STRING_VALUE _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | STRUCT ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _3 = _v in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                                 ( Memaccess (Identifier (_1), Identifier(_3)) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | PARENTHESIS_OPEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | CURLY_OPEN ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DEREF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | FALSE ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | NEW ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PRINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | READINT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | REF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | STRING_VALUE _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | STRUCT ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState28 in
            let _v : (Parser_types.expression list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | AND | ASG | COMMA | CURLY_CLOSE | DIVIDE | EQUAL | GEQ | IN | LEQ | MINUS | NOTEQ | OR | PARENTHESIS_CLOSE | PLUS | SEQ | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Parser_types.expression) =                         ( Identifier (_1) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Parser_types.expression) =                       ( Const_bool (false) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Parser_types.expression) =                         ( Const_int (_1) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CURLY_OPEN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | FALSE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | REF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | STRING_VALUE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | STRUCT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASG ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | CURLY_OPEN ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | DEREF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | FALSE ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | LET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | NEW ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | PRINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | READINT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | REF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | STRING_VALUE _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | STRUCT ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let _v : (string list) = let args =
      let xs = xs0 in
          ( xs )
    in
                                                       ( args ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARENTHESIS_CLOSE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CURLY_OPEN ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run99 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | PARENTHESIS_CLOSE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (string list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CURLY_OPEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STRING_VALUE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | INCLUDE ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | STRUCT ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARENTHESIS_OPEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState98 in
            let _v : (string list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and top : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parser_types.program * (string list)) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INCLUDE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

