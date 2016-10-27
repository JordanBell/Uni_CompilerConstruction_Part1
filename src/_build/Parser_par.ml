
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | TIMES
  | SEQ
  | REF
  | READINT
  | PRINTINT
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
  | IN
  | IF
  | IDENTIFIER of (string)
  | GEQ
  | EQUAL
  | EOF
  | ELSE
  | DIVIDE
  | DEREF
  | CURLY_OPEN
  | CURLY_CLOSE
  | CONST of (int)
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
  | MenhirState90
  | MenhirState82
  | MenhirState75
  | MenhirState72
  | MenhirState69
  | MenhirState67
  | MenhirState63
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState22
  | MenhirState19
  | MenhirState18
  | MenhirState13
  | MenhirState12
  | MenhirState10
  | MenhirState9
  | MenhirState4
  | MenhirState2
  | MenhirState0
  
open Parser_types

let rec _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parser_types.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
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
            let _v : (Parser_types.program) =                  ( p ) in
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
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, f), _, p) = _menhir_stack in
        let _v : (Parser_types.program) =                      ( f :: p ) in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parser_types.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Parser_types.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : (Parser_types.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

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
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
        let _2 = () in
        let _v : (Parser_types.expression) =                                 ( Operator (Divide, e, f) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | PLUS | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                                 ( Operator (Plus, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                             ( Operator (Or, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | NOTEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                              ( Operator (And, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | ASG | COMMA | CURLY_CLOSE | GEQ | IN | LEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                              ( Operator (Geq, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
        let _1 = () in
        let _v : (Parser_types.expression) =                         ( Deref (e) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState63 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | DEREF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | IDENTIFIER _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | DEREF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | IDENTIFIER _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | CURLY_CLOSE | IN | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), id), _, e), _, f) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                     ( Let (id, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | DEREF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | IDENTIFIER _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | CURLY_CLOSE | IN | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), id), _, e), _, f) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                     ( New (id, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | COMMA | CURLY_CLOSE | EQUAL | GEQ | IN | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _1 = () in
            let _v : (Parser_types.expression) =                        ( Operator_unary (Not, e) ) in
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
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                         ( Printint (e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
        let _1 = () in
        let _v : (Parser_types.expression) =                        ( Ref (e) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
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
            | MenhirState10 | MenhirState12 | MenhirState13 | MenhirState18 | MenhirState19 | MenhirState75 | MenhirState22 | MenhirState72 | MenhirState25 | MenhirState27 | MenhirState63 | MenhirState29 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState34 | MenhirState32 | MenhirState30 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CONST _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | CURLY_OPEN ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | DEREF ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | IDENTIFIER _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
                | IF ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | LET ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | NEW ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | NOT ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | PRINTINT ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | READINT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | REF ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
            | MenhirState67 ->
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
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState69 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, b), _, e), _, f) = _menhir_stack in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Parser_types.expression) =                       ( If (b, e, f) ) in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | MenhirState82 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, b), _, e) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Parser_types.expression) =                       ( While (b, e) ) in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | MenhirState9 ->
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
                    let _menhir_s = MenhirState90 in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, f) = _menhir_stack in
                    let _2 = () in
                    let _v : (Parser_types.program) =                  ( [f] ) in
                    _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
                | IDENTIFIER _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
            | _ ->
                _menhir_fail ())
        | DIVIDE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | CONST _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | CURLY_OPEN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | DEREF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | IDENTIFIER _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NEW ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NOT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | PRINTINT ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | READINT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | REF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | CONST _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | CURLY_OPEN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | DEREF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | IDENTIFIER _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | NEW ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | NOT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | PRINTINT ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | READINT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | REF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            | CONST _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | DEREF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | IDENTIFIER _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
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

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            | CONST _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | DEREF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | IDENTIFIER _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
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

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | CONST _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | CURLY_OPEN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | DEREF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | IDENTIFIER _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | NEW ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | NOT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | PRINTINT ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | READINT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | REF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
        | CONST _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | CURLY_OPEN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | DEREF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | IDENTIFIER _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | NEW ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | NOT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | PRINTINT ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | READINT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | REF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState29 in
            let _v : (Parser_types.expression list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
    | AND | ASG | COMMA | CURLY_CLOSE | DIVIDE | EQUAL | GEQ | IN | LEQ | MINUS | NOTEQ | OR | PARENTHESIS_CLOSE | PLUS | SEQ | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Parser_types.expression) =                      ( Identifier (_1) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Parser_types.expression) =                   ( Const (_1) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | DEREF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | IDENTIFIER _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

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
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
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
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState2 in
            let _v : (string list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
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

and top : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parser_types.program) =
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
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

