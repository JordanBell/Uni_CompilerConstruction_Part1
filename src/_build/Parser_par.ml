
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | TRUE
  | TIMES
  | SEQ
  | REF
  | READINT
  | QUOTE_DOUBLE
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
  | FALSE
  | EQUAL
  | EOF
  | ELSE
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
  | MenhirState95
  | MenhirState87
  | MenhirState80
  | MenhirState77
  | MenhirState74
  | MenhirState72
  | MenhirState68
  | MenhirState61
  | MenhirState59
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
  | MenhirState29
  | MenhirState26
  | MenhirState23
  | MenhirState22
  | MenhirState14
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
    | MenhirState95 ->
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
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Parser_types.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : (Parser_types.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

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
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
        let _2 = () in
        let _v : (Parser_types.expression) =                                ( Operator (Divide, e, f) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
        let _1 = () in
        let _v : (Parser_types.expression) =                           ( Deref (e) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState68 | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_INT _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | FALSE ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | IDENTIFIER _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | IF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | NEW ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | NOT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | PRINTINT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | QUOTE_DOUBLE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | REF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | TRUE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_INT _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | FALSE ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | IDENTIFIER _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | IF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | NEW ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | NOT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | PRINTINT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | QUOTE_DOUBLE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | REF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | TRUE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST_INT _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | FALSE ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | IDENTIFIER _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | IF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NEW ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NOT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | PRINTINT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | QUOTE_DOUBLE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | REF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | TRUE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
        let _1 = () in
        let _v : (Parser_types.expression) =                          ( Ref (e) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
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
            | MenhirState10 | MenhirState12 | MenhirState14 | MenhirState22 | MenhirState23 | MenhirState80 | MenhirState26 | MenhirState77 | MenhirState29 | MenhirState31 | MenhirState68 | MenhirState33 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState53 | MenhirState51 | MenhirState49 | MenhirState47 | MenhirState45 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CONST_INT _v ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
                | CURLY_OPEN ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | DEREF ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | FALSE ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | IDENTIFIER _v ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
                | IF ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | LET ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | NEW ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | NOT ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | PRINTINT ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | QUOTE_DOUBLE ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | READINT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | REF ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | TRUE ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
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
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState74
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
                    let _menhir_s = MenhirState95 in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, f) = _menhir_stack in
                    let _2 = () in
                    let _v : (Parser_types.program) =                  ( [f] ) in
                    _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
                | IDENTIFIER _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
            | _ ->
                _menhir_fail ())
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
        | CONST_INT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | CURLY_OPEN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | FALSE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | IDENTIFIER _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | IF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NEW ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NOT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | PRINTINT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | QUOTE_DOUBLE ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | READINT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | REF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TRUE ->
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
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Parser_types.expression) =                      ( Const_bool (true) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | QUOTE_DOUBLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                        ( Const_string (_2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | CURLY_OPEN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | FALSE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | IDENTIFIER _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | IF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | NEW ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | NOT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | PRINTINT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | QUOTE_DOUBLE ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | READINT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | REF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | TRUE ->
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | FALSE ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | IDENTIFIER _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | IF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | NEW ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | NOT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | PRINTINT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | QUOTE_DOUBLE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | REF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | TRUE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
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

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | CURLY_OPEN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | DEREF ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FALSE ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | IDENTIFIER _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | IF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NEW ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NOT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | PRINTINT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | QUOTE_DOUBLE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | REF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | TRUE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
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

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | CURLY_OPEN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | FALSE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | IDENTIFIER _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | IF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | NEW ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | NOT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | PRINTINT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | QUOTE_DOUBLE ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | READINT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | REF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | TRUE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
        | CONST_INT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | CURLY_OPEN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | DEREF ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | FALSE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | IDENTIFIER _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | IF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | NEW ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | NOT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | PRINTINT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | QUOTE_DOUBLE ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | READINT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | REF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | TRUE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState33 in
            let _v : (Parser_types.expression list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
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

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Parser_types.expression) =                       ( Const_bool (false) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TRUE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Parser_types.expression) =                         ( Const_int (_1) ) in
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
    | CONST_INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | CURLY_OPEN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | DEREF ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | FALSE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | IDENTIFIER _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NEW ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NOT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | PRINTINT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | QUOTE_DOUBLE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | REF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | TRUE ->
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
    | MenhirState95 ->
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
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
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
  

