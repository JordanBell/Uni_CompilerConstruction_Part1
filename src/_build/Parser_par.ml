
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | TYPE_INT
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
  | MINUS
  | LEQ
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
  | MenhirState77
  | MenhirState71
  | MenhirState68
  | MenhirState61
  | MenhirState59
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
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState20
  | MenhirState15
  | MenhirState14
  | MenhirState11
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
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, f), _, p) = _menhir_stack in
        let _v : (Parser_types.program) =                      ( f :: p ) in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | CURLY_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                                       ( Scope (e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_OPEN ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
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
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | IDENTIFIER _ | LEQ | MINUS | NOTEQ | OR | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | PLUS | SEQ | TIMES ->
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
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
        let _2 = () in
        let _v : (Parser_types.expression) =                                 ( Operator (Divide, e, f) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | CURLY_CLOSE | ELSE | EOF | IDENTIFIER _ | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                             ( Seq (e, f) ) in
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
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | IDENTIFIER _ | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | PLUS | SEQ ->
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
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | IDENTIFIER _ | LEQ | MINUS | NOTEQ | OR | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | PLUS | SEQ ->
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
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | IDENTIFIER _ | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
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
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | IDENTIFIER _ | LEQ | NOTEQ | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
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
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | IDENTIFIER _ | LEQ | NOTEQ | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
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
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ASG | CURLY_CLOSE | ELSE | EOF | IDENTIFIER _ | LEQ | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
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
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ASG | CURLY_CLOSE | ELSE | EOF | GEQ | IDENTIFIER _ | LEQ | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
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
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | IDENTIFIER _ | LEQ | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
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
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ASG | CURLY_CLOSE | ELSE | EOF | IDENTIFIER _ | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                             ( Asg (e, f) ) in
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Parser_types.expression) =                       ( Application (e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | PARENTHESIS_OPEN ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
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
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | IDENTIFIER _ | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _1 = () in
            let _v : (Parser_types.expression) =                       ( Deref (e) ) in
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
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | CURLY_OPEN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | DEREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | IDENTIFIER _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | NOT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | PRINTINT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | READINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | REF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | TYPE_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | PARENTHESIS_OPEN ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | CURLY_OPEN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | DEREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | IDENTIFIER _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | NOT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | PRINTINT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | READINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | REF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | TYPE_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_OPEN ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_OPEN ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | CURLY_CLOSE | ELSE | EOF | IDENTIFIER _ | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, b), _, e), _, f) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                ( If (b, e, f) ) in
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
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | IDENTIFIER _ | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
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
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                       ( Printint (e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | PARENTHESIS_OPEN ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | IDENTIFIER _ | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | PARENTHESIS_OPEN | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _1 = () in
            let _v : (Parser_types.expression) =                      ( Ref (e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_OPEN ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | CURLY_OPEN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | DEREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | IDENTIFIER _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | NOT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | PRINTINT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | READINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | REF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | TYPE_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_OPEN ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | CURLY_CLOSE | ELSE | EOF | IDENTIFIER _ | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _2), _, e), _, f) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                   ( New (_2, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | CURLY_OPEN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | DEREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | IDENTIFIER _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | NOT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | PRINTINT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | READINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | REF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | TYPE_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
        | PARENTHESIS_OPEN ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | CURLY_CLOSE | ELSE | EOF | IDENTIFIER _ | PARENTHESIS_CLOSE | PARENTHESIS_OPEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, b), _, e) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =               ( While (b, e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_OPEN ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | EOF | IDENTIFIER _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, funcid), _, args), _, e) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Parser_types.fundef) =               ( Myfunc (funcid, args, e) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState77 in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, f) = _menhir_stack in
                let _2 = () in
                let _v : (Parser_types.program) =                  ( [f] ) in
                _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
            | IDENTIFIER _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
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

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | CURLY_OPEN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | DEREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | IDENTIFIER _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | NOT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | PRINTINT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | READINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | REF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | TYPE_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
            | CURLY_OPEN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | DEREF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | IDENTIFIER _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | NOT ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | PRINTINT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | READINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | REF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | TYPE_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
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

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

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

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | CURLY_OPEN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | DEREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | IDENTIFIER _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | NOT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | PRINTINT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | READINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | REF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | TYPE_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
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
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | CURLY_OPEN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | DEREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | IDENTIFIER _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | NOT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | PRINTINT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | READINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | REF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | TYPE_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Parser_types.expression) =                    ( Identifier (_1) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CURLY_OPEN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | DEREF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | IDENTIFIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NOT ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | PRINTINT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | READINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | REF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | TYPE_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Parser_types.expression) =                ( Const (_1) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

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
        | CONST _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | CURLY_OPEN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | DEREF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | IDENTIFIER _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | NOT ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | PRINTINT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | READINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | REF ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | TYPE_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | WHILE ->
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
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
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
  

