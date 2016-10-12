
exception Error

let _eRR =
  Error

type token = 
  | TIMES
  | SEQ
  | PLUS
  | PARENTHESIS_OPEN
  | PARENTHESIS_CLOSE
  | OR
  | NOTEQ
  | NOT
  | MINUS
  | LEQ
  | IF
  | GEQ
  | EQUAL
  | EOF
  | ELSE
  | DIVIDE
  | CURLY_OPEN
  | CURLY_CLOSE
  | CONST of (int)
  | ASG
  | AND

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState36
  | MenhirState34
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState10
  | MenhirState8
  | MenhirState6
  | MenhirState3
  | MenhirState2
  | MenhirState0
  
open Parser_types

let rec _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run10 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run8 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parser_types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | CURLY_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =                                     ( Scope (e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | LEQ | MINUS | NOT | NOTEQ | OR | PARENTHESIS_CLOSE | PLUS | SEQ | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                              ( Operator (Times, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
        let _2 = () in
        let _v : (Parser_types.expression) =                               ( Operator (Divide, e, f) ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | CURLY_CLOSE | ELSE | EOF | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                           ( Seq (e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | LEQ | NOT | NOTEQ | OR | PARENTHESIS_CLOSE | PLUS | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                               ( Operator (Plus, e, f) ) in
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
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | LEQ | MINUS | NOT | NOTEQ | OR | PARENTHESIS_CLOSE | PLUS | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                              ( Operator (Minus, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | LEQ | NOTEQ | OR | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                           ( Operator (Or, e, f) ) in
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
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | LEQ | NOT | NOTEQ | OR | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                            ( Operator (Not, e, f) ) in
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
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | LEQ | NOTEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                              ( Operator (Noteq, e, f) ) in
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
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | LEQ | NOTEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                            ( Operator (And, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | ASG | CURLY_CLOSE | ELSE | EOF | LEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                              ( Operator (Leq, e, f) ) in
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
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | ASG | CURLY_CLOSE | ELSE | EOF | GEQ | LEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                            ( Operator (Geq, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | ASG | CURLY_CLOSE | ELSE | EOF | EQUAL | GEQ | LEQ | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                              ( Operator (Equal, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | ASG | CURLY_CLOSE | ELSE | EOF | PARENTHESIS_CLOSE | SEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e), _, f) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                           ( Asg (e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | CURLY_OPEN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | IF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
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
        | AND ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONST _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | CURLY_OPEN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | IF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
        | EQUAL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | CURLY_CLOSE | ELSE | EOF | PARENTHESIS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, b), _, e), _, f) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Parser_types.expression) =              ( Operator (Times, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | ASG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _2 = () in
            let _v : (Parser_types.expression) =                ( e ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | EQUAL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | SEQ ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | PARENTHESIS_OPEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | CURLY_OPEN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | IF ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Parser_types.expression) =              ( Const (_1) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

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

and top : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parser_types.expression) =
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
    | CONST _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | CURLY_OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

