-module(solve).
-compile(export_all).

support_tls(S) ->
    support_tls(S, not_in_bracket, false).

support_tls([_, _, _], _, R) ->
    R;
support_tls([A, B, B, A | T], not_in_bracket, _) when A =/= B ->
    support_tls([B, B, A | T], not_in_bracket, true);
support_tls([A, B, B, A | _], in_bracket, _) when A =/= B -> % The A=/=B part is not clear
    false;
support_tls([_, _, _, $[ | T], _, R) ->
    support_tls(T, in_bracket, R);
support_tls([_, _, _, $] |T], _, R) ->
    support_tls(T, not_in_bracket, R);
support_tls([_|T], State, R) ->
    support_tls(T, State, R).

support_ssl(S) ->
    support_ssl(S, [], [], not_in_bracket).
support_ssl([_, _], ABA, BAB, _) ->
    support_ssl(lists:sort(ABA), lists:sort(BAB));
support_ssl([$[|T], ABA, BAB, _) ->
    support_ssl(T, ABA, BAB, in_bracket);
support_ssl([$]|T], ABA, BAB, _) ->
    support_ssl(T, ABA, BAB, not_in_bracket);
support_ssl([A, B, A | T], ABA, BAB, not_in_bracket) when A =/= B ->
    support_ssl([B, A | T], [[A, B, A] | ABA], BAB, not_in_bracket);
support_ssl([B, A, B | T], ABA, BAB, in_bracket) when A=/= B ->
    support_ssl([A, B | T], ABA, [[A, B, A] | BAB], in_bracket);
support_ssl([_|T], ABA, BAB, State) ->
    support_ssl(T, ABA, BAB, State).

support_ssl([], _) ->
    false;
support_ssl(_, []) ->
    false;
support_ssl([ABA|_], [ABA|_]) ->
    true;
support_ssl([ABA|T1], [BAB|T2]) when ABA < BAB ->
    support_ssl(T1, [BAB|T2]);
support_ssl(ABA, [_|T]) ->
    support_ssl(ABA, T).

do1() ->
    doit(support_tls).
do2() ->
    doit(support_ssl).

doit(Op) ->
    {ok, FD} = file:open("input1.txt", [read]),
    doit(Op, FD, 0).
doit(Op, FD, Cnt) ->
    case file:read_line(FD) of
        {ok, Line} ->
            case apply(?MODULE, Op, [string:strip(Line, right, $\n)]) of
                true -> doit(Op, FD, Cnt+1);
                false -> doit(Op, FD, Cnt)
            end;
        eof ->
            Cnt
    end.

test() ->
    true = support_tls("abba[mnop]qrst"),
    false = support_tls("abcd[bddb]xyyx"),
    false = support_tls("aaaa[qwer]tyui"),
    true = support_tls("ioxxoj[asdfgh]zxcvbn"),

    false = support_tls("xyyx[bddb]abcd"),
    true = support_tls("abba[bbbb]abba"),
    
    true = support_ssl("aba[bab]xyz"),
    false = support_ssl("xyx[xyx]xyx"),
    true = support_ssl("aaa[kek]eke"),
    true = support_ssl("zazbz[bzb]cdb"),

    ok.

