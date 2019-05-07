%%%=============================================================================
%%% Calling python scripts for NTLM authentication
%%%=============================================================================
-module(winerlm_python).

%% API exports
-export([
    negotiate_msg/1,
    auth_msg/2
]).


-define(NEGOTIATE_CMD, "python etc/auth_script.py negotiate ").
-define(AUTH_CMD, "python etc/auth_script.py authenticate ").

negotiate_msg(Options) ->
    UserName = maps:get(user, Options),
    Password = maps:get(password, Options),
    Workstation = maps:get(workstation, Options, <<"workstation">>),
    DomainName = maps:get(domain, Options, <<"domain">>),
    ScriptCmd = <<?NEGOTIATE_CMD,
        UserName/binary, " ", Password/binary, " ",
        DomainName/binary, " ", Workstation/binary>>,
    %io:format("negotiate_msg scripting: ~p~n", [ScriptCmd]),
    StringResponse = os:cmd(binary_to_list(ScriptCmd)),
    chop_lf_end(list_to_binary(StringResponse)).

auth_msg(Options, ChallengeResponse) ->
    UserName = maps:get(user, Options),
    Password = maps:get(password, Options),
    Workstation = maps:get(workstation, Options, <<"workstation">>),
    DomainName = maps:get(domain, Options, <<"domain">>),
    ScriptCmd = <<?AUTH_CMD,
        UserName/binary, " ", Password/binary, " ",
        DomainName/binary, " ", Workstation/binary, " ",
        ChallengeResponse/binary>>,
    %io:format("authenticate scripting: ~p~n", [ScriptCmd]),
    StringResponse = os:cmd(binary_to_list(ScriptCmd)),
    chop_lf_end(list_to_binary(StringResponse)).

chop_lf_end(BinaryStr) ->
    case binary:split(BinaryStr, <<"\n">>) of
        [ChoppedStr, <<>>] ->
            ChoppedStr;
        _ ->
            {error, BinaryStr}
    end.
