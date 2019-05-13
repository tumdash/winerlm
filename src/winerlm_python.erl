%%%=============================================================================
%%% Calling python scripts for NTLM authentication
%%%=============================================================================
-module(winerlm_python).

%% API exports
-export([
    negotiate_msg/1,
    auth_msg/2
]).

negotiate_msg(Options) ->
    UserName = maps:get(user, Options),
    Password = maps:get(password, Options),
    Workstation = maps:get(workstation, Options, <<"workstation">>),
    DomainName = maps:get(domain, Options, <<"domain">>),
    ScriptCmd = negotiate_script(UserName, Password, DomainName, Workstation),
    CmdResponse = run_python_script(binary_to_list(ScriptCmd)),
    chop_lf_end(list_to_binary(CmdResponse)).

auth_msg(Options, Challenge) ->
    UserName = maps:get(user, Options),
    Password = maps:get(password, Options),
    Workstation = maps:get(workstation, Options, <<"workstation">>),
    DomainName = maps:get(domain, Options, <<"domain">>),
    ScriptCmd = authenticate_script(UserName, Password, DomainName, Workstation,
                                    Challenge),
    CmdResponse = run_python_script(binary_to_list(ScriptCmd)),
    chop_lf_end(list_to_binary(CmdResponse)).

run_python_script(PythonScript) ->
    os:cmd("python -c \"" ++ PythonScript ++ "\"").

chop_lf_end(BinaryStr) ->
    case binary:split(BinaryStr, <<"\n">>) of
        [ChoppedStr, <<>>] ->
            ChoppedStr;
        _ ->
            {error, BinaryStr}
    end.

negotiate_script(UserName, Password, DomainName, Workstation) ->
    <<
    "import base64;"
    "from ntlm_auth.ntlm import NtlmContext;"
    "ntlm_context = NtlmContext(",
        "'", UserName/binary, "', '", Password/binary, "', ",
        "'", DomainName/binary, "', '", Workstation/binary, "', ",
        "ntlm_compatibility=5);"
    "negotiate_message = ntlm_context.step();"
    "print(base64.b64encode(negotiate_message))"
    >>.

authenticate_script(UserName, Password, DomainName, Workstation, Challenge) ->
    <<
    "import base64;"
    "from ntlm_auth.ntlm import NtlmContext;"
    "ntlm_context = NtlmContext(",
        "'", UserName/binary, "', '", Password/binary, "', ",
        "'", DomainName/binary, "', '", Workstation/binary, "', ",
        "ntlm_compatibility=5);"
    "negotiate_message = ntlm_context.step();"
    "authenticate_message = ntlm_context.step(base64.b64decode('", Challenge/binary, "'));"
    "print(base64.b64encode(authenticate_message))"
    >>.
