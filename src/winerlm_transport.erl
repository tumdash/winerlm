%%%=============================================================================
%%% Module for winrm transport
%%%=============================================================================
-module(winerlm_transport).

%% API exports
-export([
    enumerate/2,
	  pull/2
]).

-define(APPLICATION_TYPE, <<"application/soap+xml;charset=UTF-8">>).

%%%=============================================================================
%% API functions
%%%=============================================================================
enumerate(#{transport := ntlm, connect := HConnect} = Options, Msg) ->
    Negotiate = winerlm_ntlm:negotiate_v2(Options),
    NegMsg = base64:encode(Negotiate),
    UnsecHeaders = [
      {<<"Content-Type">>, ?APPLICATION_TYPE},
      {<<"Authorization">>, <<"Negotiate ", NegMsg/binary>>}
  	],
    UnsecureEnumReq = {post, <<"/wsman">>, UnsecHeaders, Msg},
  	%TODO handle errors always a good idea
    case hackney:send_request(HConnect, UnsecureEnumReq) of
        {ok, 401, RespHdrs, HConnect} ->
            hackney:body(HConnect),
            case maps:get(<<"WWW-Authenticate">>, maps:from_list(RespHdrs), undefined) of
                <<"Negotiate ", ChallengeMsg/binary>> ->
                    Challenge = base64:decode(ChallengeMsg),
                    Authenticate = winerlm_ntlm:authenticate_v2(Options, Negotiate, Challenge),
                    AuthMsg = base64:encode(Authenticate),
                    SecHeaders = [
                      {<<"Content-Type">>, ?APPLICATION_TYPE},
                      {<<"Authorization">>, <<"Negotiate ", AuthMsg/binary>>}
                    ],
                    SecureEnumReq = {post, <<"/wsman">>, SecHeaders, Msg},
                    case hackney:send_request(HConnect, SecureEnumReq) of
                        {ok, _, _, HConnect} ->
                            hackney:body(HConnect);
                        Err ->
                            {error, {could_not_enumerate, Err}}
                    end;
                undefined ->
                    {error, no_auth_error}
            end;
        Err ->
            {error, {auth_error, Err}}
    end;
enumerate(#{transport := basic} = Options, Msg) ->
	 basic_auth_request(Options, Msg).

pull(#{transport := ntlm, connect := HConnect}, Msg) ->
    Headers = [{<<"Content-Type">>, ?APPLICATION_TYPE}],
    PullReq = {post, <<"/wsman">>, Headers, Msg},
  	%TODO handle errors always a good idea
    case hackney:send_request(HConnect, PullReq) of
        {ok, 200, _, HConnect} ->
            hackney:body(HConnect);
        Err ->
            {error, {could_not_pull, Err}}
    end;
pull(#{transport := basic} = Options, Msg) ->
    basic_auth_request(Options, Msg).

%%%=============================================================================
%% Internal functions
%%%=============================================================================
basic_auth_request(#{connect := HConnect} = Options, Msg) ->
  	UserName = binary_to_list(maps:get(user, Options)),
  	Password = binary_to_list(maps:get(password, Options)),
  	BasicAuth = "Basic " ++ base64:encode(UserName ++ ":" ++ Password),
  	Headers = [
      {<<"Content-Type">>, ?APPLICATION_TYPE},
  		{<<"Authorization">>, list_to_binary(BasicAuth)}
  	],
  	Req = {post, <<"/wsman">>, Headers, Msg},
  	%TODO hackney basic_auth doesn't work?
  %	AuthOptions = [{basic_auth,
  %		{maps:get(user, Options), maps:get(password, Options)}}
  %	],
  	AuthOptions = [],
  	%TODO handle errors always a good idea
  	{ok, _, _, _} = hackney:send_request(HConnect, Req, AuthOptions),
  	hackney:body(HConnect).
