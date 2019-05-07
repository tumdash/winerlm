%%%=============================================================================
%%% Library for Windows Remote Management
%%%=============================================================================
-module(winerlm).

%% API exports
-export([
    get_cim_instance/4,
    enumerate_cim_instances/3
]).

-define(ITEMS_PER_PULL, 1).
-define(WSMAN_PORT, 5985).

%%%=============================================================================
%% API functions
%%%=============================================================================
%% Reads specified CIM ClassName with specified Selectors from remote or local
%% CIM-based system i.e.
%%  get_cim_instance(
%%      <<"127.0.0.1">>,
%%      #{domain => <<"MyDomain">>, user => <<"username">>, password => <<"passwd">>},
%%      <<"Win32_Service">>,
%%      #{<<"Name">> => <<"spooler">>}
%%  ) returns {ok, Data} where
%%    Data = #{
%%        <<"AcceptPause">> => false,
%%        ...
%%        <<"ExitCode">> => 0,
%%        ...
%%        <<"SystemCreationClassName">> => <<"Win32_ComputerSystem">>,
%%        ...
%%    }
%%
-spec get_cim_instance(
        ComputerName    :: binary(),
        Options         :: map(),
        ClassName       :: binary(),
        KeyIdentity     :: map()
) ->
    {ok, CIMData :: map()} | {error, Reason :: term()}.
get_cim_instance(ComputerName, InputOptions, ClassName, _KeyIdentity) ->
    DefOptions = load_defaults(InputOptions),
    Uuid = list_to_binary(uuid:to_string(uuid:uuid1())),
    {ok, HConnect} = hackney:connect(hackney_tcp, ComputerName, ?WSMAN_PORT, []),
    Options = DefOptions#{connect => HConnect},
    case process_enumerate(Options, ComputerName, ClassName, Uuid) of
        {enumeration_resp, EnumCtx} ->
            Res = process_pull(Options, ComputerName, ClassName, EnumCtx, Uuid),
            handle_result(Res);
        {failure, Error} ->
            {error, Error}
    end.

process_enumerate(Options, ComputerName, ClassName, Uuid) ->
    EnumMsg = winerlm_xml:enumerate(ComputerName, ClassName, Uuid),
    case winerlm_transport:enumerate(Options, EnumMsg) of
        {ok, EnumResponse} ->
            winerlm_xml:parse_response(EnumResponse);
        Err ->
            Err
    end.

process_pull(Options, ComputerName, ClassName, EnumCtx, Uuid) ->
    PullMsg = winerlm_xml:pull(ComputerName, ClassName, EnumCtx, ?ITEMS_PER_PULL, Uuid),
    case winerlm_transport:pull(Options, PullMsg) of
        {ok, PullResponse} ->
            winerlm_xml:parse_response(PullResponse);
        Err ->
            Err
    end.

handle_result({pull_resp, ItemData}) ->
    {ok, ItemData};
handle_result({failure, Error}) ->
    {error, Error}.

load_defaults(Options) ->
    maps:put(transport, maps:get(transport, Options, basic), Options).

%% Filters specified CIM ClassNames with specified WQL from remote or local
%% CIM-based system i.e.
%%  enumerate_cim_instances(
%%      <<"127.0.0.1">>,
%%      #{domain => <<"MyDomain">>, user => <<"UserName">>, password => <<"passwd">>},
%%      <<"select * FROM Win32_NetworkAdapterConfiguration WHERE IpEnabled = TRUE">>,
%%  ) returns {ok, Data} where
%%    Data = [
%%      #{
%%        <<"DHCPEnabled">> => true,
%%        <<"IpAddress">> => <<"{192.168.1.33, fe80::7dec:0475:11d7:1376}">>
%%        <<"Index">> => 7,
%%        <<"serviceName">> => <<"someApp1">>
%%        ...
%%      },
%%      #{
%%        <<"DHCPEnabled">> => false,
%%        <<"IpAddress">> => <<"{10.0.1.42, 1::8899:1001:11d7:1376}">>
%%        <<"Index">> => 32,
%%        <<"serviceName">> => <<"someApp2">>,
%%        ...
%%      }
%%    ]
%%
-spec enumerate_cim_instances(
        ComputerName    :: binary(),
        Options         :: map(),
        Wql             :: binary()
) ->
    {ok, CIMData :: [map()]} | {error, Reason :: term()}.
enumerate_cim_instances(ComputerName, Options, Wql) ->
    {error, not_implemented}.

%%%=============================================================================
%% Internal functions
%%%=============================================================================
